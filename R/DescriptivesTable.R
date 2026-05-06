#' Title
#'
#' @param popdf input data frame / data.table containing the population of interest
#' @param table_metadata metadata describing the variables, their types and levels, headers etc.
#' @param groupcol   name of column in which group membership can be found, defaults to "group"
#' @param output_format "processed" or "raw"; raw better for debugging
#' @param calculate_asd  option; calculate asd and add to table or not
#' @param keep_varinfo display option; keep additional raw variable information or only labels
#' @param label_lookup optional table of lookup values to recode category labels
#' @param control_types  optional to control resetting of types
#' @param missing_vars optional; check for variables which should be recoded as missing
#' @param missing_flag optional; flag for missing values
#' @param round_decimals defaults to FALSE; otherwise, an integer specifying rounding
#' @param use_weights defaults to FALSE; if weighted asd required, supply name of the column in popdf which defines these weights
#' @param output_asd optional; return a list containing a table and an object specifying the ASDs only
#'
#' @returns
#' @import data.table
#' @import stringr
#' @import stddiff
#'
#' @examples
#' @export
DescriptivesTable <- function(
  popdf,
  table_metadata,
  groupcol = "group",
  output_format = "processed",
  calculate_asd = TRUE, #
  keep_varinfo = TRUE, #
  label_lookup = NULL, #
  control_types = TRUE, #
  missing_vars = NULL, #
  missing_flag = -99, #
  round_decimals = FALSE,
  use_weights = FALSE,
  output_asd = TRUE
) {
  # detected strange behaviour where compute_asd can sometimes re-write column types
  original_types <- sapply(popdf, class)

  group_list <- unique(popdf[[groupcol]])

  if (calculate_asd == TRUE) {
    # compute asd using helper function
    asd_col <- compute_asd(popdf, table_metadata, "1",
      use_weights = use_weights,
      group_names = group_list
    )
    # remove duplicates (can occur when variable appears in table metadata >1)
    asd_col <- asd_col[!duplicated(asd_col), ]
  }

  # re-set types if altered
  if (control_types == TRUE) {
    for (i in (names(original_types))) {
      popdf[, (i) := as(get(i), original_types[i])]
    }
  }

  if (!(groupcol %in% colnames(popdf))) {
    stop("No column name in input matching specified groupcol input, check variable names")
  }
  # extract unique values
  group_list <- unique(popdf[[groupcol]])
  # rename column to group for easier data.table syntax
  colnames(popdf)[colnames(popdf) == groupcol] <- "group"

  # create empty storage table
  table_out <- data.table()

  # ----------------------------------------------------------------
  # ---- Calculate Counts and Required Descriptive Statistics ------
  # ----------------------------------------------------------------

  # loop through exposed and control groups, calculating N for variable categories
  for (x in group_list) {
    # first obtain total N
    inter_tout <- data.table()
    popN_group <- popdf[group %in% x, .N]
    inter_tout <- rbind(inter_tout, list(
      "var" = "Total", "type" = "NUM",
      "parentv" = NA, "parent_catv" = NA,
      "cat" = 1, "V1" = popN_group, "V2" = NA, "V3" = NA,
      "m_id" = NA
    ))

    # loop through variables in the metadata
    for (i in seq(1, nrow(table_metadata))) {
      # grab variable name and type
      var <- table_metadata[i, var]
      type <- table_metadata[i, type]
      m_id <- i

      # parent variable determines the denominator used to calculate the percentage
      # and the "population" from which to calculate counts
      parent <- table_metadata[i, parent]
      # if parent variable supplied, read the parent category value of interest
      parent_cat <- table_metadata[i, parent_cat]
      # if no parent variable, take all members of the exposed/control group
      if (is.na(parent)) {
        popN <- popN_group
      } else {
        # take the n count from that variable calculated previously
        popN <- as.numeric(inter_tout[inter_tout$var == parent & inter_tout$cat == parent_cat, "V1"])
      }

      # create target categories for N calculation
      if ("flex" %in% table_metadata[i, expectedCat]) {
        # get unique values in the entire data frame (note, not just that group)
        expectedCat <- sort(unique(popdf[, get(var)]))
      } else if (!is.na(str_detect(table_metadata[i, expectedCat], "get")) && str_detect(table_metadata[i, expectedCat], "get") == TRUE) {
        var_categories <- unlist(str_split(table_metadata[i, expectedCat], " "))[2]
        expectedCat <- eval(parse(text = var_categories))
      } else {
        expectedCat <- table_metadata[i, expectedCat]
        if (!is.na(expectedCat)) {
          if (any(str_detect(string = expectedCat, pattern = ","))) {
            expectedCat <- str_remove(unlist(str_split(expectedCat, ",")), " ")
          }
        }
      }

      # grab relevant data from the population-wide data frame
      if (is.na(parent)) {
        data <- popdf[group %in% x, ..var]
      } else {
        data <- popdf[get(parent) == parent_cat & group %in% x, ..var]
      }

      res <- get(paste0("count_", type))(data, var, popN, expectedCat)
      res <- cbind(
        var = rep(var, nrow(res)), type = rep(type, nrow(res)),
        parentv = rep(parent, nrow(res)), parent_catv = rep(parent_cat, nrow(res)),
        res,
        m_id = rep(m_id, nrow(res))
      )

      # colnames(res) <- c('var','type','parentv','parent_catv','cat','m_id','V1','V2','V3')
      colnames(res) <- c("var", "type", "parentv", "parent_catv", "cat", "V1", "V2", "V3", "m_id")

      # here; insert otherwise-empty header rows based on label entry + variable type
      # or do this later when "back-translating" useing scores or dictionery

      inter_tout <- rbindlist(list(inter_tout, res), use.names = TRUE)
    }
    # create an ID column following order of the meta-data (for reshaping later)
    inter_tout <- inter_tout[, id := .I]
    table_out <- rbind(table_out, inter_tout[, strata := x])
    # log_print(paste0('[Table Descriptives]: Values calculated for: ', x))
  }

  # reshape table object from long to wide
  dcaste_tout_1 <- dcast(table_out, id + var + type + cat + parentv + parent_catv + m_id ~ strata, value.var = c("V1", "V2", "V3"))

  # --------------  add ASD column -----------
  # add top row of the asd column
  if (isTRUE(calculate_asd)) {
    asd_toprow <- list("var" = "Total", "type" = "NUM", "asd_1" = "")
    names(asd_toprow) <- colnames(asd_col)
    asd_col <- rbind(asd_toprow, asd_col)

    # merge group descriptives table with asd informaiton
    dcaste_tout_1 <- merge(dcaste_tout_1, asd_col, by = c("var", "type"), all.x = TRUE)
  }
  # re-order rows to desired order specified by id column
  setorderv(dcaste_tout_1, cols = "id")

  # create copy of the output table
  output_df <- dcaste_tout_1


  # -----------------------------------------------------------------
  # - Tidy Appearance of the Table: Adding headers, creating labels -
  # -----------------------------------------------------------------

  # start if statement for processed output
  if (output_format == "processed") {
    numinfo <- c(colnames(output_df)[grepl("^V", colnames(output_df))])
    # set types of columns
    for (i in numinfo) {
      output_df[, (i) := as(get(i), "numeric")]
    }

    if (isTRUE(calculate_asd)) {
      numinfo <- c(numinfo, "asd_1")
    }
    # ----- process missing variables ------
    # if concepts are missing from the DAP, re-fill with -99
    if (!is.null(missing_vars)) {
      if (any(missing_vars %in% output_df$var)) {
        for (i in 1:length(numinfo)) {
          col <- numinfo[i]
          output_df[var %in% missing_vars, (col) := -99]
        }
      }
    }

    # check if label_lookup supplied
    # if(is.null(label_lookup)){
    #   warning("for processed output, please supply a lookup table for any categorical variables, including VarName, category, integerVal")
    # }

    # create a new empty label column
    output_df$label <- vector(mode = "character", length = nrow(output_df))
    output_df[1, "label"] <- "Total"

    # loop through meta-data file, add headers or create labels as desired

    for (i in 1:nrow(table_metadata)) {
      var <- table_metadata[i, "var"]
      type <- table_metadata[i, "type"]
      parent <- table_metadata[i, "parent"]
      parent_cat <- as.logical(table_metadata[i, "parent_cat"])
      label <- table_metadata[i, "label"]
      empty_header <- table_metadata[i, "header"]

      # identify rows of output_df related to the variable specified in the meta-data
      rids <- which(output_df$m_id == i)
      start <- min(rids)
      end <- max(rids)

      # if optional header supplied (not parent category, but just label for these variables)
      # insert this empty header, then continue with rest of the process
      if (!is.na(empty_header)) {
        newrow <- matrix(NA, 1, ncol = ncol(output_df))
        colnames(newrow) <- colnames(output_df)
        newrow[, "label"] <- as.character(empty_header)
        output_df <- rbind(output_df[1:(start - 1), ], newrow, output_df[start:nrow(output_df), ])

        # overwrite start and end points
        start <- start + 1
        end <- end + 1
      }

      # --- Transform output representation according to variable types -----

      # special case; if Categorical variable with only one row/category specified, treat as TF
      if (type == "CAT" & start == end) {
        type <- "TF"
      }

      # if categorical or numeric, insert a new "header" row
      if (type == "CAT" | type == "NUM1" | type == "NUM2") {
        # create empty row for new header
        newrow <- matrix(NA, 1, ncol = ncol(output_df))
        colnames(newrow) <- colnames(output_df)

        # put header in new row
        newrow[, "label"] <- as.character(label)

        # re-write ASD column to the header row, NA otherwise
        if (isTRUE(calculate_asd)) {
          if (length(unique(output_df[start:end, "asd_1"])) != 1) {
            stop(paste0("error in writing ASD values to labels, check categorical setting variable ", var))
          }
          newrow[, "asd_1"] <- unique(output_df[start:end, "asd_1"])
          output_df[start:end, "asd_1"] <- NA
        }
        # insert header row into the table
        output_df <- rbind(output_df[1:(start - 1), ], newrow, output_df[start:nrow(output_df), ])


        # for categorical variables, write category labels for the rest of the rows
        if (type == "CAT") {
          # re-write category label strings if available in dictionary or scores files
          if (var %in% label_lookup$VarName) {
            rtab <- label_lookup[label_lookup$VarName %in% var, ]
            output_df[(start + 1):(end + 1), "label"] <- levels(factor(output_df[(start + 1):(end + 1), "cat"],
              levels = rtab$integerVal,
              labels = rtab$category
            ))
          } else {
            # write category labels into the label column
            output_df[(start + 1):(end + 1), "label"] <- output_df[(start + 1):(end + 1), "cat"]
          }
        } # end type==CAT if

        # for numeric variables, re-write row labels with standard format
        if (type == "NUM1") {
          output_df[(start + 1):(end + 1), "label"] <- c("Mean (SD)", "Median (Q1, Q3)")
        }
        if (type == "NUM2") {
          output_df[(start + 1):(end + 1), "label"] <- c("Median (Q1, Q3)", "Min, Max")
        }
      } # end if CAT,NUM1,NUM2
      # if variable type is TF, no extra header column needed
      if (type == "TF") {
        output_df[start, "label"] <- label
        # write category labels into the label column
        output_df[(start + 1):(end + 1), "label"] <- output_df[(start + 1):(end + 1), "cat"]
      }
    } # end for loop through table


    # ------ Additional appearance processing ------

    # tidy up table appearance according to options
    # vector with variable indicator informaiton not in label
    varinfo <- c("var", "type", "id", "cat", "parentv", "parent_catv", "m_id")
    # vector with names of numeric output columns
    numinfo <- names(output_df)[!names(output_df) %in% c("label", varinfo)]
    # ensure numinfo columns are numeric type
    output_df[, (numinfo) := lapply(.SD, function(col) {
      if (is.logical(col)) {
        as.numeric(col)
      } else {
        col
      }
    }), .SDcols = numinfo]

    # move "raw" information to end of table
    column_order <- c(
      "label",
      names(output_df)[!names(output_df) %in% c("label", varinfo)],
      varinfo
    )
    setcolorder(output_df, column_order)


    # drop "raw" variable information
    if (keep_varinfo == FALSE) {
      output_df[, (varinfo) := NULL]
    }

    # apply rounding
    output_df[, (numinfo) := lapply(.SD, as.numeric), .SDcols = numinfo]
    if (!isFALSE(round_decimals)) {
      output_df[, (numinfo) := lapply(.SD, function(x) round(x, round_decimals)), .SDcols = numinfo]
    }
  } # end if statement for processed output
  if (isFALSE(output_asd)) {
    return(output_df)
  } else {
    return(list(table = output_df, asd = asd_col))
  }
} # end of function
