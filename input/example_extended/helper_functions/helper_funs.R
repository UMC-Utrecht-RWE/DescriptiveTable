# Clean label lookup

getLabelLookup <- function(label_lookup, mapping_dictionary) {
  
  # Load dictionary for integer_value:category mapping
  mapping_dictionary <- mapping_dictionary |>
    dplyr::arrange(variable_id, value) |>
    dplyr::rename("VarName" = "variable_id",
                  "integerVal" = "value") |>
    dplyr::filter(!is.na(integerVal))
  
  # Load label_lookup
  label_lookup <- label_lookup |>
    dplyr::select(-VarLabel) |>
    dplyr::rename("integerVal" = "value")
  
  # Merge label_lookup with mapping_dictionary where "get dictionary" appears: get DEAP specific categories
  dap_specific_label_lookup <- label_lookup |>
    dplyr::filter(integerVal == "get dictionary") |>
    dplyr::select(-integerVal) |>
    dplyr::left_join(
      mapping_dictionary,  # Exclude NA values in mapping_dictionary
      by = "VarName"
    ) |>
    dplyr::mutate(
      category = ifelse(is.na(category), integerVal, category)
    ) |>
    dplyr::select(VarName, category, integerVal)  # Keep only necessary columns
  
  label_lookup <- rbind(label_lookup |>
                          dplyr::filter(integerVal != "get dictionary" | is.na(integerVal)), dap_specific_label_lookup)
  
  
  # Drop 'Unknown' pregnancy trimester:
  label_lookup <- label_lookup |>
    tidytable::filter(!(VarName == "SV_PREG_TRIMESTER" & integerVal == 0))
  
  return(label_lookup)
  
}



convert_types <- function(df, metadata) {
  # Identify variables that should be logical (TF type in metadata)
  tf_vars <- metadata$var[metadata$type == "TF"]
  
  # Convert matching columns in df to logical
  for (var in tf_vars) {
    if (var %in% names(df)) {
      df[[var]] <- ifelse(df[[var]] %in% c("0", "1"), as.numeric(df[[var]]), df[[var]])
      df[[var]] <- as.logical(df[[var]])
    }
  }
  
  return(df)
}