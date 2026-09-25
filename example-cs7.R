load("cs7.RData")

baseline_descriptives <- DescriptiveTable::DescriptivesTable(
  popdf = covariates_with_weights,
  table_metadata = metadata,
  groupcol = "group",
  output_format = 'raw',
  calculate_asd = output_asd,
  keep_varinfo = TRUE,
  label_lookup = NULL,
  control_types = FALSE,
  missing_vars = NULL,
   missing_flag = NULL,
  round_decimals = 2,
  use_weights = use_weights,
  weighted_stats = weighted_stats,
  output_asd = output_asd,
  asd_per_level = asd_per_level) 
baseline_descriptives

## results before the fix --
## the result table asd_1 returns only the overall asd, not the per level asd. The per level asd is in the $asd table of the result.
# > baseline_descriptives
# $table
#                       var   type    id    cat parentv parent_catv  m_id       V1_CONTROL       V1_EXPOSED        V2_CONTROL        V2_EXPOSED V3_CONTROL V3_EXPOSED                asd_1
#                    <char> <char> <int> <char>  <lgcl>      <lgcl> <int>           <char>           <char>            <char>            <char>     <char>     <char>               <char>
#  1:                 Total    NUM     1      1      NA          NA    NA 1212.51575795084 1209.76700304533              <NA>              <NA>       <NA>       <NA>                     
#  2:         SV_AGE_FATHER   NUM1     2 STATS1      NA          NA     1 33.3963181130157 33.5636197328007  9.08123927075128  9.27845563339165                         0.0182238288076413
#  3:         SV_AGE_FATHER   NUM1     3 STATS2      NA          NA     1               33               33                26                26         41         42   0.0182238288076413
#  4:      SV_CALENDAR_YEAR    CAT     4   2010      NA          NA     2 769.510365386687 781.261674826433   63.463947609816  64.5795159613194       <NA>       <NA>    0.193880285269585
#  5:      SV_CALENDAR_YEAR    CAT     5   2011      NA          NA     2 154.123250498612 118.852129004219  12.7110307217022  9.82438177806422       <NA>       <NA>    0.193880285269585
#  6:      SV_CALENDAR_YEAR    CAT     6   2012      NA          NA     2  114.43433034498  149.54050292992  9.43776025957587  12.3610994971333       <NA>       <NA>    0.193880285269585
#  7:      SV_CALENDAR_YEAR    CAT     7   2013      NA          NA     2 120.071390259377  95.245432521371  9.90266637542904  7.87303937713717       <NA>       <NA>    0.193880285269585
#  8:      SV_CALENDAR_YEAR    CAT     8   2014      NA          NA     2 33.8880046174064 52.8714022752449  2.79485065618259  4.37037893595647       <NA>       <NA>    0.193880285269585
#  9:      SV_CALENDAR_YEAR    CAT     9   2015      NA          NA     2 20.4884168437823 9.66015876620773  1.68974437729435  0.79851399004026       <NA>       <NA>    0.193880285269585
# 10: SV_PSYCHIATRIC_FATHER     TF    10   TRUE      NA          NA     3 26.2506302773328 25.8803818869883  2.16497229872678   2.1392864759776       <NA>       <NA>  0.00177004282114361
# 11:            SV_OBESITY     TF    11   TRUE      NA          NA     4 169.896617756696 170.109718654892  14.0119100838592  14.0613620826719       <NA>       <NA>  0.00142362543600172
# 12:       SV_ASM_PREVIOUS    CAT    12    VPA      NA          NA     5                0 1209.76700304533                 0               100       <NA>       <NA>                  -88
# 13:       SV_ASM_PREVIOUS    CAT    13    LEV      NA          NA     5 1212.51575795084                0               100                 0       <NA>       <NA>                  -88
# 14: SV_TERATOGENIC_FATHER     TF    14   TRUE      NA          NA     6 481.296741288325 480.214936622299  39.6940607272369  39.6948284598159       <NA>       <NA> 1.56915716710175e-05
# 15:      SV_ALCOHOL_ABUSE     TF    15   TRUE      NA          NA     7 5.31933460617274 4.67140544387009 0.438702307272479 0.386140920698848       <NA>       <NA>  0.00820157329275521
# 16:     SV_SMOKING_FATHER     TF    16   TRUE      NA          NA     8 9.17412444573177 8.15930375681033  0.75661898705845 0.674452496742847       <NA>       <NA>  0.00974862986549135
# 17:     SV_AGE_FATHER_CAT    CAT    17  18_27      NA          NA     9 338.770848739163 336.840441581512  27.9395007048557  27.8434145363188       <NA>       <NA>  0.00263091393811205
# 18:     SV_AGE_FATHER_CAT    CAT    18  28_37      NA          NA     9 422.002805494907 420.839477485166  34.8039027722076  34.7868206378412       <NA>       <NA>  0.00263091393811205
# 19:     SV_AGE_FATHER_CAT    CAT    19  38_47      NA          NA     9 385.845904038288 386.059875152847  31.8219290354105  31.9119197482675       <NA>       <NA>  0.00263091393811205
# 20:     SV_AGE_FATHER_CAT    CAT    20  48_55      NA          NA     9  65.896199678487 66.0272088258068  5.43466748752624  5.45784507757258       <NA>       <NA>  0.00263091393811205
#                       var   type    id    cat parentv parent_catv  m_id       V1_CONTROL       V1_EXPOSED        V2_CONTROL        V2_EXPOSED V3_CONTROL V3_EXPOSED                asd_1
#                    <char> <char> <int> <char>  <lgcl>      <lgcl> <int>           <char>           <char>            <char>            <char>     <char>     <char>               <char>

# $asd
#                       var   type    cat                asd_1
#                    <char> <char> <char>               <char>
#  1:                 Total    NUM   <NA>                     
#  2:         SV_AGE_FATHER   NUM1   <NA>   0.0182238288076413
#  3:      SV_CALENDAR_YEAR    CAT   <NA>    0.193880285269585
#  4:      SV_CALENDAR_YEAR    CAT   2010   0.0232456492759945
#  5:      SV_CALENDAR_YEAR    CAT   2011   0.0913878109870616
#  6:      SV_CALENDAR_YEAR    CAT   2012   0.0939107533141052
#  7:      SV_CALENDAR_YEAR    CAT   2013   0.0713684054122507
#  8:      SV_CALENDAR_YEAR    CAT   2014   0.0848474774694385
#  9:      SV_CALENDAR_YEAR    CAT   2015   0.0804687064381755
# 10:      SV_CALENDAR_YEAR    CAT   2016    0.062200312452846
# 11: SV_PSYCHIATRIC_FATHER     TF   <NA>  0.00177004282114361
# 12:            SV_OBESITY     TF   <NA>  0.00142362543600172
# 13:       SV_ASM_PREVIOUS    CAT   <NA>                  -88
# 14: SV_TERATOGENIC_FATHER     TF   <NA> 1.56915716710175e-05
# 15:      SV_ALCOHOL_ABUSE     TF   <NA>  0.00820157329275521
# 16:     SV_SMOKING_FATHER     TF   <NA>  0.00974862986549135
# 17:     SV_AGE_FATHER_CAT    CAT   <NA>  0.00263091393811205
# 18:     SV_AGE_FATHER_CAT    CAT  18_27  0.00214255589845952
# 19:     SV_AGE_FATHER_CAT    CAT  28_37 0.000358626367971809
# 20:     SV_AGE_FATHER_CAT    CAT  38_47  0.00193129591817953
# 21:     SV_AGE_FATHER_CAT    CAT  48_55  0.00102136255617977
#                       var   type    cat                asd_1
#                    <char> <char> <char>               <char>
# >


# ## after the fix, error with `output_format = 'processed'
# [DescriptiveTable]: counts and statistics weighted by 'stabilized_trunc_iptw'
# [DescriptiveTable]: ASD weighted by 'stabilized_trunc_iptw'
# Error in `DescriptiveTable::DescriptivesTable()`:
# ! error in writing per-category ASD values, no match found for category/categories of variable SV_ASM_PREVIOUS
