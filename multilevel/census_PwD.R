require(openxlsx)


"""  
Census Disability data downloadable from:  https://doi.org/10.5683/SP3/UHEPKL
"""


disability = read_sf("./tract_disability_prop.shp")
disability$geometry = NULL
disability = subset(disability, PRUID %in% c("10","12","13","24","35"))

colnames(disability) = c(
  "CTUID","DGUID","CTNAME","LANDAREA","PRUID","C1_COUNT_T",
  "total_population",'total_psyc_disability_count', 'total_phys_disability_count',
  'child_population', 'child_psyc_disability_count', 'child_phys_disability_count', 
  'elderly_population', 'elderly_psyc_disability_count', 'elderly_phys_disability_count',
  "total_disability_count", 'child_disability_count', 'elderly_disability_count', 
  "tot_perc","psy_perc", "phy_perc", "chd_perc", "chd_psy_perc", "chd_phy_perc", "eld_perc", "eld_psy_perc", "eld_phy_perc"
)

disability = disability[, !(names(disability) %in% c(
    "total_disability_count", 'child_disability_count', 'elderly_disability_count',"tot_perc", "chd_perc", "eld_perc"
    ))]

# disability$tot_perc = disability$total_disability_count / disability$total_population * 100
disability$psy_perc = disability$total_psyc_disability_count / disability$total_population * 100
disability$phy_perc = disability$total_phys_disability_count / disability$total_population * 100

# disability$chd_perc = disability$child_disability_count / disability$child_population * 100
disability$chd_psy_perc = disability$child_psyc_disability_count / disability$child_population * 100
disability$chd_phy_perc = disability$child_phys_disability_count / disability$child_population * 100

# disability$eld_perc = disability$elderly_disability_count / disability$elderly_population * 100
disability$eld_psy_perc = disability$elderly_psyc_disability_count / disability$elderly_population * 100
disability$eld_phy_perc = disability$elderly_phys_disability_count / disability$elderly_population * 100
