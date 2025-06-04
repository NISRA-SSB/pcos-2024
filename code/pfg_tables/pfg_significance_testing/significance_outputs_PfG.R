# Select All and run this script to produce final output
# Or press Ctrl + Alt + R

# ***currently only includes age and sex ***

library(here)

# Define years to analyse based on current_year value from config ####
data_years <- c(seq(2014, 2016, 2), 2019:current_year)

### assign data_years to global environment ####
assign(
  "data_years",
  data_years)

source(paste0(here(), "/code/pfg_tables/pfg_significance_testing/significance_testing_PfG.R"))

if (!exists(paste0(here(), "/outputs/PfG/significance outputs"))) {
  dir.create(paste0(here(), "/outputs/PfG/significance outputs"))
}

# Create NI Assembly Workbook ####

wb <- createWorkbook()
modifyBaseFont(wb, fontSize = 12, fontName = "Arial")

for (year in data_years) {

## Trust NI Assembly sheet for each year####
  
addWorksheet(wb, paste("Trust NI Assembly",year))

setColWidths(wb, paste("Trust NI Assembly",year),
  cols = 1:ncol(eval(as.name(paste0("assembly_age_stats_",year)))),
  widths = c(47, rep(12, ncol((eval(as.name(paste0("assembly_age_stats_",year))))) - 1))
)

r <- 1

writeData(wb, paste("Trust NI Assembly",year),
  x = c("Trust in NI Assembly (WEIGHTED)",
        "[Note: Qu in 2019 & 2023 was Elected bodies]"),
  startRow = r
)

addStyle(wb, paste("Trust NI Assembly",year),
  style = pt,
  rows = r,
  cols = 1
)

r <- r + 2

### Trust NI Assembly by Sex ####

f_insert_sig_table(
  df = (eval(as.name(paste0("assembly_sex_stats_",year)))),
  sheet = paste("Trust NI Assembly",year),
  title = paste("Trust in NI Assembly - by Sex -", year)
)

### Trust NI Assembly by Age ####

f_insert_sig_table(
  df = (eval(as.name(paste0("assembly_age_stats_",year)))),
  sheet = paste("Trust NI Assembly",year),
  title = paste("Trust in NI Assembly - by Age Group -", year)
)

f_insert_z_table(
  df = (eval(as.name(paste0("assembly_age_z_scores_",year)))),
  sheet = paste("Trust NI Assembly",year),
  title = paste0("Yes - Trust in NI Assembly by Age Group - ", year)
)

f_insert_z_table(
  df = (eval(as.name(paste0("assembly_disagree_age_z_scores_",year)))),
  sheet = paste("Trust NI Assembly",year),
  title = paste0("No - Trust in NI Assembly by Age Group - ", year)
)

f_insert_z_table(
  df = (eval(as.name(paste0("assembly_dont_know_age_z_scores_",year)))),
  sheet = paste("Trust NI Assembly",year),
  title = paste0("Don't know - Trust in NI Assembly by Age Group - ", year)
)

## Trust NI Assembly (exc DK) sheet for each year ####

addWorksheet(wb, paste("TruNIAssemExDK",year))

setColWidths(wb, paste("TruNIAssemExDK",year),
  cols = 1:ncol((eval(as.name(paste0("assembly_age_ex_dk_",year))))),
  widths = c(47, rep(12, ncol((eval(as.name(paste0("assembly_age_ex_dk_",year))))) - 1))
)

r <- 1

writeData(wb, paste("TruNIAssemExDK",year),
  x = c("Trust in NI Assembly (excluding Don't knows) (WEIGHTED)",
        "[Note: Qu in 2019 & 2023 was Elected bodies]"),
  startRow = r
)

addStyle(wb, paste("TruNIAssemExDK",year),
  style = pt,
  rows = r,
  cols = 1
)

r <- r + 3

### Trust NI Assembly (exc DK) by Sex ####

f_insert_sig_table(
  df = (eval(as.name(paste0("assembly_sex_ex_dk_",year)))),
  sheet = paste("TruNIAssemExDK",year),
  title = paste0("Trust in NI Assembly - by Sex - ", year, " (exc DKs)")
)

### Trust NI Assembly (exc DK) by Age ####

f_insert_sig_table(
  df = (eval(as.name(paste0("assembly_age_ex_dk_",year)))),
  sheet = paste("TruNIAssemExDK",year),
  title = paste0("Trust in NI Assembly - by Age Group - ", year, " (exc DKs)")
)

f_insert_z_table(
  df = (eval(as.name(paste0("assembly_age_z_scores_ex_dk_",year)))),
  sheet = paste("TruNIAssemExDK",year),
  title = paste0("Trust in NI Assembly by Age Group - ", year, " (exc DKs)")
)

}
# Save Workbook ####

saveWorkbook(wb,
             paste0(here(), "/outputs/PfG/significance outputs/significance output NI assembly", ".xlsx"),
             overwrite = TRUE
)
openXL(paste0(here(), "/outputs/PfG/significance outputs/significance output NI assembly", ".xlsx"))


# Create media Workbook ####

wb <- createWorkbook()
modifyBaseFont(wb, fontSize = 12, fontName = "Arial")

for (year in data_years) {
  
## Trust in media sheet for each year####

addWorksheet(wb, paste("Trust Media",year))

setColWidths(wb, paste("Trust Media",year),
  cols = 1:ncol((eval(as.name(paste0("media_age_stats_",year))))),
  widths = c(47, rep(12, ncol((eval(as.name(paste0("media_age_stats_",year))))) - 1))
)

r <- 1

writeData(wb, paste("Trust Media",year),
  x = "Trust in the Media (WEIGHTED)",
  startRow = r
)

addStyle(wb, paste("Trust Media",year),
  style = pt,
  rows = r,
  cols = 1
)

r <- r + 2

### Trust in media by Sex ####

f_insert_sig_table(
  df = (eval(as.name(paste0("media_sex_stats_",year)))),
  sheet = paste("Trust Media",year),
  title = paste("Trust in the Media - by Sex -", year)
)

### Trust in media by Age ####

f_insert_sig_table(
  df = (eval(as.name(paste0("media_age_stats_",year)))),
  sheet = paste("Trust Media",year),
  title = paste("Trust in the Media - by Age Group -", year)
)

f_insert_z_table(
  df = (eval(as.name(paste0("media_age_z_scores_",year)))),
  sheet = paste("Trust Media",year),
  title = paste0("Yes - Trust in the Media by Age Group - ", year)
)

f_insert_z_table(
  df = (eval(as.name(paste0("media_disagree_age_z_scores_",year)))),
  sheet = paste("Trust Media",year),
  title = paste0("No - Trust in the Media by Age Group - ", year)
)

f_insert_z_table(
  df = (eval(as.name(paste0("media_dont_know_age_z_scores_",year)))),
  sheet = paste("Trust Media",year),
  title = paste0("Don't know - Trust in the Media by Age Group - ", year)
)


## Trust in media (exc DK) sheet for each year ####

addWorksheet(wb, paste("TruMediaExDK",year))

setColWidths(wb, paste("TruMediaExDK",year),
  cols = 1:ncol((eval(as.name(paste0("media_age_ex_dk_",year))))),
  widths = c(47, rep(12, ncol((eval(as.name(paste0("media_age_ex_dk_",year))))) - 1))
)

r <- 1

writeData(wb, paste("TruMediaExDK",year),
  x = "Trust in the Media (excluding Don't knows) (WEIGHTED)",
  startRow = r
)

addStyle(wb, paste("TruMediaExDK",year),
  style = pt,
  rows = r,
  cols = 1
)

r <- r + 2

### Trust in media (exc DK) by Sex ####

f_insert_sig_table(
  df = (eval(as.name(paste0("media_sex_ex_dk_",year)))),
  sheet = paste("TruMediaExDK",year),
  title = paste0("Trust the Media - by Sex - ", year, " (exc DKs)")
)

### Trust in media (exc DK) by Age ####

f_insert_sig_table(
  df = (eval(as.name(paste0("media_age_ex_dk_",year)))),
  sheet = paste("TruMediaExDK",year),
  title = paste0("Trust the Media - by Age Group - ", year, " (exc DKs)")
)

f_insert_z_table(
  df = (eval(as.name(paste0("media_age_z_scores_ex_dk_",year)))),
  sheet = paste("TruMediaExDK",year),
  title = paste0("Trust the Media by Age Group - ", year, " (exc DKs)")
)
}

# Save Workbook ####

saveWorkbook(wb,
  paste0(here(), "/outputs/PfG/significance outputs/significance output media", ".xlsx"),
  overwrite = TRUE
)

openXL(paste0(here(), "/outputs/PfG/significance outputs/significance output media", ".xlsx"))
