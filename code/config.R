# configuration script

#### INPUTS AND SETTINGS ####

##### SETTINGS #####

# Set Report parameters, name of department, pre release status and logo type

nics_theme <- "dof"
pre_release <- FALSE
bilingual <- TRUE

###### parameter options:
#  nics_theme - can be any of teo, daera, dfc, de, dfe, dof, dfi, doh, doj, bso
#  pre_release - can be TRUE or FALSE
#  bilingual - can be TRUE or FALSE - Sets language for NISRA Logo

# Location of data folder - see sourced script
source(paste0(here::here(), "/code/path_to_data.R"))

data_filename <- "CHS2425_PCOS_FINAL_DATASET_PASSWORDED.sav"
password <- "CHS2425"

ons_filename <- "ONS PCOS Data - 2023.xlsx"

##### YEAR OF DATA & REPORT TITLE  #####
# Specify the current year your data is for.
# Set the report title and subtitle if required
# THESE SHOULD BE UPDATED FOR EACH NEW PUBLICATION

current_year <- 2024
ons_year <- 2023

survey_start_mth <- "September"
survey_end_mth <- "November"

# Set to "TrustNIAssembly2" (name when Assembly is sitting)
# or "TrustElectedRep2" (name when Assembly isn't sitting, eg 2019)
trust_body_var <- "TrustNIAssembly2"
# add current year to list, if an elected rep year
elected_rep_years_text <- "2019 and 2023"

# Weights used for current year for analysis by age, sex and all other co-vars
age_weight <- "W1"
sex_weight <- "W2"
weight <- "W3"

##### HEADER ######
# Select the Statistic type for the report - select from the list of five below

statistic_type <- "os" # options: as  (Accredited Official Statistics),
#          os  (Official Statistics),
#          osd (Official Statistics in Development),
#          mi  (Management Information),
#          rr  (Research Report)

# Set the report publication data and next publication data if required

pub_date <- "3 September 2025"
next_pub_date <- "10 June 2026"

##### CONTACT DETAILS #####
# Set contact details for the report - these details will feed into the contact
# function and appear at the bottom of the report above the footer

header_publisher <- "NISRA Statistical Support Branch "
lead_statistician <- "Norma Broomfield"
header_telephone <- "028 9038 8481"
header_email <- "Norma.Broomfield@nisra.gov.uk"


#### INSTALL PACKAGES  ####
# check for presence of required packages and if necessary,
# install and then load each

library(markdown)
library(broom)
library(rmarkdown)
library(yaml)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(xfun)
library(htmltools)
library(openxlsx)
library(readxl)
library(lubridate)
library(plotly)
library(here)
library(kableExtra)
library(scales)
library(DT)
library(AMR)
library(httpuv)
library(janitor)
library(foreign)
library(fontawesome)
library(readspss)
library(tufte)
library(readODS)
library(data.table)
library(english)
library(svgtools)
library(rsvg)
library(sjPlot)
library(officedown)
library(officer)
library(pkgbuild)
library(purrr)
library(odsconvertr)
library(pagedown)
library(formattable)
library(grid)
library(patchwork)
library(png)

# turn off warning messages
options(warn = -1)


#### DERIVED TEXT ####

statistic_type_text <- case_when(
  statistic_type == "as" ~ "Accredited Official Statistics",
  statistic_type == "os" ~ "Official Statistics",
  statistic_type == "osd" ~ "Official Statistics in Development",
  statistic_type == "mi" ~ "Management Information",
  statistic_type == "rr" ~ "Research Report",
  TRUE ~ "[UNDEFINED statistic_type_text]"
)

#### DERIVE GLOBAL DATES  ####

# convert to lubridate
pub_date <- parse_date_time(pub_date, orders = "dmY")
folder_year <- format(pub_date, "%Y")
folder_month <- format(pub_date, "%m")

# create pub_date in different formats
pub_date_words_dmy <- format(pub_date, "%d %B %Y")
pub_date_words_my <- format(pub_date, "%B-%Y")


#### CONFIGURE FOLDER PATHS FOR DOWNLOAD BUTTONS #####

# create folder for outputs
if (!dir.exists(paste0(here(), "/outputs"))) {
  dir.create(paste0(here(), "/outputs"))
}

if (!dir.exists(paste0(here(), "/outputs/significance outputs"))) {
  dir.create(paste0(here(), "/outputs/significance outputs"))
}

#### NISRA COLOURS AND LOGOS ####
##### LOGOS #####

# NISRA logo
if (bilingual == TRUE) {
  nisra_logo <- encodeURIComponent(paste0(
    here(), "/data/images/",
    "Bilingual white logo.svg"
  ))
} else {
  nisra_logo <- encodeURIComponent(paste0(
    here(), "/data/images/",
    "English only white logo.svg"
  ))
}
nisra_alt <- "NISRA logo"

# Departmental logo
dep_logo <- base64enc::dataURI(file = paste0(
  here(), "/data/images/",
  "dept_logos/logo-white-",
  nics_theme, ".png"
))
dep_alt <- paste0(toupper(nics_theme), " logo")

# Departmental link


# Accredited Official Statistics logo
acc_official_stats <- encodeURIComponent(paste0(
  here(), "/data/images/",
  "Accredited Official Statistics Logo English.svg"
))
nat_alt <- "Accredited Official Statistics logo"


##### COLOURS #####
nisra_blue <- "#3878c5"
nisra_navy <- "#00205b"
nisra_green <- "#CEDC20"
nisra_lightblue <- "#d9e7ff"
nisra_darkgreen <- "#474c00"

ons_blue <- "#12436d"
ons_green <- "#28a197"
ons_red <- "#801650"
ons_orange <- "#f46a25"

#### CALL & LOAD FUNCTIONS SCRIPTS ####
for (file in list.files(path = paste0(here(), "/code/functions"), pattern = "*.R")) {
  source(paste0(here(), "/code/functions/", file))
}

utils::globalVariables(c("new_workbook", ".", "report_final"))
