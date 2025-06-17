# This script does not need run. It will be sourced when you run significance_outputs_PfG.R

# Only make updates to this script to prepare new data frames for outputting to the PfG Significance Outputs

library(here)
source(paste0(here(), "/code/config.R"))

for (year in data_years) {

# Assign trust, distrust and don't know labels (NIAssembly & Media) ####
  
trust <- if (year %in% 2014:2016) {
    "Tend to trust/trust a great deal"
} else {
    "Trust a great deal/Tend to trust"
}

distrust <- if (year %in% 2014:2016) {
  "Tend to distrust/distrust greatly"
} else {
  "Tend to distrust/Distrust greatly"
}

dont_know <- if (year %in% 2014:2016) {
  "Don't Know"
} else {
  "Don't know"
}

# Trust NI Assembly ####

## Trust NI Assembly by Age ####

assign(paste0("assembly_age_stats_",year),f_age_stats_year(year,"TrustAssemblyElectedBody2", trust, distrust))

assign(paste0("assembly_age_z_scores_",year),f_age_z_scores_year(year,"TrustAssemblyElectedBody2", trust))

assign(paste0("assembly_disagree_age_z_scores_",year),f_age_z_scores_year(year,"TrustAssemblyElectedBody2", distrust))

assign(paste0("assembly_dont_know_age_z_scores_",year),f_age_z_scores_year(year,"TrustAssemblyElectedBody2", dont_know))

## Trust NI Assembly by Age (exc DK) ####

assign(paste0("assembly_age_ex_dk_",year),f_age_stats_year(year,"TrustAssemblyElectedBody2", trust, dk = FALSE))

assign(paste0("assembly_age_z_scores_ex_dk_",year),f_age_z_scores_year(year,"TrustAssemblyElectedBody2", trust, dk = FALSE))

## Trust NI Assembly by Sex ####

assign(paste0("assembly_sex_stats_",year),f_sex_stats_year(year,"TrustAssemblyElectedBody2", trust, distrust))

## Trust NI Assembly by Sex (exc DK) ####

assign(paste0("assembly_sex_ex_dk_",year),f_sex_stats_year(year,"TrustAssemblyElectedBody2", trust, dk = FALSE))


# Trust the Media ####

## Trust the Media by Age ####

assign(paste0("media_age_stats_",year),f_age_stats_year(year,"TrustMedia2", trust, distrust))

assign(paste0("media_age_z_scores_",year),f_age_z_scores_year(year,"TrustMedia2", trust))

assign(paste0("media_disagree_age_z_scores_",year),f_age_z_scores_year(year,"TrustMedia2", distrust))

assign(paste0("media_dont_know_age_z_scores_",year),f_age_z_scores_year(year,"TrustMedia2", dont_know))

## Trust the Media by Age (exc DK) ####

assign(paste0("media_age_ex_dk_",year),f_age_stats_year(year,"TrustMedia2", trust, dk = FALSE))

assign(paste0("media_age_z_scores_ex_dk_",year),f_age_z_scores_year(year,"TrustMedia2", trust, dk = FALSE))


## Trust the Media by Sex ####

assign(paste0("media_sex_stats_",year),f_sex_stats_year(year,"TrustMedia2", trust, distrust))

## Trust NI Media by Sex (exc DK) ####

assign(paste0("media_sex_ex_dk_",year),f_sex_stats_year(year,"TrustMedia2", trust, dk = FALSE))

}

