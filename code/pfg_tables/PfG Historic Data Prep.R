# This code creates or renames the relevant S75 derived variables within the historic R data files
# This code DOES NOT SAVE the new dataset to the drive - uncomment the code at the end if you wish to do so

# The variable names used are 
    # LGD2014name (only present 2020 onwards)
    # URBH (only present 2020 onwards)
    # AGE2 (available all years)
    # SEX (available all years) 
    # MS & MS_GRP (available all years, but response options in source variable differ in 2014, 2016 & 2019)
    # OwnRelig2 (only present 2019 onwards; Religon variable in 2014 & 2016 combines 'None' and 'Refusal/Missing' rather than 'None' & 'Other')
    # LimLongStand (not available in 2020)
    # ETHNIC & Ethnic_group (only present 2020 onwards)

# These are used in PfG Table Output, PfG Supplementary Tables and PfG Significance testing

# The categories are:
  # LGD2014name = ("Antrim and Newtownabbey", 
                # "Armagh, Banbridge and Craigavon",
                # "Belfast",
                # "Causeway Coast and Glens",
                # "Derry and Strabane",
                # "Fermanagh and Omagh",
                # "Lisburn and Castlereagh",
                # "Mid and East Antrim",
                # "Mid Ulster",
                # "Newry, Mourne and Down",
                # "North Down and Ards")
  # URBH = (URBAN, RURAL)
  # AGE2 = (16-24, 25-34, 35-44, 45-54, 55-64, 65-74, 75 and over)
  # SEX = (M, F)
  # MS = (Single, Married/CP, Separated, Divorced/Dissolved CP, Widowed/Surviving CP)
  # MS_GRP = (Single, Married/CP, Other)
  # OwnRelig2 = (Catholic, Protestant, Other/No Religion, Refusal, Dont know)
  # LimLongStand = ("No Limiting longstanding illness","Limiting longstanding illness" )
  # ETHNIC = ("White", "Irish Traveller",
              #"White and Black Caribbean", "White and Black African","White and Asian","other Mixed/Multiple background",
              #"Indian", "Pakistani", "Bangladeshi","Chinese","Any other Asian background",
              #"African", "Caribbean","Any other Black/African/Carribean background", 
              #"Arab", "Any other ethnic group"
  #Ethnic_group = ("White", "Irish Traveller", "Mixed/Multiple ethnic groups", "Asian/Asian British", "Black/African/Caribbean/Black British", "Other ethnic group")

# Currently, the following variables are provided with the correct labels:
  #SEX, OwnRelig2, LimLongStand, ETHNIC

# In the 'Data Prep' code, for 2022 onwards:
  # TrustAssemblyElectedBody2 set to relevant variable
  # AGE2 calculated from AGE

# In the 'Historic Data to R' code, the following prep (relevant to S75/PfG) is carried out for 2012 to 2021:
  # TrustAssemblyElectedBody2 = TrustNIAssembly2 (2014,2016,2020,2021) or TrustElectedRep2 (2019)
  # SEX = persex (2014,2016)
  # AGE2 = pcosage (2014,2016)
  # OwnRelig2 = OwnRelig (2021,2020) with labels tidied up in 2020
  # OwnRelig2 = Religion2 (2019), with "Refusal" = "Unwilling to answer" and "Dont Know" = "Undefined"; other labels tidied up
  # URBH labels changed to "URBAN" and "RURAL" (2020,2021); not present (2014, 2016, 2019)

#START OF CODE

library(here)
source(paste0(here(), "/code/config.R"))

# Check for existence of pre 2021 data ####

if (!file.exists(paste0(data_folder, "Final/PCOS 2021 Final Dataset.RDS"))) {
  source(paste0(here(), "/code/pfg_tables/Historic Data to R.R"))
}

# Check for existence of previous year data (post 2021) ####

for (year in c(2022:current_year)) {

  if (!file.exists(paste0(data_folder, "Final/PCOS ", year, " Final Dataset.RDS"))) {
  print(paste0("Final Dataset RDS file missing for ", current_year - 1),
        ". Re-visit the Project for ", year, " and re-run that year's data_prep.R script")}
}

# For each year... ####

for (year in c(seq(2014, 2016, 2), 2019:current_year)) {

# Read in data file ####
  
  data_year <- readRDS(paste0(data_folder, "Final/PCOS ", year, " Final Dataset.RDS"))
  
## RECODE ####

### LGD2014 ####
# recode where present in data (not in 2014, 2016 or 2019)
  
  if ("LGD2014" %in% names(data_year)) {
    
    data_year <- data_year %>%
      mutate(
        LGD2014name = as.factor(case_when(
          LGD2014 == "N09000001" ~ "Antrim and Newtownabbey",
          LGD2014 == "N09000002" ~ "Armagh, Banbridge and Craigavon",
          LGD2014 == "N09000003" ~ "Belfast",
          LGD2014 == "N09000004" ~ "Causeway Coast and Glens",
          LGD2014 == "N09000005" ~ "Derry and Strabane",
          LGD2014 == "N09000006" ~ "Fermanagh and Omagh",
          LGD2014 == "N09000007" ~ "Lisburn and Castlereagh",
          LGD2014 == "N09000008" ~ "Mid and East Antrim",
          LGD2014 == "N09000009" ~ "Mid Ulster",
          LGD2014 == "N09000010" ~ "Newry, Mourne and Down",
          LGD2014 == "N09000011" ~ "North Down and Ards"))
    )
  }
  
### Urban Rural (URBH) ####
# convert to factor

if ("URBH" %in% names(data_year)) {
    data_year <- data_year %>%
    mutate(
           URBH = factor(URBH,
              levels = c("URBAN", "RURAL"),
              labels = c("URBAN", "RURAL"))
    )
}

### Marital/Civil Partnership status (MARSTT) ####
# recode
  
  if ("MARSTT" %in% names(data_year) & year != 2019) {

    data_year <- data_year %>%
      mutate(  
        MS = as.factor(case_when(
          MARSTT %in% c("Single, that is, never married and never registered in a civil partnership") ~ "Single",
          MARSTT %in% c("Married", "In a registered civil partnership") ~ "Married/CP",
          MARSTT %in% c("Seperated, but still legally married", "Seperated, but still legally in a civil partnership") ~ "Separated",
          MARSTT %in% c("Divorced", "Formerly in a civil partnership which is now legally dissolved") ~ "Divorced/Dissolved CP",
          MARSTT %in% c("Widowed", "Surviving partner from a civil partnership") ~ "Widowed/Surviving CP")
         ),
        MS = factor(MS,levels=c("Single", "Married/CP", "Separated", "Divorced/Dissolved CP","Widowed/Surviving CP")),
        MS_GRP = as.factor(case_when(
          MARSTT %in% c("Single, that is, never married and never registered in a civil partnership") ~ "Single",
          MARSTT %in% c("Married", "In a registered civil partnership") ~ "Married/CP",
          MARSTT %in% c("Seperated, but still legally married", "Seperated, but still legally in a civil partnership", 
                        "Divorced", "Formerly in a civil partnership which is now legally dissolved",
                        "Widowed", "Surviving partner from a civil partnership") ~ "Other")
         ),
        MS_GRP = factor(MS_GRP,levels=c("Single", "Married/CP", "Other" ))
      )
  }
  if (year == 2019) {
      
      data_year <- data_year %>%
        mutate(  
          MS = as.factor(case_when(
            MARSTT %in% c("Single, never married") ~ "Single",
            MARSTT %in% c("Married and living with husband/wife", "In a registered same-sex civil partnership") ~ "Married/CP",
            MARSTT %in% c("Married and separated from husband/wife", "SPONTANEOUS ONLY Separated, but still legally in a same-sex civil partnership") ~ "Separated",
            MARSTT %in% c("Divorced", "SPONTANEOUS ONLY Formerly in a same-sex civil partnership which is now legally dissolved") ~ "Divorced/Dissolved CP",
            MARSTT %in% c("Widowed?", "SPONTANEOUS ONLY Surviving partner from a same-sex civil partnership") ~ "Widowed/Surviving CP")
          ),
          MS = factor(MS,levels=c("Single", "Married/CP", "Separated", "Divorced/Dissolved CP","Widowed/Surviving CP")),
          MS_GRP = as.factor(case_when(
            MARSTT %in% c("Single, never married") ~ "Single",
            MARSTT %in% c("Married and living with husband/wife", "In a registered same-sex civil partnership") ~ "Married/CP",
            MARSTT %in% c("Married and separated from husband/wife", "SPONTANEOUS ONLY Separated, but still legally in a same-sex civil partnership", 
                          "Divorced", "SPONTANEOUS ONLY Formerly in a same-sex civil partnership which is now legally dissolved",
                          "Widowed?", "SPONTANEOUS ONLY Surviving partner from a same-sex civil partnership") ~ "Other")
          ),
          MS_GRP = factor(MS_GRP,levels=c("Single", "Married/CP", "Other" ))
        )
  }
  if (year %in% c(2014,2016)) {
    
    data_year <- data_year %>%
      mutate(  
        MS = as.factor(case_when(
          marital %in% c("Single, that is never married") ~ "Single",
          marital %in% c("Married and \\nliving with \\nhusband/wife \\n or in a civil \\npartnership", "A civil partner in a legally-recognised Civil Partnership") ~ "Married/CP",
          marital %in% c("Married and separated from husband/wife", "In a legally-recognised Civil Partnership & separated from civil partner") ~ "Separated",
          marital %in% c("Divorced", "Formerly a civil partner, the Civil Partnership now legally dissolved") ~ "Divorced/Dissolved CP",
          marital %in% c("Widowed", "A surviving civil partner: his/her partner having since died") ~ "Widowed/Surviving CP")
        ),
        MS = factor(MS,levels=c("Single", "Married/CP", "Separated", "Divorced/Dissolved CP","Widowed/Surviving CP")),
        MS_GRP = as.factor(case_when(
          marital %in% c("Single, that is never married") ~ "Single",
          marital %in% c("Married and \\nliving with \\nhusband/wife \\n or in a civil \\npartnership", "A civil partner in a legally-recognised Civil Partnership") ~ "Married/CP",
          marital %in% c("Married and separated from husband/wife", "In a legally-recognised Civil Partnership & separated from civil partner", 
                        "Divorced", "Formerly a civil partner, the Civil Partnership now legally dissolved",
                        "Widowed", "A surviving civil partner: his/her partner having since died") ~ "Other")
        ),
        MS_GRP = factor(MS_GRP,levels=c("Single", "Married/CP", "Other" ))
      )
  }

### LimLongStand ####
# needs to be created from component variables (HLONGILL & REDACT) in 2019 & 2021, called DISABIL in 2014 & 2016

if (year %in% c(2019,2021)) {
  
  data_year <- data_year %>%
    mutate(  
      LimLongStand = as.factor(case_when(
        HLONGILL == "Yes" & REDACT %in% c("Yes, a lot", "Yes, a little") ~ "Limiting longstanding illness",
        HLONGILL == "Yes" & REDACT %in% c("Not at all") ~ "No Limiting longstanding illness",
        HLONGILL == "No" ~ "No Limiting longstanding illness")
      )
    )
}

if (year %in% c(2014,2016)) {
  data_year <- data_year %>%
    mutate(  
      LimLongStand = factor(DISABIL,
                            levels = levels(DISABIL),
                            labels = c("Limiting longstanding illness", "No Limiting longstanding illness"))
    )
  }
  
### ETHNIC ####
# recode where present in data

if (year %in% c(2020:current_year)) {
  data_year <- data_year %>%
    mutate(  
      Ethnic_group = as.factor(case_when(
        ETHNIC %in% c("White") ~ "White",
        ETHNIC %in% c("Irish Traveller") ~ "Irish Traveller",
        ETHNIC %in% c("White and Black Caribbean", "White and Black African","White and Asian","other Mixed/Multiple background") ~ "Mixed/Multiple ethnic groups",
        ETHNIC %in% c("Indian", "Pakistani", "Bangladeshi","Chinese","Any other Asian background") ~ "Asian/Asian British",
        ETHNIC %in% c("African", "Caribbean","Any other Black/African/Carribean background") ~ "Black/African/Caribbean/Black British",
        ETHNIC %in% c("Arab", "Any other ethnic group") ~ "Other ethnic group")
      ),
      Ethnic_white_other = as.factor(case_when(
        ETHNIC %in% c("White") ~ "White",
        ETHNIC %in% c("Irish Traveller", 
                      "White and Black Caribbean", "White and Black African","White and Asian","other Mixed/Multiple background",
                      "Indian", "Pakistani", "Bangladeshi","Chinese","Any other Asian background",
                      "African", "Caribbean","Any other Black/African/Carribean background",
                      "Arab", "Any other ethnic group") ~ "not White"))
  )
}

### AGE2 ####
  
# tidy up AGE2 labels for 2014 & 2016
  
  if (year %in% c(2014, 2016)) {
    levels(data_year$AGE2)[levels(data_year$AGE2) == "16 - 24"] <- "16-24" 
    levels(data_year$AGE2)[levels(data_year$AGE2) == "25 - 34"] <- "25-34"
    levels(data_year$AGE2)[levels(data_year$AGE2) == "35 - 44"] <- "35-44"
    levels(data_year$AGE2)[levels(data_year$AGE2) == "45 - 54"] <- "45-54"
    levels(data_year$AGE2)[levels(data_year$AGE2) == "55 - 64"] <- "55-64"
    levels(data_year$AGE2)[levels(data_year$AGE2) == "65 - 74"] <- "65-74"
  }
 
### SEX ####
  
# recode SEX labels for 2014, 2016, 2020 & 2021
  
  if (year %in% c(2012:2016,2020:2021)) {
    levels(data_year$SEX)[levels(data_year$SEX) == "Male"] <- "M" 
    levels(data_year$SEX)[levels(data_year$SEX) == "Female"] <- "F" 
  }  

## Save to relevant 'data_year' ####

assign(paste0("data_",year),data_year)

#If you want to save the dataset with the extra variables to the drive, then uncomment the 'saveRDS' line below.
#WARNING: this will overwrite the previous version

#saveRDS(data_year, paste0(data_folder, "Final/PCOS ",year," Final Dataset.RDS"))

}
