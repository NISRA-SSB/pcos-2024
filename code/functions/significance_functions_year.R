# Year significance functions (currently includes age & sex) ####

#GUIDANCE

# create additional significance functions (for other covariates) as required
# where recoding is required, include the recoding code after 
  # "data_year <- eval((as.name(paste0("data_",year))))"
# use the recoding code in PfG Historic Data Prep

#FUNCTIONS

# Returns data frame containing p and n values of "value1" (and optionally "value2") in "var" across all age groups in agevar
# Default behaviour "dk" includes don't knows.

f_age_stats_year <- function(year, var, value1, value2 = NA, dk = TRUE) {
  
  data_year <- eval((as.name(paste0("data_",year))))
  age_groups <- levels(data_year$AGE2)
  
  age_weight <- if (year %in% 2012:2016) {
    "weight"
  } else if (year == 2020) {
    "W1a"
  } else {
    "W1"
  }
  
  if (dk) {
    age_stats_year <- data.frame(stat = c("% Yes", "% No", "% DK", "Base"))
    
    for (age in age_groups) {
      age_stats_year[[age]] <- c(
        f_return_p_group(data_year, var, value1, "AGE2", age, weight = age_weight) * 100,
        f_return_p_group(data_year, var, value2, "AGE2", age, weight = age_weight) * 100,
        f_return_p_group(data_year, var, "Don't know", "AGE2", age, weight = age_weight) * 100,
        f_return_n_group(data_year[[var]], data_year$AGE2, age)
      )
    }
  } else {
    age_stats_year <- data.frame(stat = c("% Yes", "Base"))
    
    for (age in age_groups) {
      age_stats_year[[age]] <- c(
        f_return_p_group(data_year, var, value1, "AGE2", age, weight = age_weight, dk = FALSE) * 100,
        data_year %>%
          filter(!is.na(.[[var]]) & .[[var]] != "Don't know" & .[[var]] != "Don't Know" & data_year$AGE2 == age) %>%
          nrow()
      )
    }
  }
  
  names(age_stats_year)[names(age_stats_year) == "stat"] <- " "
  
  age_stats_year
}

# Returns data frame comparing significant differences of "value" in "var" across all age groups
# Default behaviour "dk" includes don't knows.

f_age_z_scores_year <- function(year, var, value, dk = TRUE) {
  
  data_year <- eval((as.name(paste0("data_",year))))
  age_groups <- levels(data_year$AGE2)
  
  age_weight <- if (year %in% 2012:2016) {
    "weight"
  } else if (year == 2020) {
    "W1a"
  } else {
    "W1"
  }
  
  age_z_scores_year <- data.frame(age = age_groups)
  
  for (i in 1:length(age_groups)) {
    col <- c()
    for (j in 1:length(age_groups)) {
      if (i > j) {
        if (dk) {
          col[j] <- f_return_z(
            p1 = f_return_p_group(data_year, var, value, "AGE2", age_groups[j], weight = age_weight),
            n1 = f_return_n_group(data_year[[var]], data_year$AGE2, age_groups[j]),
            p2 = f_return_p_group(data_year, var, value, "AGE2", age_groups[i], weight = age_weight),
            n2 = f_return_n_group(data_year[[var]], data_year$AGE2, age_groups[i])
          )
        } else {
          col[j] <- f_return_z(
            p1 = f_return_p_group(data_year, var, value, "AGE2", age_groups[j], weight = age_weight, dk = FALSE),
            n1 = data_year %>%
              filter(!is.na(.[[var]]) & .[[var]] != "Don't know" & .[[var]] != "Don't Know" & AGE2 == age_groups[j]) %>%
              nrow(),
            p2 = f_return_p_group(data_year, var, value, "AGE2", age_groups[i], weight = age_weight, dk = FALSE),
            n2 = data_year %>%
              filter(!is.na(.[[var]]) & .[[var]] != "Don't know" & .[[var]] != "Don't Know" & AGE2 == age_groups[i]) %>%
              nrow()
          )
        }
      } else {
        col[j] <- NA
      }
    }
    age_z_scores_year[[age_groups[i]]] <- col
  }
  
  names(age_z_scores_year)[names(age_z_scores_year) == "age"] <- " "
  
  age_z_scores_year
}

# Returns data frame comparing significant differences of "value" in "var" across sex
# Default behaviour "dk" includes don't knows.

f_sex_stats_year <- function(year, var, value1, value2 = NA, dk = TRUE) {
  
  data_year <- eval((as.name(paste0("data_",year))))
  
  sex_weight <- if (year %in% 2012:2016) {
    "weight"
  } else {
    "W2"
  }
  
  dont_know <- if (year %in% 2014:2016) {
    "Don't Know"
  } else {
    "Don't know"
  }
  
  #recode SEX labels where required
  
  if (year %in% c(2012:2016,2020:2021)) {
    levels(data_year$SEX)[levels(data_year$SEX) == "Male"] <- "M" 
    levels(data_year$SEX)[levels(data_year$SEX) == "Female"] <- "F" 
  }  
  
  #calculate values
  
  if (dk) {
    sex_stats_year <- data.frame(
      stat = c("% Yes", "% No", "% DK","Base"),
      male = c(
        f_return_p_group(data_year, var, value1, "SEX", "M",weight = sex_weight) * 100,
        f_return_p_group(data_year, var, value2, "SEX", "M", weight = sex_weight) * 100,
        f_return_p_group(data_year, var, dont_know, "SEX", "M", weight = sex_weight) * 100,
        f_return_n_group(data_year[[var]], data_year$SEX, "M")
      ),
      female = c(
        f_return_p_group(data_year, var, value1, "SEX", "F", weight = sex_weight) * 100,
        f_return_p_group(data_year, var, value2, "SEX", "F", weight = sex_weight) * 100,
        f_return_p_group(data_year, var, dont_know, "SEX", "F", weight = sex_weight) * 100,
        f_return_n_group(data_year[[var]], data_year$SEX, "F")
      )
      ,
      z = c(
        f_return_z(
          p1 = f_return_p_group(data_year, var, value1, "SEX", "M", weight = sex_weight),
          n1 = f_return_n_group(data_year[[var]], data_year$SEX, "M"),
          p2 = f_return_p_group(data_year, var, value1, "SEX", "F", weight = sex_weight),
          n2 = f_return_n_group(data_year[[var]], data_year$SEX, "F")
        ),
        f_return_z(
          p1 = f_return_p_group(data_year, var, value2, "SEX", "M", weight = sex_weight),
          n1 = f_return_n_group(data_year[[var]], data_year$SEX, "M"),
          p2 = f_return_p_group(data_year, var, value2, "SEX", "F", weight = sex_weight),
          n2 = f_return_n_group(data_year[[var]], data_year$SEX, "F")
        ),
        f_return_z(
          p1 = f_return_p_group(data_year, var, dont_know, "SEX", "M", weight = sex_weight),
          n1 = f_return_n_group(data_year[[var]], data_year$SEX, "M"),
          p2 = f_return_p_group(data_year, var, dont_know, "SEX", "F", weight = sex_weight),
          n2 = f_return_n_group(data_year[[var]], data_year$SEX, "F")
        ),
        NA
      )
    )
  } else {
    sex_stats_year <- data.frame(
      trust = c("% Yes", "Base"),
      male = c(
        f_return_p_group(data_year, var, value1, "SEX", "M", weight = sex_weight, dk = FALSE) * 100,
        data_year %>%
          filter(!is.na(.[[var]]) & .[[var]] != dont_know & SEX == "M") %>%
          nrow()
      ),
      female = c(
        f_return_p_group(data_year, var, value1, "SEX", "F", weight = sex_weight, dk = FALSE) * 100,
        data_year %>%
          filter(!is.na(.[[var]]) & .[[var]] != dont_know & SEX == "F") %>%
          nrow()
      )
    ) %>%
      mutate(Z = case_when(
        trust == "Base" ~ NA,
        TRUE ~ f_return_z(p1 = male / 100, 
                          n1 = male[trust == "Base"],
                          p2 = female / 100,
                          n2 = female[trust == "Base"])
      ))
  }
  
  names(sex_stats_year) <- c(" ", "Male", "Female", "Z Score")
  
  sex_stats_year
}
