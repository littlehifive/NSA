# endline cleaning and checking

# library(tidyverse)

# 1. Kathmandu endline---------------------------------------------------------------

# names(dat_ktm)
# unique(dat_ktm$age)
# table(dat_ktm$siblings, useNA = "always")
# sapply(dat_ktm[,12:38], table)

clean_dat_ktm_e <- function(dat_ktm_e_raw){
  
  # dat_ktm <- dat_ktm_e_raw
  
  dat_ktm <- dat_ktm_e_raw %>% 
    rename(notes = Notes, st_name = std_name) %>% 
    mutate(sch_id = as.factor(sch_id)) %>% # change sch_id from numeric to factor
    mutate(st_name = gsub("(.*)(\\s)$", "\\1", st_name),  # remove white space at the end
           st_name = gsub("([\\w])([\\w]+)", "\\U\\1\\L\\2", st_name, perl = TRUE)) %>% # title case names
    mutate(grade = as.numeric(gsub("Grade ", "", grade))) %>% # should be numeric instead of string
    mutate(grade = as.factor(grade)) %>% # change grade from numeric to factor
    mutate(gender = ifelse(gender == 2, "Female", "Male")) %>%  # change 1/2 to texts
    mutate_at(vars(capable_person:conclusion_abt_me), function(x){ifelse(x == 6, NA_integer_, x)}) %>% # change don't know to NA
    mutate_at(vars(bad_grades:pressure_parent_teacher), function(x){ifelse(x == 5, NA_integer_, x)}) # change don't know to NA
  
  # sort data
  dat_ktm <- dat_ktm %>% 
    mutate(temp_id = as.numeric(gsub("^SCH\\d+_GR\\d+_ST(\\d+)$", "\\1", st_id))) %>% 
    arrange(grade, temp_id) %>%  # sort by grade and student ID
    select(-temp_id)
  
  # add wave tag
  names(dat_ktm) <- paste0(names(dat_ktm), "_e")
  
  return(dat_ktm)
}

# 2. Pokhara endline---------------------------------------------------------------

# names(dat_pokhara)
# unique(dat_pokhara$age)
# table(dat_pokhara$siblings, useNA = "always")
# sapply(dat_pokhara[,12:38], table)

clean_dat_pokhara_e <- function(dat_pokhara_e_raw){
  
  # dat_pokhara <- dat_pokhara_e_raw
  
  dat_pokhara <- dat_pokhara_e_raw %>% 
    rename(notes = Notes, st_name = std_name) %>% 
    mutate(enumerator_name = case_when(
      enumerator_name %in% c("Dilmaya Gurung", "Dilmaya gurung") ~ "Dilmaya Gurung",
      enumerator_name %in% c("Archana rokka", "Archanarokka", "Archana. Rokka") ~ "Archana Rokka"
    )) %>% 
    mutate(sch_id = as.factor(sch_id)) %>% # change sch_id from numeric to factor
    mutate(st_name = gsub("(.*)(\\s)$", "\\1", st_name),  # remove white space at the end
           st_name = gsub("([\\w])([\\w]+)", "\\U\\1\\L\\2", st_name, perl = TRUE)) %>% # title case names
    mutate(grade = as.numeric(gsub("Grade ", "", grade))) %>% # should be numeric instead of string
    mutate(grade = as.factor(grade)) %>% # change grade from numeric to factor
    mutate_at(vars(capable_person:conclusion_abt_me), function(x){ifelse(x == 6, NA_integer_, x)}) %>% # change don't know to NA
    mutate_at(vars(bad_grades:pressure_parent_teacher), function(x){ifelse(x == 5, NA_integer_, x)}) # change don't know to NA
  
  # sort data
  dat_pokhara <- dat_pokhara %>% 
    mutate(temp_id = as.numeric(gsub("^SCH\\d+_GR\\d+_ST(\\d+)$", "\\1", st_id))) %>% 
    arrange(grade, temp_id) %>%  # sort by grade and student ID
    select(-temp_id)
  
  # add wave tag
  names(dat_pokhara) <- paste0(names(dat_pokhara), "_e")
  
  return(dat_pokhara)
}

# 3. Baglung endline---------------------------------------------------------------

# names(dat_baglung)
# unique(dat_baglung$age)
# table(dat_baglung$siblings, useNA = "always")
# sapply(dat_baglung[,12:38], table)

clean_dat_baglung_e <- function(dat_baglung_e_raw){
  
  # dat_baglung <- dat_baglung_e_raw
  
  dat_baglung <- dat_baglung_e_raw %>% 
    rename(notes = Notes, st_name = std_name) %>% 
    mutate(enumerator_name = ifelse(enumerator_name == "Mira bista", "Mira Bista", enumerator_name)) %>% 
    mutate(sch_id = as.factor(sch_id)) %>% # change sch_id from numeric to factor
    mutate(st_name = gsub("(.*)(\\s)$", "\\1", st_name),  # remove white space at the end
           st_name = gsub("([\\w])([\\w]+)", "\\U\\1\\L\\2", st_name, perl = TRUE)) %>% # title case names
    mutate(grade = as.numeric(gsub("Grade ", "", grade))) %>% # should be numeric instead of string
    mutate(grade = as.factor(grade)) %>% # change grade from numeric to factor
    mutate_at(vars(capable_person:conclusion_abt_me), function(x){ifelse(x == 6, NA_integer_, x)}) %>% # change don't know to NA
    mutate_at(vars(bad_grades:pressure_parent_teacher), function(x){ifelse(x == 5, NA_integer_, x)}) # change don't know to NA
  
  # sort data
  dat_baglung <- dat_baglung %>% 
    mutate(temp_id = as.numeric(gsub("^SCH\\d+_GR\\d+_ST(\\d+)$", "\\1", st_id))) %>% 
    arrange(grade, temp_id) %>%  # sort by grade and student ID
    select(-temp_id)
  
  # add wave tag
  names(dat_baglung) <- paste0(names(dat_baglung), "_e")
  
  return(dat_baglung)
}