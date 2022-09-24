# baseline cleaning and checking

# library(tidyverse)

# 1. Kathmandu baseline---------------------------------------------------------------

# path <- "/Volumes/GoogleDrive/My Drive/Nepal SA Study/Data"

# dat_ktm_b_raw <- openxlsx::read.xlsx(file.path(path, "Baseline/original/Baseline_Survey_NSA_Kathmandu_27032022_v01.xlsx"))

# unique(dat_ktm$siblings)
# table(dat_ktm$siblings, useNA = "always")
# sapply(dat_ktm[,18:44], table)

clean_dat_ktm_b <- function(dat_ktm_b_raw, name_check){
  
dat_ktm <- dat_ktm_b_raw |> 
  rename(worried_other_think = worried_.other_think, 
         conclusion_my_perform = conclusion_my_.perform,
         conclusion_abt_me = `_12_conclusion_me`,
         teacher_like_me = teacher_likeme,
         comfortable_who_i_am = comfortable_who_iam,
         not_understand_class = not_.understand_class) |> 
  mutate(query = ifelse(query == 1, "Yes", "No"),
         understand_clearly = ifelse(understand_clearly == 1, "Yes", "No"),
         consent = ifelse(consent == 1, "Yes", "No")
         ) |> # numeric 1/2 changed to Yes/No
  mutate(sch_id = ifelse(sch_id == 2, sch_id, 2)) |> # there is one labeled 27 instead of 2
  mutate(sch_id = as.factor(sch_id)) |> # change sch_id from numeric to factor
  mutate(std_name = gsub("(.*)(\\s)$", "\\1", std_name),  # remove white space at the end
         std_name = gsub("([\\w])([\\w]+)", "\\U\\1\\L\\2", std_name, perl = TRUE)) |> # title case names
  mutate(grade = as.factor(grade)) |> # change grade from numeric to factor
  rename(st_id = std_id,
         st_name = std_name) |> # should be st_id to match what's in the id
  mutate(st_id = gsub("-", "_", st_id)) |> # change - to _ in st_id
  mutate(gender = ifelse(st_id == "SCH2_GR9_ST4", 2, gender), # this should be a girl that is not labelled
         gender = ifelse(gender == 2, "Female", "Male")) |>  # change 1/2 to texts
  mutate(father_edu = case_when(father_edu == 1 ~ "Do not go to school",
                                father_edu == 2 ~ "Basic education (Class 1 to 8)",
                                father_edu == 3 ~ "Secondary education (Class 9 to 12)",
                                father_edu == 4 ~ "Some college education",
                                father_edu == 5 ~ "Bachelor and above",
                                father_edu == 6 ~ "Do not know",
                                father_edu == 7 ~ "Don't want to answer"
                                )) |> # labels set to original text values
  mutate(father_edu_f = ifelse(father_edu %in% c("Do not know", "Don't want to answer"),
                               NA_character_,
                               father_edu
  )) |> 
  mutate(father_edu_f = factor(father_edu_f, 
                               levels = c("Do not go to school",
                                          "Basic education (Class 1 to 8)",
                                          "Secondary education (Class 9 to 12)",
                                          "Some college education",
                                          "Bachelor and above"
                               ))) |> 
  mutate(mother_edu = case_when(mother_edu == 1 ~ "Do not go to school",
                                mother_edu == 2 ~ "Basic education (Class 1 to 8)",
                                mother_edu == 3 ~ "Secondary education (Class 9 to 12)",
                                mother_edu == 4 ~ "Some college education",
                                mother_edu == 5 ~ "Bachelor and above",
                                mother_edu == 6 ~ "Do not know",
                                mother_edu == 7 ~ "Don't want to answer"
  )) |> # labels set to original text values
  mutate(mother_edu_f = ifelse(mother_edu %in% c("Do not know", "Don't want to answer"),
                               NA_character_,
                               mother_edu
  )) |> 
  mutate(mother_edu_f = factor(mother_edu_f, 
                               levels = c("Do not go to school",
                                          "Basic education (Class 1 to 8)",
                                          "Secondary education (Class 9 to 12)",
                                          "Some college education",
                                          "Bachelor and above",
                                          TRUE ~ NA_character_
                               ))) |> 
  mutate(father_occ = case_when(father_occ == 1 ~ "Self-employment (agriculture)",
                                father_occ == 2 ~ "Self-employment (non-agriculture/business)",
                                father_occ == 3 ~ "Agricultural-based labor",
                                father_occ == 4 ~ "Other labor (Daily wages)",
                                father_occ == 5 ~ "Regular salary based job (government/job)",
                                father_occ == 6 ~ "Not involved in any occupation",
                                father_occ == 7 ~ "Seeking job",
                                father_occ == 8 ~ "Household work",
                                father_occ == 9 ~ "Others (please mention)",
                                father_occ == 10 ~ "Do not want to mention",
                                father_occ == 11 ~ "Do not know",
                                )) |> # labels set to original text values
  mutate(father_occ_f = case_when(
    father_occ %in% c("Do not know", "Do not want to mention") ~ NA_character_,
    father_occ == "Others (please mention)" ~ father_occ_other,
    TRUE ~ father_occ
  )) |> 
  mutate(father_occ_salary = case_when(
    father_occ %in% c("Other labor (Daily wages)", "Regular salary based job (government/job)",
                      "Social service", "Foreign employment", "Social Services") ~ "Salary job",
    father_occ %in% c("Self-employment (agriculture)", "Self-employment (non-agriculture/business)") ~ "Non-salary job",
    TRUE ~ "No job"
  )) |>
  mutate(mother_occ = case_when(mother_occ == 1 ~ "Self-employment (agriculture)",
                                mother_occ == 2 ~ "Self-employment (non-agriculture/business)",
                                mother_occ == 3 ~ "Agricultural-based labor",
                                mother_occ == 4 ~ "Other labor (Daily wages)",
                                mother_occ == 5 ~ "Regular salary based job (government/job)",
                                mother_occ == 6 ~ "Not involved in any occupation",
                                mother_occ == 7 ~ "Seeking job",
                                mother_occ == 8 ~ "Household work",
                                mother_occ == 9 ~ "Others (please mention)",
                                mother_occ == 10 ~ "Do not want to mention",
                                mother_occ == 11 ~ "Do not know",
  )) |> # labels set to original text values
  mutate(mother_occ_f = case_when(
    mother_occ %in% c("Do not know", "Do not want to mention") ~ NA_character_,
    mother_occ == "Others (please mention)" ~ mother_occ_other,
    TRUE ~ mother_occ
  )) |> 
  mutate(mother_occ_salary = case_when(
    mother_occ %in% c("Foreign employment", "Other labor (Daily wages)", 
                      "Regular salary based job (government/job)", "Works as a maid") ~ "Salary job",
    mother_occ %in% c("Agricultural-based labor", "Runs a shop",
                      "Self-employment (agriculture)", "Self-employment (non-agriculture/business)") ~ "Non-salary job",
    TRUE ~ "No job"
  )) |>
  mutate(siblings = case_when(siblings == 27 ~ 2,
                              siblings == 37 ~ 3,
                              TRUE ~ siblings)) |> # mislabelled 27 and 37
  mutate_at(vars(capable_person:conclusion_abt_me), function(x){ifelse(x == 6, NA_integer_, x)}) |> # change don't know to NA
  mutate_at(vars(bad_grades:pressure_parent_teacher), function(x){ifelse(x == 5, NA_integer_, x)}) |> # change don't know to NA
  mutate(student_type = "Deaf", .after = enumerator_name)  # all students are deaf (to match variable name with Baglung data)

# sort data
dat_ktm <- dat_ktm |> 
  mutate(temp_id = as.numeric(gsub("^SCH\\d+_GR\\d+_ST(\\d+)$", "\\1", st_id))) |> 
  arrange(grade, temp_id) |>  # sort by grade and student ID
  select(-temp_id)

# clean student names
dat_ktm <- dat_ktm |> 
  left_join(name_check, by = c("st_id" = "st_id_correct")) |> 
  select(-st_name) |> 
  rename(st_name = st_name_correct) |> 
  select(enumerator_name:st_id, st_name, gender:mother_occ_other, 
         father_edu_f:mother_occ_salary, adult_members:notes)

# add wave tag
names(dat_ktm) <- paste0(names(dat_ktm), "_b")

# write data
# write_csv(dat_ktm, file.path(path, "Baseline/cleaned/Baseline_Kathmandu.csv"))

return(dat_ktm)

}

# 2. Pokhara baseline--------------------------------------------------------------

# dat_pokhara_b_raw <- openxlsx::read.xlsx(file.path(path, "Baseline/original/Baseline_Survey_NSA_Pokhara_27032022_v01.xlsx"))

# unique(dat_pokhara$adult_members)
# table(dat_pokhara$age, useNA = "always")
# sapply(dat_pokhara[,18:44], table) 

clean_dat_pokhara_b <- function(dat_pokhara_b_raw, name_check){
  
dat_pokhara <- dat_pokhara_b_raw |>
  rename(worried_other_think = worried_.other_think, 
         conclusion_my_perform = conclusion_my_.perform,
         conclusion_abt_me = `_12_conclusion_me`,
         teacher_like_me = teacher_likeme,
         comfortable_who_i_am = comfortable_who_iam,
         not_understand_class = not_.understand_class) |> 
  rename(st_id = std_id,
         st_name = std_name) |> # should be st_id to match what's in the id
  mutate(query = ifelse(query == 1, "Yes", "No"),
         understand_clearly = ifelse(understand_clearly == 1, "Yes", "No"),
         consent = ifelse(consent == 1, "Yes", "No")
  ) |> 
  mutate(sch_id = as.factor(sch_id)) |> # change sch_id from numeric to factor
  mutate(grade = as.factor(grade)) |> # change grade from numeric to factor
  mutate(gender = ifelse(gender == 2, "Female", "Male")) |> # change 1/2 to texts
  mutate(father_edu = case_when(father_edu == 1 ~ "Do not go to school",
                                father_edu == 2 ~ "Basic education (Class 1 to 8)",
                                father_edu == 3 ~ "Secondary education (Class 9 to 12)",
                                father_edu == 4 ~ "Some college education",
                                father_edu == 5 ~ "Bachelor and above",
                                father_edu == 6 ~ "Do not know",
                                father_edu == 7 ~ "Don't want to answer"
  )) |> # labels set to original text values
  mutate(father_edu_f = ifelse(father_edu %in% c("Do not know", "Don't want to answer"),
                               NA_character_,
                               father_edu
  )) |> 
  mutate(father_edu_f = factor(father_edu_f, 
                               levels = c("Do not go to school",
                                          "Basic education (Class 1 to 8)",
                                          "Secondary education (Class 9 to 12)",
                                          "Some college education",
                                          "Bachelor and above"
                               ))) |> 
  mutate(mother_edu = case_when(mother_edu == 1 ~ "Do not go to school",
                                mother_edu == 2 ~ "Basic education (Class 1 to 8)",
                                mother_edu == 3 ~ "Secondary education (Class 9 to 12)",
                                mother_edu == 4 ~ "Some college education",
                                mother_edu == 5 ~ "Bachelor and above",
                                mother_edu == 6 ~ "Do not know",
                                mother_edu == 7 ~ "Don't want to answer"
  )) |> # labels set to original text values
  mutate(mother_edu_f = ifelse(mother_edu %in% c("Do not know", "Don't want to answer"),
                               NA_character_,
                               mother_edu
  )) |> 
  mutate(mother_edu_f = factor(mother_edu_f, 
                               levels = c("Do not go to school",
                                          "Basic education (Class 1 to 8)",
                                          "Secondary education (Class 9 to 12)",
                                          "Some college education",
                                          "Bachelor and above",
                                          TRUE ~ NA_character_
                               ))) |> 
  mutate(father_occ = case_when(father_occ == 1 ~ "Self-employment (agriculture)",
                                father_occ == 2 ~ "Self-employment (non-agriculture/business)",
                                father_occ == 3 ~ "Agricultural-based labor",
                                father_occ == 4 ~ "Other labor (Daily wages)",
                                father_occ == 5 ~ "Regular salary based job (government/job)",
                                father_occ == 6 ~ "Not involved in any occupation",
                                father_occ == 7 ~ "Seeking job",
                                father_occ == 8 ~ "Household work",
                                father_occ == 9 ~ "Others (please mention)",
                                father_occ == 10 ~ "Do not want to mention",
                                father_occ == 11 ~ "Do not know",
  )) |> # labels set to original text values
  mutate(father_occ_f = case_when(
    father_occ %in% c("Do not know", "Do not want to mention") ~ NA_character_,
    father_occ == "Others (please mention)" ~ father_occ_other,
    TRUE ~ father_occ
  )) |> 
  mutate(father_occ_salary = case_when(
    father_occ %in% c("Foreign Employment", "Truck driver", "Regular salary based job (government/job)",
                      "Other labor (Daily wages)", "Social work", "Army") ~ "Salary job",
    father_occ %in% c("Self-employment (non-agriculture/business)", "Self-employment (agriculture)",
                      "Agricultural-based labor", "Temple Priests") ~ "Non-salary job",
    TRUE ~ "No job"
  )) |>
  mutate(mother_occ = case_when(mother_occ == 1 ~ "Self-employment (agriculture)",
                                mother_occ == 2 ~ "Self-employment (non-agriculture/business)",
                                mother_occ == 3 ~ "Agricultural-based labor",
                                mother_occ == 4 ~ "Other labor (Daily wages)",
                                mother_occ == 5 ~ "Regular salary based job (government/job)",
                                mother_occ == 6 ~ "Not involved in any occupation",
                                mother_occ == 7 ~ "Seeking job",
                                mother_occ == 8 ~ "Household work",
                                mother_occ == 9 ~ "Others (please mention)",
                                mother_occ == 10 ~ "Do not want to mention",
                                mother_occ == 11 ~ "Do not know",
  )) |> 
  mutate(mother_occ_f = case_when(
    mother_occ %in% c("Do not know", "Do not want to mention") ~ NA_character_,
    mother_occ == "Others (please mention)" ~ mother_occ_other,
    TRUE ~ mother_occ
  )) |> 
  mutate(mother_occ_salary = case_when(
    mother_occ %in% c("Doctor", "Regular salary based job (government/job)", 
                      "Other labor (Daily wages)", "Foreign Employment") ~ "Salary job",
    mother_occ %in% c("Self-employment (agriculture)", "Self-employment (non-agriculture/business)") ~ "Non-salary job",
    TRUE ~ "No job"
  )) |>
  mutate_at(vars(capable_person:conclusion_abt_me), function(x){ifelse(x == 6, NA_integer_, x)}) |> # change don't know to NA
  mutate_at(vars(bad_grades:pressure_parent_teacher), function(x){ifelse(x == 5, NA_integer_, x)}) |> # change don't know to NA
  mutate(student_type = "Deaf", .after = enumerator_name) # all students are deaf (to match variable name with Baglung data)

# sort data
dat_pokhara <- dat_pokhara |> 
  mutate(temp_id = as.numeric(gsub("^SCH\\d+_GR\\d+_ST(\\d+)$", "\\1", st_id))) |> 
  arrange(grade, temp_id) |>  # sort by grade and student ID
  select(-temp_id)

# clean student names
dat_pokhara <- dat_pokhara |> 
  left_join(name_check, by = c("st_id" = "st_id_correct")) |> 
  select(-st_name) |> 
  rename(st_name = st_name_correct) |> 
  select(enumerator_name:st_id, st_name, gender:mother_occ_other, 
         father_edu_f:mother_occ_salary, adult_members:notes)

# add wave tag
names(dat_pokhara) <- paste0(names(dat_pokhara), "_b")

# write data
# write_csv(dat_pokhara, file.path(path, "Baseline/cleaned/Baseline_Pokhara.csv"))

return(dat_pokhara)
}


# 3. Baglung baseline ---------------------------------------------------------

# dat_baglung_b_raw <- openxlsx::read.xlsx(file.path(path, "Baseline/original/Baseline_Survey_NSA_Baglung_31032022_v01.xlsx"))

#unique(dat_baglung$father_edu)
#table(dat_baglung$age, useNA = "always")
#sapply(dat_baglung[,18:44], table) 


clean_dat_baglung_b <- function(dat_baglung_b_raw, name_check){
  
dat_baglung <- dat_baglung_b_raw |>
  rename(notes = Note) |> 
  rename(st_name = std_name) |> 
  mutate(query = ifelse(query == 1, "Yes", "No"),
         understand_clearly = ifelse(understand_clearly == 1, "Yes", "No"),
         consent = ifelse(consent == 1, "Yes", "No")
  ) |> 
  mutate(sch_id = as.factor(sch_id)) |> # change sch_id from numeric to factor
  mutate(student_type = ifelse(student_type == 1, "Deaf", "Hearing")) |> # changed to original texts
  mutate(grade = as.numeric(gsub("Grade ", "", grade))) |> # should be numeric instead of string
  mutate(grade = as.factor(grade)) |> # change grade from numeric to factor
  mutate(gender = ifelse(gender == 2, "Female", "Male")) |> # change 1/2 to texts
  mutate(gender = ifelse(st_id == "SCH1_GR8_ST14", "Female", gender)) |>  # fix this student's gender
  mutate(father_edu = case_when(father_edu == 1 ~ "Do not go to school",
                                father_edu == 2 ~ "Basic education (Class 1 to 8)",
                                father_edu == 3 ~ "Secondary education (Class 9 to 12)",
                                father_edu == 4 ~ "Some college education",
                                father_edu == 5 ~ "Bachelor and above",
                                father_edu == 6 ~ "Do not know",
                                father_edu == 7 ~ "Don't want to answer"
  )) |> # labels set to original text values
  mutate(father_edu_f = ifelse(father_edu %in% c("Do not know", "Don't want to answer"),
                               NA_character_,
                               father_edu
                               )) |> 
  mutate(father_edu_f = factor(father_edu_f, 
                               levels = c("Do not go to school",
                                          "Basic education (Class 1 to 8)",
                                          "Secondary education (Class 9 to 12)",
                                          "Some college education",
                                          "Bachelor and above"
                                          ))) |> 
  mutate(mother_edu = case_when(mother_edu == 1 ~ "Do not go to school",
                                mother_edu == 2 ~ "Basic education (Class 1 to 8)",
                                mother_edu == 3 ~ "Secondary education (Class 9 to 12)",
                                mother_edu == 4 ~ "Some college education",
                                mother_edu == 5 ~ "Bachelor and above",
                                mother_edu == 6 ~ "Do not know",
                                mother_edu == 7 ~ "Don't want to answer"
  )) |> # labels set to original text values
  mutate(mother_edu_f = ifelse(mother_edu %in% c("Do not know", "Don't want to answer"),
                               NA_character_,
                               mother_edu
  )) |> 
  mutate(mother_edu_f = factor(mother_edu_f, 
                               levels = c("Do not go to school",
                                          "Basic education (Class 1 to 8)",
                                          "Secondary education (Class 9 to 12)",
                                          "Some college education",
                                          "Bachelor and above",
                                          TRUE ~ NA_character_
                               ))) |> 
  mutate(father_occ = case_when(father_occ == 1 ~ "Self-employment (agriculture)",
                                father_occ == 2 ~ "Self-employment (non-agriculture/business)",
                                father_occ == 3 ~ "Agricultural-based labor",
                                father_occ == 4 ~ "Other labor (Daily wages)",
                                father_occ == 5 ~ "Regular salary based job (government/job)",
                                father_occ == 6 ~ "Not involved in any occupation",
                                father_occ == 7 ~ "Seeking job",
                                father_occ == 8 ~ "Household work",
                                father_occ == 9 ~ "Others (please mention)",
                                father_occ == 10 ~ "Do not want to mention",
                                father_occ == 11 ~ "Do not know",
  )) |> # labels set to original text values
  mutate(father_occ_f = case_when(
    father_occ %in% c("Do not know", "Do not want to mention") ~ NA_character_,
    father_occ == "Others (please mention)" ~ father_occ_other,
    TRUE ~ father_occ
  )) |> 
  mutate(father_occ_salary = case_when(
    father_occ %in% c("Foreign employment", "Indian Army", "Other labor (Daily wages)",
                      "Regular salary based job (government/job)") ~ "Salary job",
    father_occ %in% c("Agricultural-based labor", "Self-employment (agriculture)", 
                      "Self-employment (non-agriculture/business)") ~ "Non-salary job",
    TRUE ~ "No job"
  )) |> 
  mutate(mother_occ = case_when(mother_occ == 1 ~ "Self-employment (agriculture)",
                                mother_occ == 2 ~ "Self-employment (non-agriculture/business)",
                                mother_occ == 3 ~ "Agricultural-based labor",
                                mother_occ == 4 ~ "Other labor (Daily wages)",
                                mother_occ == 5 ~ "Regular salary based job (government/job)",
                                mother_occ == 6 ~ "Not involved in any occupation",
                                mother_occ == 7 ~ "Seeking job",
                                mother_occ == 8 ~ "Household work",
                                mother_occ == 9 ~ "Others (please mention)",
                                mother_occ == 10 ~ "Do not want to mention",
                                mother_occ == 11 ~ "Do not know",
  )) |> 
  mutate(mother_occ_f = case_when(
    mother_occ %in% c("Do not know", "Do not want to mention") ~ NA_character_,
    mother_occ == "Others (please mention)" ~ mother_occ_other,
    TRUE ~ mother_occ
  )) |> 
  mutate(mother_occ_salary = case_when(
    mother_occ %in% c("Foreign employment", "Other labor (Daily wages)",
                      "Regular salary based job (government/job)") ~ "Salary job",
    mother_occ %in% c("Agricultural-based labor", "Health volunteer", "Self-employment (agriculture)", 
                      "Self-employment (non-agriculture/business)") ~ "Non-salary job",
    TRUE ~ "No job"
  )) |> 
  mutate_at(vars(capable_person:conclusion_abt_me), function(x){ifelse(x == 6, NA_integer_, x)}) |> # change don't know to NA
  mutate_at(vars(bad_grades:pressure_parent_teacher), function(x){ifelse(x == 5, NA_integer_, x)}) # change don't know to NA

# sort data
dat_baglung <- dat_baglung |> 
  mutate(temp_id = as.numeric(gsub("^SCH\\d+_GR\\d+_ST(\\d+)$", "\\1", st_id))) |> 
  arrange(grade, temp_id) |>  # sort by grade and student ID
  select(-temp_id)

# clean student names
dat_baglung <- dat_baglung |> 
  left_join(name_check, by = c("st_id" = "st_id_correct")) |> 
  select(-st_name) |> 
  rename(st_name = st_name_correct) |> 
  select(enumerator_name:st_id, st_name, gender:mother_occ_other, 
         father_edu_f:mother_occ_salary, adult_members:notes)

# add wave tag
names(dat_baglung) <- paste0(names(dat_baglung), "_b")

# write data
# write_csv(dat_baglung, file.path(path, "Baseline/cleaned/Baseline_Baglung.csv"))

return(dat_baglung)
}
