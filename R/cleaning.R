# cleaning and checking

library(tidyverse)

# 1. Kathmandu baseline---------------------------------------------------------------

path <- "/Volumes/GoogleDrive/My Drive/Nepal SA Study/Data"

dat_ktm <- openxlsx::read.xlsx(file.path(path, "Baseline/original/Baseline_Survey_NSA_Kathmandu_27032022_v01.xlsx"))

# unique(dat_ktm$confident_abt_future)
# table(dat_ktm$confident_abt_future, useNA = "always")
# sapply(dat_ktm[,18:44], table)

dat_ktm <- dat_ktm %>% 
  rename(worried_other_think = worried_.other_think, 
         conclusion_my_perform = conclusion_my_.perform,
         conclusion_abt_me = `_12_conclusion_me`,
         teacher_like_me = teacher_likeme,
         comfortable_who_i_am = comfortable_who_iam,
         not_understand_class = not_.understand_class) %>% 
  mutate(sch_id = ifelse(sch_id == 2, sch_id, 2)) %>% # there is one labeled 27 instead of 2
  mutate(std_name = gsub("(.*)(\\s)$", "\\1", std_name),  # remove white space at the end
         std_name = gsub("([\\w])([\\w]+)", "\\U\\1\\L\\2", std_name, perl = TRUE)) %>% # title case names
  mutate(grade = as.factor(grade)) %>% # change grade from numeric to factor
  rename(st_id = std_id) %>% # should be st_id to match what's in the id
  mutate(st_id = gsub("-", "_", st_id)) %>% # change - to _ in st_id
  mutate(gender = ifelse(st_id == "SCH2_GR9_ST4", 2, gender), # this should be a girl that is not labelled
         gender = ifelse(gender == 2, "Female", "Male")) %>%  # change 1/2 to texts
  mutate(father_edu = case_when(father_edu == 1 ~ "Do not go to school",
                                father_edu == 2 ~ "Basic education (Class 1 to 8)",
                                father_edu == 3 ~ "Secondary education (Class 9 to 12)",
                                father_edu == 4 ~ "Some college education",
                                father_edu == 5 ~ "Bachelor and above",
                                father_edu == 6 ~ "Do not know",
                                father_edu == 7 ~ "Don't want to answer"
                                )) %>% # labels set to original text values
  mutate(mother_edu = case_when(mother_edu == 1 ~ "Do not go to school",
                                mother_edu == 2 ~ "Basic education (Class 1 to 8)",
                                mother_edu == 3 ~ "Secondary education (Class 9 to 12)",
                                mother_edu == 4 ~ "Some college education",
                                mother_edu == 5 ~ "Bachelor and above",
                                mother_edu == 6 ~ "Do not know",
                                mother_edu == 7 ~ "Don't want to answer"
  )) %>% # labels set to original text values
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
                                )) %>% # labels set to original text values
  mutate(father_occ = case_when(mother_occ == 1 ~ "Self-employment (agriculture)",
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
  )) %>% # labels set to original text values
  mutate_at(vars(capable_person:conclusion_abt_me), function(x){ifelse(x == 6, NA_integer_, x)}) %>% # change don't know to NA
  mutate_at(vars(bad_grades:pressure_parent_teacher), function(x){ifelse(x == 5, NA_integer_, x)}) # change don't know to NA
  
# write data
write_csv(dat_ktm, file.path(path, "Baseline/cleaned/Baseline_Kathmandu.csv"))


# 2. Pokhara baseline--------------------------------------------------------------

dat_pokhara <- openxlsx::read.xlsx(file.path(path, "Baseline/original/Baseline_Survey_NSA_Pokhara_27032022_v01.xlsx"))

# unique(dat_pokhara$adult_members)
# table(dat_pokhara$age, useNA = "always")
# sapply(dat_pokhara[,18:44], table) 

dat_pokhara <- dat_pokhara %>%
  rename(worried_other_think = worried_.other_think, 
         conclusion_my_perform = conclusion_my_.perform,
         conclusion_abt_me = `_12_conclusion_me`,
         teacher_like_me = teacher_likeme,
         comfortable_who_i_am = comfortable_who_iam,
         not_understand_class = not_.understand_class) %>% 
  rename(st_id = std_id) %>% # should be st_id to match what's in the id
  mutate(gender = ifelse(gender == 2, "Female", "Male")) %>% # change 1/2 to texts
  mutate(father_edu = case_when(father_edu == 1 ~ "Do not go to school",
                                father_edu == 2 ~ "Basic education (Class 1 to 8)",
                                father_edu == 3 ~ "Secondary education (Class 9 to 12)",
                                father_edu == 4 ~ "Some college education",
                                father_edu == 5 ~ "Bachelor and above",
                                father_edu == 6 ~ "Do not know",
                                father_edu == 7 ~ "Don't want to answer"
  )) %>% # labels set to original text values
  mutate(mother_edu = case_when(mother_edu == 1 ~ "Do not go to school",
                                mother_edu == 2 ~ "Basic education (Class 1 to 8)",
                                mother_edu == 3 ~ "Secondary education (Class 9 to 12)",
                                mother_edu == 4 ~ "Some college education",
                                mother_edu == 5 ~ "Bachelor and above",
                                mother_edu == 6 ~ "Do not know",
                                mother_edu == 7 ~ "Don't want to answer"
  )) %>% # labels set to original text values
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
  )) %>% # labels set to original text values
  mutate(father_occ = case_when(mother_occ == 1 ~ "Self-employment (agriculture)",
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
  )) %>% 
  mutate_at(vars(capable_person:conclusion_abt_me), function(x){ifelse(x == 6, NA_integer_, x)}) %>% # change don't know to NA
  mutate_at(vars(bad_grades:pressure_parent_teacher), function(x){ifelse(x == 5, NA_integer_, x)}) # change don't know to NA

# write data
write_csv(dat_pokhara, file.path(path, "Baseline/cleaned/Baseline_Pokhara.csv"))
