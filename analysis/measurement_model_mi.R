library(lavaan)
library(semTools)
library(dplyr)

dat_s <- dat_fs |> filter(!is.na(treated_int))

# exclude the raw survey items to prevent double-dipping in multiple imputation
dat_s <- dat_s |> select(
  st_id, sch_id, grade, class_id, class_size, gender, age_b, student_type, father_edu_f,
  mother_edu_f, father_occ_salary, mother_occ_salary, adult_members,
  siblings, treated_int, duration_int, 
  int_q1_value_1_int, int_q1_value_2_int, int_q1_value_3_int,
  mc_survey_1_int:mc_survey_4_int,
  capable_person_b:pressure_parent_teacher_b,
  capable_person_e:pressure_parent_teacher_e)

# multiple imputation on data including all students  
dat_mi <- mice::mice(dat_s, m = 5, seed = 1234)




var_f1_b <- c("capable_person_b" , "confident_abt_future_b" , "comfortable_who_i_am_b" , "feel_smart_b" , "respect_lookup_b" , "belong_school_b" , "people_accept_b" , "comfortable_at_school_b") 

var_f2_b <- c("worried_other_think_b" , "worry_abt_dumb_b" , "nervous_worried_b" , "worry_ppl_dislike_b" , "conclusion_other_deaf_b" , "conclusion_my_perform_b" , "conclusion_abt_me_b") 

var_f3_b <- c("bad_grades_b" , "not_understand_class_b" , "not_understand_homework_b" , "bad_class_teacher_b" , "trouble_studying_b" , "pressure_parent_teacher_b") 

# cfa model
model_cfa_f1_b <- '
  f1 =~ capable_person_b + confident_abt_future_b + comfortable_who_i_am_b + feel_smart_b + respect_lookup_b + belong_school_b + people_accept_b + comfortable_at_school_b

# respect_lookup_b ~~         people_accept_b
# comfortable_who_i_am_b ~~ comfortable_at_school_b
'

model_cfa_f2_b <- '
  f2 =~ worried_other_think_b + worry_abt_dumb_b + nervous_worried_b + worry_ppl_dislike_b + conclusion_other_deaf_b + conclusion_my_perform_b + conclusion_abt_me_b

# worried_other_think_b ~~        worry_abt_dumb_b
# conclusion_other_deaf_b ~~ conclusion_my_perform_b
'

model_cfa_f3_b <- '
  f3 =~ bad_grades_b + not_understand_class_b + not_understand_homework_b + bad_class_teacher_b + trouble_studying_b + pressure_parent_teacher_b
'

# cfa with ordinally scaled items
fit_cfa_f1_b <- semTools::cfa.mi(model_cfa_f1_b,
                            data = dat_mi,
                            ordered = var_f1_b,
                            cluster = "class_id")
summary(fit_cfa_f1_b, standardized = "std.nox")
fitMeasures(fit_cfa_f1_b, test = "D2")


fit_cfa_f2_b <- lavaan::cfa(model_cfa_f2_b,
                            data = dat,
                            ordered = var_f2_b,
                            cluster = "class_id")

summary(fit_cfa_f2_b, fit.measures = TRUE, standardized = TRUE)

fit_cfa_f3_b <- lavaan::cfa(model_cfa_f3_b,
                            data = dat,
                            ordered = var_f3_b,
                            cluster = "class_id")

summary(fit_cfa_f3_b, fit.measures = TRUE, standardized = TRUE)