# check consistencies across baseline, intervention, and endline
# and then merge all data into one master dataset

merge_dat <- function(dat_b_cleaned, dat_e_cleaned, dat_int_cleaned){
  
# tar_load(names = c(dat_b_cleaned, dat_e_cleaned, dat_int_cleaned, name_check))
  
# 1.  Check baseline against endline --------------------------------------
  
  # variables to check:
  # "student_type_e", "sch_name_e", "sch_id_e", st_name_e", 
  # "grade_e", "st_id_e", "gender_e"   
  
  
  # decision: keep student_type_b
  # temp <- select(dat_b_cleaned, st_id_b, student_type_b) |> 
  #   left_join(select(dat_e_cleaned, st_id_e, student_type_e),
  #             by = c("st_id_b" = "st_id_e"))
  # 
  # prod(na.omit(temp$student_type_b == temp$student_type_e))
  
  # decision: keep sch_name_b
  # temp <- select(dat_b_cleaned, st_id_b, sch_name_b) |> 
  #   left_join(select(dat_e_cleaned, st_id_e, sch_name_e),
  #             by = c("st_id_b" = "st_id_e"))
  # 
  # prod(na.omit(temp$sch_name_b == temp$sch_name_e))
  
  # decision: keep sch_id_b
  # temp <- select(dat_b_cleaned, st_id_b, sch_id_b) |>
  #   left_join(select(dat_e_cleaned, st_id_e, sch_id_e),
  #             by = c("st_id_b" = "st_id_e"))
  # 
  # prod(na.omit(temp$sch_id_b == temp$sch_id_e))
  
  
  # decision: keep grade_b
  # temp <- select(dat_b_cleaned, st_id_b, grade_b) |>
  #   left_join(select(dat_e_cleaned, st_id_e, grade_e),
  #             by = c("st_id_b" = "st_id_e"))
  # 
  # prod(na.omit(temp$grade_b == temp$grade_e))
  
  # decision: keep gender_b
  # temp <- select(dat_b_cleaned, st_id_b, gender_b) |>
  #   left_join(select(dat_e_cleaned, st_id_e, gender_e),
  #             by = c("st_id_b" = "st_id_e"))
  # 
  # temp <- na.omit(temp)
  # temp[temp$gender_b != temp$gender_e,]
  # prod(na.omit(temp$gender_b == temp$gender_e))
  
  
# 2.  Check baseline against intervention --------------------------------------
  
  # variables to check:
  # "sch_id_int", "grade_int", "st_id_int", st_name_int"
  
  # decision: keep student_type_b
  # temp <- select(dat_b_cleaned, st_id_b, sch_id_b) |>
  #   left_join(select(dat_int_cleaned, st_id_int, sch_id_int),
  #             by = c("st_id_b" = "st_id_int"))
  # 
  # prod(na.omit(temp$sch_id_b == temp$sch_id_int))
  
  # decision: keep grade_b
  # temp <- select(dat_b_cleaned, st_id_b, grade_b) |>
  #   left_join(select(dat_int_cleaned, st_id_int, grade_int),
  #             by = c("st_id_b" = "st_id_int"))
  # 
  # prod(na.omit(temp$sch_id_b == temp$sch_id_int))
  
  # decision: keep st_name_b
  # temp <- select(dat_b_cleaned, st_id_b, st_name_b) |>
  #   left_join(select(dat_int_cleaned, st_id_int, st_name_int),
  #             by = c("st_id_b" = "st_id_int"))
  # 
  # prod(na.omit(temp$sch_id_b == temp$sch_id_int))

# 3. merge datasets -------------------------------------------------------------------------

  # full join
  dat_all_cleaned <- dat_b_cleaned |>
    full_join(select(dat_e_cleaned, 
                     -c(student_type_e, sch_name_e, sch_id_e, 
                        grade_e, gender_e)),
              by = c("st_id_b" = "st_id_e")) |> 
    full_join(select(dat_int_cleaned, 
                     -c(sch_id_int, grade_int)),
              by = c("st_id_b" = "st_id_int"))
  
  
  # retain st_name in baseline or intervention when baseline is missing
  dat_all_cleaned <- dat_all_cleaned |> 
    mutate(st_name = ifelse(is.na(st_name_b) == T,
                            ifelse(is.na(st_name_e) == T, st_name_int, st_name_e),
                            st_name_b))
 
  # rename variables that don't change across waves
  dat_all_cleaned <- dat_all_cleaned |> 
    select(-c(st_name_b, st_name_e, st_name_int)) |> 
    rename_with(.cols = c(student_type_b, sch_name_b:gender_b, 
                          father_edu_b:siblings_b),
                .fn = ~ gsub("_b", "", .x))
  
  
  # add sch_name, sch_id, grade for those only with data in intervention
  dat_all_cleaned <- dat_all_cleaned |> 
    mutate(sch_id = gsub("^SCH(\\d+)_GR(\\d+)_ST(\\d+)$", "\\1", st_id),
           sch_id = as.character(sch_id)) |>
    mutate(grade = gsub("^SCH(\\d+)_GR(\\d+)_ST(\\d+)$", "\\2", st_id),
           grade = as.factor(as.numeric(grade))) |> 
    mutate(sch_name = case_when(
      sch_id == "1" ~ "Dhawalagiri Higher School for Deaf, Baglung",
      sch_id == "2" ~ "Central School for Deaf, Kathmandu",
      sch_id == "3" ~ "Shree Sirjana Deaf School, Pokhara"
    )) 
  
  # recreate ids
  dat_all_cleaned <- dat_all_cleaned |> 
    mutate(temp_id = as.numeric(gsub("^SCH(\\d+)_GR(\\d+)_ST(\\d+)$", "\\3", st_id))) |> 
    arrange(sch_id, grade, temp_id) |>  # sort by grade and student ID
    select(-temp_id)
  
  # create class ids for multilevel modeling
  dat_all_cleaned <- dat_all_cleaned |> 
    mutate(temp_id = gsub("^SCH(\\d+)_GR(\\d+)_ST(\\d+)$", "SCH\\1_GR\\2", st_id)) |> 
    mutate(temp_id = factor(temp_id,
                            levels = c("SCH1_GR6","SCH1_GR7","SCH1_GR8",
                                       "SCH1_GR9","SCH1_GR10","SCH1_GR11",
                                       "SCH1_GR12",
                                       "SCH2_GR6","SCH2_GR7","SCH2_GR8",
                                       "SCH2_GR9","SCH2_GR10","SCH2_GR11",
                                       "SCH2_GR12",
                                       "SCH3_GR6","SCH3_GR7","SCH3_GR8",
                                       "SCH3_GR9","SCH3_GR10","SCH3_GR11",
                                       "SCH3_GR12"
                                       ))) |> 
    group_by(temp_id) %>%
    mutate(class_id = cur_group_id()) |> 
    mutate(class_id = as.factor(class_id))
  
  # create class size
  dat_all_cleaned <- dat_all_cleaned |> 
    group_by(class_id) |> 
    mutate(class_size = n())

  # combine intervention conditions
  dat_all_cleaned <- dat_all_cleaned |> 
    mutate(treated_int = ifelse(form_int == "A", 1, 0))
  
  # fill in the mode dates in each classroom for date_b and date_e when students actually have data collected
  dat_all_cleaned <- dat_all_cleaned |> 
    rowwise() |> 
    mutate(n_cc_b = sum(!is.na(c_across(capable_person_b:pressure_parent_teacher_b))),
           n_cc_e = sum(!is.na(c_across(capable_person_e:pressure_parent_teacher_e)))
           ) # number of completed entries in survey items

  dat_all_cleaned <- dat_all_cleaned |>
    mutate(date_b = as.character(date_b),
           date_e = as.character(date_e)) |> 
    group_by(grade) |> 
    mutate(date_b = ifelse(is.na(date_b) == T,
                           ifelse(n_cc_b == 0,
                                  NA,
                                  get.mode(date_b)
                                  ),
                           date_b),
           date_e = ifelse(is.na(date_e) == T,
                           ifelse(n_cc_e == 0,
                                  NA,
                                  get.mode(date_e)
                           ),
                           date_e)
           ) |> 
    ungroup() |> 
    mutate(date_b = ymd(date_b),
           date_e = ymd(date_e))
  
  
  # reorder variables
  dat_all_cleaned <- dat_all_cleaned |> 
    select(sch_name:st_id, class_id, class_size, st_name, gender, age_b, age_e, student_type, 
           father_edu:siblings,
           date_int, treated_int, form_int:notes_int,
           date_b, capable_person_b:notes_b,
           date_e, capable_person_e:notes_e,
           enumerator_name_b, query_b:consent_b, enumerator_name_e:consent_e
           )
  
  # remove the students that did not give consent
  dat_all_cleaned <- dat_all_cleaned |> 
    filter(consent_b == "Yes" | is.na(consent_b) == T) |> 
    filter(consent_e == "Yes" | is.na(consent_e) == T)
  
  return(dat_all_cleaned)
}

# reverse code certain items so that larger values reflect better child development

reverse_code_dat <- function(dat_all_cleaned){
  
  # reverse code
  dat_all_cleaned_reverse_coded <- dat_all_cleaned |> 
    mutate(
      concerned_abt_impression_b = recode(concerned_abt_impression_b,
                                          `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
      concerned_abt_impression_e = recode(concerned_abt_impression_e,
                                          `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
      worried_other_think_b = recode(worried_other_think_b,
                                     `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
      worried_other_think_e = recode(worried_other_think_e,
                                     `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
      feel_outsider_b = recode(feel_outsider_b,
                               `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
      feel_outsider_e = recode(feel_outsider_e,
                               `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
      teacher_like_me_b = recode(teacher_like_me_b,
                                 `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
      teacher_like_me_e = recode(teacher_like_me_e,
                                 `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1)
    )
  
  # create composite average scores (NOT USEFUL FOR NOW)
  # dat_all_cleaned_reverse_coded <- dat_all_cleaned_reverse_coded |> 
  #   mutate(
  #     self_integrity_b = rowMeans(across(capable_person_b:comfortable_who_i_am_b), na.rm = T),
  #     self_integrity_e = rowMeans(across(capable_person_e:comfortable_who_i_am_e), na.rm = T),
  #     self_esteem_b = rowMeans(across(feel_smart_b:worried_other_think_b), na.rm = T),
  #     self_esteem_e = rowMeans(across(feel_smart_e:worried_other_think_e), na.rm = T),
  #     belonging_b = rowMeans(across(belong_school_b:ppl_like_me_b), na.rm = T),
  #     belonging_e = rowMeans(across(belong_school_e:ppl_like_me_e), na.rm = T), 
  #     stereotype_b = rowMeans(across(worry_abt_dumb_b:worry_ppl_dislike_b), na.rm = T),
  #     stereotype_e = rowMeans(across(worry_abt_dumb_e:worry_ppl_dislike_e), na.rm = T),
  #     threat_collective_b = conclusion_other_deaf_b,
  #     threat_collective_e = conclusion_other_deaf_e,
  #     threat_stereotype_b = conclusion_my_perform_b,
  #     threat_stereotype_e = conclusion_my_perform_e,
  #     threat_general_b = conclusion_abt_me_b,
  #     threat_general_e = conclusion_abt_me_e,
  #     academic_stress_b = rowMeans(across(bad_grades_b:pressure_parent_teacher_b), na.rm = T),
  #     academic_stress_e = rowMeans(across(bad_grades_e:pressure_parent_teacher_e), na.rm = T)
  #   )
  
  return(dat_all_cleaned_reverse_coded)
}

# recode 5-point to 3-point

combine_code_dat <- function(dat_all_cleaned_reverse_coded){
  
  dat_all_cleaned_combine_coded <- dat_all_cleaned_reverse_coded |> 
    mutate_at(vars(capable_person_b:pressure_parent_teacher_b,
                   capable_person_e:pressure_parent_teacher_e),
              function(x){recode(x, `1` = 1, `2` = 1, `3` = 2, `4` = 3, `5` = 3)}
    )
  
  return(dat_all_cleaned_combine_coded)
}
  