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
                     -c(student_type_e, sch_name_e, sch_id_e, st_name_e, 
                        grade_e, gender_e)),
              by = c("st_id_b" = "st_id_e")) |> 
    full_join(select(dat_int_cleaned, 
                     -c(sch_id_int, grade_int, st_name_int)),
              by = c("st_id_b" = "st_id_int"))
 
  # rename variables that don't change across waves
  dat_all_cleaned <- dat_all_cleaned |> 
    rename_with(.cols = c(student_type_b, sch_name_b:gender_b, 
                          father_edu_b:siblings_b),
                .fn = ~ gsub("_b", "", .x))
  
  # reorder variables
  dat_all_cleaned <- dat_all_cleaned |> 
    select(sch_name:gender, age_b, age_e, student_type, 
           father_edu:siblings,
           form_int:notes_int,
           capable_person_b:notes_b,
           capable_person_e:notes_e,
           enumerator_name_b, query_b:consent_b, enumerator_name_e:consent_e
           )
  
  return(dat_all_cleaned)
}

