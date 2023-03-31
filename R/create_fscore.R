create_fscore_dat <- function(dat = dat_all_cleaned_combine_coded){
  
  # See measurement_models.Rmd for EFA and CFA processes
  # This function only generates the factor scores from the finalized models that show relatively
  # acceptable evidence of internal consistency and construct validity.

# Baseline CFA ----------------------------------------------------------------
  
  var_f1_b <- c("capable_person_b" , "confident_abt_future_b" , "comfortable_who_i_am_b" , "feel_smart_b" , "respect_lookup_b" , "belong_school_b" , "people_accept_b" , "comfortable_at_school_b") 
  
  var_f2_b <- c("worried_other_think_b" , "worry_abt_dumb_b" , "nervous_worried_b" , "worry_ppl_dislike_b" , "conclusion_other_deaf_b" , "conclusion_my_perform_b" , "conclusion_abt_me_b") 
  
  var_f3_b <- c("bad_grades_b" , "not_understand_class_b" , "not_understand_homework_b" , "bad_class_teacher_b" , "trouble_studying_b" , "pressure_parent_teacher_b") 
  
  # cfa model
  model_cfa_f1_b <- '
  f1 =~ capable_person_b + confident_abt_future_b + comfortable_who_i_am_b + feel_smart_b + respect_lookup_b + belong_school_b + people_accept_b + comfortable_at_school_b

respect_lookup_b ~~         people_accept_b
comfortable_who_i_am_b ~~ comfortable_at_school_b
'
  
  model_cfa_f2_b <- '
  f2 =~ worried_other_think_b + worry_abt_dumb_b + nervous_worried_b + worry_ppl_dislike_b + conclusion_other_deaf_b + conclusion_my_perform_b + conclusion_abt_me_b

worried_other_think_b ~~        worry_abt_dumb_b
conclusion_other_deaf_b ~~ conclusion_my_perform_b
'
  
  model_cfa_f3_b <- '
  f3 =~ bad_grades_b + not_understand_class_b + not_understand_homework_b + bad_class_teacher_b + trouble_studying_b + pressure_parent_teacher_b
'
  
  # cfa with ordinally scaled items
  fit_cfa_f1_b <- lavaan::cfa(model_cfa_f1_b,
                              data = dat,
                              ordered = var_f1_b,
                              cluster = "class_id")
  
  fit_cfa_f2_b <- lavaan::cfa(model_cfa_f2_b,
                              data = dat,
                              ordered = var_f2_b,
                              cluster = "class_id")
  
  fit_cfa_f3_b <- lavaan::cfa(model_cfa_f3_b,
                              data = dat,
                              ordered = var_f3_b,
                              cluster = "class_id")

# Endline CFA -----------------------------------------------------------------

  var_f1_e <- c("capable_person_e" , "confident_abt_future_e" , "comfortable_who_i_am_e" , "feel_smart_e" , "respect_lookup_e" , "belong_school_e" , "people_accept_e" , "comfortable_at_school_e") 
  
  var_f2_e <- c("worried_other_think_e" , "worry_abt_dumb_e" , "nervous_worried_e" , "worry_ppl_dislike_e" , "conclusion_other_deaf_e" , "conclusion_my_perform_e" , "conclusion_abt_me_e") 
  
  var_f3_e <- c("bad_grades_e" , "not_understand_class_e" , "not_understand_homework_e" , "bad_class_teacher_e" , "trouble_studying_e" , "pressure_parent_teacher_e") 
  
  # cfa model
  model_cfa_f1_e <- '
  f1 =~ capable_person_e + confident_abt_future_e + comfortable_who_i_am_e + feel_smart_e + respect_lookup_e + belong_school_e + people_accept_e + comfortable_at_school_e

respect_lookup_e ~~         people_accept_e
comfortable_who_i_am_e ~~ comfortable_at_school_e
'
  
  model_cfa_f2_e <- '
  f2 =~ worried_other_think_e + worry_abt_dumb_e + nervous_worried_e + worry_ppl_dislike_e + conclusion_other_deaf_e + conclusion_my_perform_e + conclusion_abt_me_e

worried_other_think_e ~~        worry_abt_dumb_e
conclusion_other_deaf_e ~~ conclusion_my_perform_e
'
  
  model_cfa_f3_e <- '
  f3 =~ bad_grades_e + not_understand_class_e + not_understand_homework_e + bad_class_teacher_e + trouble_studying_e + pressure_parent_teacher_e
'
  
  # cfa with ordinally scaled items
  fit_cfa_f1_e <- lavaan::cfa(model_cfa_f1_e,
                              data = dat,
                              ordered = var_f1_e,
                              cluster = "class_id")
  
  fit_cfa_f2_e <- lavaan::cfa(model_cfa_f2_e,
                              data = dat,
                              ordered = var_f2_e,
                              cluster = "class_id")

  fit_cfa_f3_e <- lavaan::cfa(model_cfa_f3_e,
                              data = dat,
                              ordered = var_f3_e,
                              cluster = "class_id")
  

# Create factor scores ----------------------------------------------------
  
  # Baseline
  pred_f1_b <- lavaan::lavPredict(fit_cfa_f1_b)
  index_f1_b <- lavaan::inspect(fit_cfa_f1_b, "case.idx")
  
  pred_f2_b <- lavaan::lavPredict(fit_cfa_f2_b)
  index_f2_b <- lavaan::inspect(fit_cfa_f2_b, "case.idx")
  
  pred_f3_b <- lavaan::lavPredict(fit_cfa_f3_b)
  index_f3_b <- lavaan::inspect(fit_cfa_f3_b, "case.idx")
  
  dat[index_f1_b, "fscore_f1_b"] <- pred_f1_b[,1]
  dat[index_f2_b, "fscore_f2_b"] <- pred_f2_b[,1]
  dat[index_f3_b, "fscore_f3_b"] <- pred_f3_b[,1]
  
  
  # Endline
  pred_f1_e <- lavaan::lavPredict(fit_cfa_f1_e)
  index_f1_e <- lavaan::inspect(fit_cfa_f1_e, "case.idx")
  
  pred_f2_e <- lavaan::lavPredict(fit_cfa_f2_e)
  index_f2_e <- lavaan::inspect(fit_cfa_f2_e, "case.idx")
  
  pred_f3_e <- lavaan::lavPredict(fit_cfa_f3_e)
  index_f3_e <- lavaan::inspect(fit_cfa_f3_e, "case.idx")
  
  dat[index_f1_e, "fscore_f1_e"] <- pred_f1_e[,1]
  dat[index_f2_e, "fscore_f2_e"] <- pred_f2_e[,1]
  dat[index_f3_e, "fscore_f3_e"] <- pred_f3_e[,1]
  
  # dat_fscore <- dat |> select(st_id, fscore_f1_b:fscore_f3_e)

  return(dat)

  }