# clean spaces in text responses
clean_space <- function(data, var){
  data[,var] <- gsub("\\r", "", data[, var])
  data[,var] <- gsub("\\n", "", data[, var])
  data[,var] <- gsub("^\\s+", "", data[, var])
  data[,var] <- gsub("\\s+$", "", data[, var])
  return(data)
}

# refactor GPA letter grades
refactor_letter_grade <- function(x){
  return(
    ordered(x, levels = c("E", "D", "D+", "C", "C+", "B", "B+", "A", "A+"))
  )
}

# scores to GPA
scores_to_gpa <- function(x) {
  case_when(
    x >= 90 ~ 4,
    x >= 80 ~ 3.6,
    x >= 70 ~ 3.2,
    x >= 60 ~ 2.8,
    x >= 50 ~ 2.4,
    x >= 40 ~ 2,
    x >= 30 ~ 1.6,
    x >= 20 ~ 1.2,
    x >= 0 ~ 0.8,
    TRUE ~ NA_integer_
  )
}

# scores to letter grades
scores_to_letter_grade <- function(x) {
  case_when(
    x >= 90 ~ "A+",
    x >= 80 ~ "A",
    x >= 70 ~ "B+",
    x >= 60 ~ "B",
    x >= 50 ~ "C+",
    x >= 40 ~ "C",
    x >= 30 ~ "D+",
    x >= 20 ~ "D",
    x >= 0 ~ "E",
    TRUE ~ NA_character_
  )
}

# gpa to letter grades
gpa_to_letter_grade <- function(x) {
  case_when(
    x == 4 ~ "A+",
    x == 3.6 ~ "A",
    x == 3.2 ~ "B+",
    x == 2.8 ~ "B",
    x == 2.4 ~ "C+",
    x == 2 ~ "C",
    x == 1.6 ~ "D+",
    x == 1.2 ~ "D",
    x == 0.8 ~ "E",
    TRUE ~ NA_character_
  )
}

# letter grades to gpa
letter_grade_to_gpa <- function(x) {
  case_when(
    x == "A+" ~ 4,
    x == "A" ~ 3.6,
    x == "B+" ~ 3.2,
    x == "B" ~ 2.8,
    x == "C+" ~ 2.4,
    x == "C" ~ 2.0,
    x == "D+" ~ 1.6,
    x == "D" ~ 1.2,
    x == "E" ~ 0.8,
    x == "NG" ~ NA_integer_,
    TRUE ~ NA_integer_
  )
}

# Function to scale 40-point to 100-point
scale_40_to_100 <- function(x) {
  x * 100/40
}

# Function to scale 60-point to 100-point
scale_60_to_100 <- function(x) {
  x * 100/60
}

# create correlation matrix
get.cor <- function(fs_data){
  x <- as.matrix(fs_data)
  R <- Hmisc::rcorr(x)$r
  p <- Hmisc::rcorr(x)$P
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", "")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 3))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  df <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(df) <- paste(diag(R), " ", sep="")
  rownames(df) <- colnames(x)
  colnames(df) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle
  df <- as.matrix(df)
  df[upper.tri(df, diag = TRUE)] <- "--"
  df[upper.tri(df)] <- ""
  df <- as.data.frame(df)
  
  ## remove last column and return the matrix (which is now a data frame)
  df <- cbind(df[1:length(df)])
  
  df[is.na(df)]<- "--"
  names(df) <- 1:nrow(df)
  rownames(df) <- paste(1:nrow(df), ". ", rownames(df), sep = "")
  return(df)
}

# get mode in a vector
get.mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# get ordinal omega reliability coefficients directly from data frame
get.ordinal.omega <- function(data, nfactors = 1){
  data <- psych::polychoric(data)
  return(psych::omega(data$rho, nfactors = nfactors))
}

# get ordinal alpha reliability coefficients directly from data frame
get.ordinal.alpha <- function(data){
  data <- psych::polychoric(data)
  return(psych::alpha(data$rho, check.keys=TRUE))
}

# get scree plot
plot.scree <- function(efa_model) {
  
  # number of factors from EFA   
  n_factors <- length(efa_model$e.values)
  
  # a data frame for eigenvalues
  scree <- data.frame(
    Factor_n =  as.factor(1:n_factors), 
    Eigenvalue = efa_model$e.values)
  
  # scree plot
  ggplot(scree, aes(x = Factor_n, y = Eigenvalue, group = 1)) + 
    geom_point() + 
    geom_line() +
    xlab("Number of factors") +
    ylab("Initial eigenvalue") +
    labs(title = "Scree Plot", 
         subtitle = "(Based on the unreduced correlation matrix)") + 
    theme_bw()
}

# get Cohen's d from posterior draws
get.posterior.treat.d <- function(model, var){

  draws <- as.data.frame(model)
  draws$cohens_d <- draws$b_treated_int / draws$sigma
  return(draws$cohens_d)
  
}

# plot kernel density of a variable
plot.kdensity <- function(var, 
                          value = 0,
                          xlab = "",
                          ylab = "Density",
                          main = "",
                          plot.prior = FALSE,
                          col1 = "firebrick1",
                          col2 = "steelblue1"){
  # https://r-charts.com/distribution/fill-area-density/
  if (plot.prior == T) {
    den <- curve(dnorm(x, 0, 1), from=-4, to=4, 
                 xlab = xlab, ylab = ylab, main = main)
  } else {
    den <- density(var)
    plot(den, xlab = xlab, ylab = ylab, main = main)
  }
  
  polygon(c(den$x[den$x >= value ], value),
          c(den$y[den$x >= value ], 0),
          col = col1,
          border = 1)
  
  polygon(c(den$x[den$x <= value ], value),
          c(den$y[den$x <= value ], 0),
          col = col2,
          border = 1)

}

# store model results as a list (all secondary outcomes)
get.model.results.secondary <- function(data = dat_mi, 
                                        my_prior_pos = prior("normal(0, 1)", class = "b"),
                                        my_prior_neg = prior("normal(0, 1)", class = "b")){
  
  fit_integrity_esteem <- brm_multiple(
    self_integrity_esteem_e ~ self_integrity_esteem_b + treated_int +
      class_size + grade + gender + age_b + student_type + father_edu_f + mother_edu_f +
      father_occ_salary + mother_occ_salary + adult_members + siblings + duration_int +
      (treated_int | class_id),
    data,
    prior = my_prior_pos,
    chains = 4,
    seed = 1234
  )
  
  
  fit_belonging <- brm_multiple(
    belonging_e ~ belonging_b + treated_int +
      class_size + grade + gender + age_b + student_type + father_edu_f + mother_edu_f +
      father_occ_salary + mother_occ_salary + adult_members + siblings + duration_int +
      (treated_int | class_id),
    data,
    prior = my_prior_pos,
    chains = 4,
    seed = 1234
  )
  
  fit_worry <- brm_multiple(
    worry_e ~ worry_b + treated_int +
      class_size + grade + gender + age_b + student_type + father_edu_f + mother_edu_f +
      father_occ_salary + mother_occ_salary + adult_members + siblings + duration_int +
      (treated_int | class_id),
    data,
    prior = my_prior_neg,
    chains = 4,
    seed = 1234
  )
  
  fit_stereotype <- brm_multiple(
    stereotype_e ~ stereotype_b + treated_int +
      class_size + grade + gender + age_b + student_type + father_edu_f + mother_edu_f +
      father_occ_salary + mother_occ_salary + adult_members + siblings + duration_int +
      (treated_int | class_id),
    data,
    prior = my_prior_neg, 
    chains = 4,
    seed = 1234
  )
  
  fit_academic_stress <- brm_multiple(
    academic_stress_e ~ academic_stress_b + treated_int +
      class_size + grade + gender + age_b + student_type + father_edu_f + mother_edu_f +
      father_occ_salary + mother_occ_salary + adult_members + siblings + duration_int +
      (treated_int | class_id),
    data,
    prior = my_prior_neg,
    chains = 4,
    seed = 1234
  )
  
  return(list(fit_integrity_esteem, fit_belonging, fit_worry, fit_stereotype, fit_academic_stress))
}

get.model.results.deaf.secondary <- function(data = dat_mi_deaf, 
                                             my_prior_pos = prior("normal(0, 1)", class = "b"),
                                             my_prior_neg = prior("normal(0, 1)", class = "b")){
  
  fit_integrity_esteem <- brm_multiple(
    self_integrity_esteem_e ~ self_integrity_esteem_b + treated_int +
      class_size + grade + gender + age_b + father_edu_f + mother_edu_f +
      father_occ_salary + mother_occ_salary + adult_members + siblings + duration_int +
      (treated_int | class_id),
    data,
    prior = my_prior_pos,
    chains = 4,
    seed = 1234
  )
  
  
  fit_belonging <- brm_multiple(
    belonging_e ~ belonging_b + treated_int +
      class_size + grade + gender + age_b + father_edu_f + mother_edu_f +
      father_occ_salary + mother_occ_salary + adult_members + siblings + duration_int +
      (treated_int | class_id),
    data,
    prior = my_prior_pos,
    chains = 4,
    seed = 1234
  )
  
  fit_worry <- brm_multiple(
    worry_e ~ worry_b + treated_int +
      class_size + grade + gender + age_b + father_edu_f + mother_edu_f +
      father_occ_salary + mother_occ_salary + adult_members + siblings + duration_int +
      (treated_int | class_id),
    data,
    prior = my_prior_neg,
    chains = 4,
    seed = 1234
  )
  
  fit_stereotype <- brm_multiple(
    stereotype_e ~ stereotype_b + treated_int +
      class_size + grade + gender + age_b + father_edu_f + mother_edu_f +
      father_occ_salary + mother_occ_salary + adult_members + siblings + duration_int +
      (treated_int | class_id),
    data,
    prior = my_prior_neg, 
    chains = 4,
    seed = 1234
  )
  
  fit_academic_stress <- brm_multiple(
    academic_stress_e ~ academic_stress_b + treated_int +
      class_size + grade + gender + age_b + father_edu_f + mother_edu_f +
      father_occ_salary + mother_occ_salary + adult_members + siblings + duration_int +
      (treated_int | class_id),
    data,
    prior = my_prior_neg,
    chains = 4,
    seed = 1234
  )
  
  return(list(fit_integrity_esteem, fit_belonging, fit_worry, fit_stereotype, fit_academic_stress))
}

get.fs.model.results.secondary <- function(data = dat_mi, 
                                        my_prior_pos = prior("normal(0, 1)", class = "b"),
                                        my_prior_neg = prior("normal(0, 1)", class = "b")){
  
  fit_f1 <- brm_multiple(
    fscore_f1_e ~ fscore_f1_b + treated_int +
      class_size + grade + gender + age_b + student_type + father_edu_f + mother_edu_f +
      father_occ_salary + mother_occ_salary + adult_members + siblings + duration_int + sch_id +
      (treated_int | class_id),
    data,
    prior = my_prior_pos, 
    chains = 4,
    seed = 1234
  )
  
  fit_f2 <- brm_multiple(
    fscore_f2_e ~ fscore_f2_b + treated_int +
      class_size + grade + gender + age_b + student_type + father_edu_f + mother_edu_f +
      father_occ_salary + mother_occ_salary + adult_members + siblings + duration_int + sch_id +
      (treated_int | class_id),
    data,
    prior = my_prior_neg, 
    chains = 4,
    seed = 1234
  )
  
  fit_f3 <- brm_multiple(
    fscore_f3_e ~ fscore_f3_b + treated_int +
      class_size + grade + gender + age_b + student_type + father_edu_f + mother_edu_f +
      father_occ_salary + mother_occ_salary + adult_members + siblings + duration_int + sch_id +
      (treated_int | class_id),
    data,
    prior = my_prior_neg, 
    chains = 4,
    seed = 1234
  )
  
  return(list(fit_f1, fit_f2, fit_f3))
}


get.fs.model.results.deaf.secondary <- function(data = dat_mi_deaf, 
                                           my_prior_pos = prior("normal(0, 1)", class = "b"),
                                           my_prior_neg = prior("normal(0, 1)", class = "b")){
  
  fit_f1 <- brm_multiple(
    fscore_f1_e ~ fscore_f1_b + treated_int +
      class_size + grade + gender + age_b + father_edu_f + mother_edu_f +
      father_occ_salary + mother_occ_salary + adult_members + siblings + duration_int + sch_id +
      (treated_int | class_id),
    data,
    prior = my_prior_pos, 
    chains = 4,
    seed = 1234
  )
  
  fit_f2 <- brm_multiple(
    fscore_f2_e ~ fscore_f2_b + treated_int +
      class_size + grade + gender + age_b + father_edu_f + mother_edu_f +
      father_occ_salary + mother_occ_salary + adult_members + siblings + duration_int + sch_id +
      (treated_int | class_id),
    data,
    prior = my_prior_neg, 
    chains = 4,
    seed = 1234
  )
  
  fit_f3 <- brm_multiple(
    fscore_f3_e ~ fscore_f3_b + treated_int +
      class_size + grade + gender + age_b + father_edu_f + mother_edu_f +
      father_occ_salary + mother_occ_salary + adult_members + siblings + duration_int + sch_id +
      (treated_int | class_id),
    data,
    prior = my_prior_neg, 
    chains = 4,
    seed = 1234
  )
  
  return(list(fit_f1, fit_f2, fit_f3))
}


get.model.results.primary <- function(data = dat_s, 
                                      my_prior = prior("normal(0, 1)", class = "b")){
  
  fit_nepali <- brm(
    nepali_gpa_e ~ nepali_gpa_b + treated_int +
      class_size + grade + gender + age_b + 
      father_edu_f + mother_edu_f +
      father_occ_salary + mother_occ_salary + adult_members + siblings + duration_int + sch_id +
      (treated_int | class_id),
    data = data,
    prior = my_prior, 
    chains = 4,
    seed = 1234,
    control = list(adapt_delta = 0.99,
                   max_treedepth = 15)
  )
  
  fit_english <- brm(
    english_gpa_e ~ english_gpa_b + treated_int +
      class_size + grade + gender + age_b + 
      father_edu_f + mother_edu_f +
      father_occ_salary + mother_occ_salary + adult_members + siblings + duration_int + sch_id +
      (treated_int | class_id),
    data = data,
    prior = my_prior, 
    chains = 4,
    seed = 1234,
    control = list(adapt_delta = 0.99,
                   max_treedepth = 15)
  )
  
  fit_math <- brm(
    math_gpa_e ~ math_gpa_b + treated_int +
      class_size + grade + gender + age_b + 
      father_edu_f + mother_edu_f +
      father_occ_salary + mother_occ_salary + adult_members + siblings + duration_int + sch_id +
      (treated_int | class_id),
    data = data,
    prior = my_prior, 
    chains = 4,
    seed = 1234,
    control = list(adapt_delta = 0.99,
                   max_treedepth = 15)
  )
  
  fit_science <- brm(
    science_gpa_e ~ science_gpa_b + treated_int +
      class_size + grade + gender + age_b + 
      father_edu_f + mother_edu_f +
      father_occ_salary + mother_occ_salary + adult_members + siblings + duration_int + sch_id +
      (treated_int | class_id),
    data = data,
    prior = my_prior, 
    chains = 4,
    seed = 1234,
    control = list(adapt_delta = 0.99,
                   max_treedepth = 15)
  )
  
  fit_all <- brm(
    all_gpa_e ~ all_gpa_b + treated_int +
      class_size + grade + gender + age_b + 
      father_edu_f + mother_edu_f +
      father_occ_salary + mother_occ_salary + adult_members + siblings + duration_int + sch_id +
      (treated_int | class_id),
    data = data,
    prior = my_prior, 
    chains = 4,
    seed = 1234,
    control = list(adapt_delta = 0.99,
                   max_treedepth = 15)
  )
  
  return(list(fit_nepali, fit_english, fit_math, fit_science, fit_all))
}
