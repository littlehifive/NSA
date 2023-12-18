library(timevis)
library(plotly)

timeline_ktm <- data.frame(
  id      = 1:8,
  content = c("Baseline", 
              "Intervention<br>(Mar.18 - Apr.12)",
              "Grade 6-9 Exams<br>(Apr.12 - Apr.22)", 
              "Grade 10 Exams<br>(Apr.22 - May.3)",
              "Grade 12 Exams<br>(May.30 - Jun.8)",
              "Grade 11 Exams<br>(Jun.12 - Jun.20)",
              "Endline (Grade 6, 7, 8, 9, 11)",
              "Endline (Grade 10)"
              ),
  start   = c("2022-03", 
              "2022-03-18",
              "2022-04-12", 
              "2022-04-22",
              "2022-05-30",
              "2022-06-12",
              "2022-06-25",
              "2022-08-15"),
  end     = c(NA, 
              NA,#"2022-04-12", 
              NA, 
              NA,#"2022-05-03",
              NA,#"2022-06-08",
              NA,#"2022-06-20",
              NA,
              NA
              )
)
  
timevis(timeline_ktm)


timeline_pokhara<- data.frame(
  id      = 1:8,
  content = c("Baseline", 
              "Intervention<br>(Mar.09 - May.03)",
              "Grade 6-9 Exams<br>(Apr.12 - Apr.21)", 
              "Grade 10 Exams<br>(Apr.22 - May.3)",
              "Grade 12 Exams<br>(May.30 - Jun.8)",
              "Grade 11 Exams<br>(Jun.12 - Jun.20)",
              "Endline (Grade 6, 7, 8, 9, 11)",
              "Endline (Grade 10)"
  ),
  start   = c("2022-03", 
              "2022-03-09",
              "2022-04-11", 
              "2022-04-22",
              "2022-05-30",
              "2022-06-12",
              "2022-06-25",
              "2022-09-05"),
  end     = c(NA, 
              NA,
              NA, 
              NA,
              NA,
              NA,
              NA,
              NA
  )
)

timevis(timeline_pokhara)

timeline_baglung <- data.frame(
  id      = 1:7,
  content = c("Baseline", 
              "Intervention<br>(Mar.16 - Apr.17)",
              "Grade 6-9 Exams<br>(Apr.25 - May.5)", 
              "Grade 10 Exams<br>(Apr.22 - May.3)",
              "Grade 12 Exams<br>(May.30 - Jun.8)",
              "Grade 11 Exams<br>(Jun.12 - Jun.20)",
              "Endline (Grades 6-12)"
  ),
  start   = c("2022-02-25", 
              "2022-03-16",
              "2022-04-25", 
              "2022-04-22",
              "2022-05-30",
              "2022-06-12",
              "2022-06-28"),
  end     = c(NA, 
              NA,
              NA, 
              NA,
              NA,
              NA,
              NA
  )
)

timevis(timeline_baglung)



