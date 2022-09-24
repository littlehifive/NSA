# cleaning and checking

# library(tidyverse)
# library(sjlabelled)

# read transcribed text data

# path <- "/Volumes/GoogleDrive/My Drive/Nepal SA Study/Data"

# dat_form_a <- openxlsx::read.xlsx(
#   file.path(path, 
#             "Intervention/original/intervention_codebook.xlsx"),
#   sheet = 1)

# dat_form_bc <- openxlsx::read.xlsx(
#   file.path(path, 
#             "Intervention/original/intervention_codebook.xlsx"),
#   sheet = 2)

clean_dat_int <- function(dat_int_form_a_raw, 
                          dat_int_form_bc_raw, 
                          name_check){
  
# add variable labels

label_a <- dat_int_form_a_raw |> slice(1) |> unlist(use.names = FALSE)

dat_form_a <- dat_int_form_a_raw |> slice(-1) |> set_label(label_a)

label_bc <- dat_int_form_bc_raw |> slice(1) |> unlist(use.names = FALSE)

dat_form_bc <- dat_int_form_bc_raw |> slice(-1) |> set_label(label_bc)

# row bind form a and bc
dat_int <- bind_rows(dat_form_a, dat_form_bc)

# clean s_id
dat_int <- dat_int |> 
  rename(st_id = s_id) |> 
  mutate(st_id = gsub("\\s+", "", st_id)) |>
  mutate(st_id = gsub("_$", "", st_id)) |> 
  mutate(st_id = gsub("-", "_", st_id)) |> 
  mutate(st_id = ifelse(st_id == "SCH3GR9ST3", "SCH3_GR9_ST3", st_id))

# rename and arrange by school
dat_int <- dat_int |> 
  mutate(sch_id = as.numeric(gsub("^SCH(\\d+)_GR(\\d+)_ST(\\d+)$", "\\1", st_id))) |>
  mutate(grade = as.numeric(gsub("^SCH(\\d+)_GR(\\d+)_ST(\\d+)$", "\\2", st_id))) |> 
  mutate(temp_id = as.numeric(gsub("^SCH(\\d+)_GR(\\d+)_ST(\\d+)$", "\\3", st_id))) |> 
  arrange(sch_id, grade, temp_id) |>  # sort by grade and student ID
  select(-temp_id)

# clean st_name
dat_int <- dat_int |> 
  rename(st_name = s_name) 

dat_int <- dat_int |> 
  mutate(st_name = gsub("^\\s+", "", st_name)) |> 
  mutate(st_name = gsub("\\s+$", "", st_name))

# clean duration
dat_int <- dat_int |> 
  mutate(duration = ifelse(duration == "10 min 12 sec", 10.2, duration)) |> 
  mutate(duration = ifelse(duration == "16 min 23 sec", 16.38, duration)) |> 
  mutate(duration = gsub("\\s+", "", duration)) |>
  mutate(duration = gsub("min", "", duration)) |> 
  mutate(duration = as.numeric(duration))
  
# clean int_q1_value_1
dat_int <- dat_int |> 
  mutate(int_q1_value_1 = gsub("^\\s+", "", int_q1_value_1)) |> 
  mutate(int_q1_value_1 = gsub("\\s+$", "", int_q1_value_1))

dat_int <- dat_int |> 
  mutate(int_q1_value_1 = case_when(
    int_q1_value_1 == "Creating art, painting, or handicraft / Being good at art (painting, dance, etc." ~ "Creating art, painting, or handicraft / Being good at art (painting, dance, etc.)",
    int_q1_value_1 == "Creating new things or generating\nnew ideas" ~ "Creating new things or generating new ideas",
    int_q1_value_1 == "Creating new things or generating\r\nnew ideas" ~ "Creating new things or generating new ideas",
    TRUE ~ int_q1_value_1
  ))

# clean int_q1_value_2

dat_int <- dat_int |> 
  mutate(int_q1_value_2 = gsub("^\\s+", "", int_q1_value_2)) |> 
  mutate(int_q1_value_2 = gsub("\\s+$", "", int_q1_value_2))

dat_int <- dat_int |> 
  mutate(int_q1_value_2 = case_when(
    int_q1_value_2 == "Creating art, painting, or handicraft / Being good at art (painting, dance, etc." ~ "Creating art, painting, or handicraft / Being good at art (painting, dance, etc.)",
    int_q1_value_2 == "Being a part of a social group (such as your community, ethnic group, or school" ~ "Being a part of a social group (such as your community, ethnic group, or school club)",
    int_q1_value_2 == "Being interested in politics / being politically active\r\nMaking people happy" ~ "Being interested in politics / being politically active",
    int_q1_value_2 == "Being interested in politics / being politically active for the development of the country." ~ "Being interested in politics / being politically active",
    TRUE ~ int_q1_value_2
  ))

# clean int_q1_value_3

dat_int <- dat_int |> 
  mutate(int_q1_value_3 = gsub("^\\s+", "", int_q1_value_3)) |> 
  mutate(int_q1_value_3 = gsub("\\s+$", "", int_q1_value_3))

dat_int <- dat_int |> 
  mutate(int_q1_value_3 = case_when(
    int_q1_value_3 == "My\r\nrelationships with friends" ~ "My relationships with friends",
    TRUE ~ int_q1_value_3
  ))

# clean int_q2
dat_int <- clean_space(dat_int, "int_q2")

# clean int_q3
dat_int <- clean_space(dat_int, "int_q3")

# clean mc_reason

# MS Excel will turn - into a function by adding =; remove all the "-"
dat_int <- dat_int |> 
  mutate(mc_reason = gsub("-", "", mc_reason))

dat_int <- clean_space(dat_int, "mc_reason")

# clean mc_survey_#
dat_int <- dat_int |> 
  mutate_at(vars(mc_survey_1:mc_survey_4), as.numeric)

# clean variable type
dat_int <- dat_int |> 
  mutate(sch_id = as.factor(sch_id),
         grade = as.factor(grade))

# reorder variables
dat_int <- dat_int |> select(sch_id, grade, st_id:notes)

# clean student names
dat_int <- dat_int |> 
  left_join(name_check, by = c("st_id" = "st_id_correct")) |> 
  select(-st_name) |> 
  rename(st_name = st_name_correct) |> 
  select(sch_id:st_id, st_name, form:notes)

# add wave tag
names(dat_int) <- paste0(names(dat_int), "_int")

# write data
# write_csv(dat_int, file.path(path, "Intervention/cleaned/Intervention_response.csv"))

return(dat_int)
}


# clean the intervention dates
clean_dat_int_date <- function(dat_int_date_raw){

  dat_int_date <- dat_int_date_raw |> 
    mutate(date_int = parse_date_time(date_int, orders = "dmy")) |> 
    select(st_id_int, date_int)
  
  return(dat_int_date)
}


