library(dplyr)

# Duplicated text responses (possibly due to copy-paste errors by numerators) 
# to be cleaned manually in Excel and re-merged back to the master dataset
dat_text <- dat_all_cleaned |> 
  select(st_id, int_q2_int, int_q3_int) |> 
  filter(!is.na(st_id)) |> 
  mutate(across(c(st_id, int_q2_int, int_q3_int), ~ifelse(is.na(.x), "", .x))) |> 
  mutate(essay_int = paste(int_q2_int, int_q3_int)) |> 
  select(st_id, essay_int) |> 
  mutate(essay_int = ifelse(essay_int == " ", NA_character_, essay_int))

openxlsx::write.xlsx(dat_text, "/Users/michaelfive/Library/CloudStorage/GoogleDrive-wuzezhen33@gmail.com/My Drive/Nepal SA Study/Data/Checks/int_text.xlsx")
