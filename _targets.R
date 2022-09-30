# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tidyverse", "sjlabelled", "lubridate"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  # 0. set data file root path
  tar_target(
    path, "/Users/michaelfive/Library/CloudStorage/GoogleDrive-wuzezhen33@gmail.com/My Drive/Nepal SA Study/Data"
  ),
  
  # 1.1 load raw data - baseline
  tar_target(
    dat_ktm_b_raw,
    openxlsx::read.xlsx(
      file.path(path, "Baseline/original/Baseline_Survey_NSA_Kathmandu_27032022_v01.xlsx")
      )
  ),
  tar_target(
    dat_pokhara_b_raw,
    openxlsx::read.xlsx(
      file.path(path, "Baseline/original/Baseline_Survey_NSA_Pokhara_27032022_v01.xlsx")
    )
  ),
  tar_target(
    dat_baglung_b_raw,
    openxlsx::read.xlsx(
      file.path(path, "Baseline/original/Baseline_Survey_NSA_Baglung_31032022_v01.xlsx")
    )
  ),  
  tar_target(
    dat_b_date_raw,
    openxlsx::read.xlsx(
      file.path(path, "Baseline/original/baseline_date.xlsx"),
      sheet = 1,
      detectDates = T)
  ),
  
  # 1.2 load raw data - intervention
  tar_target(
    dat_int_form_a_raw,
    openxlsx::read.xlsx(
      file.path(path, "Intervention/original/intervention_codebook.xlsx"),
      sheet = 1)
  ),
  tar_target(
    dat_int_form_bc_raw,
    openxlsx::read.xlsx(
      file.path(path, "Intervention/original/intervention_codebook.xlsx"),
      sheet = 2)
  ),
  tar_target(
    dat_int_date_raw,
    openxlsx::read.xlsx(
      file.path(path, "Intervention/original/intervention_date.xlsx"),
      sheet = 1)
  ),
  
  # 1.3 load raw data - endline
  tar_target(
    dat_ktm_e_raw,
    openxlsx::read.xlsx(
      file.path(path, "Endline/original/Endline_Kathmandu v0.1.xlsx")
    )
  ),
  tar_target(
    dat_pokhara_e_raw,
    openxlsx::read.xlsx(
      file.path(path, "Endline/original/Endline_Pokhara v0.1.xlsx")
    )
  ),
  tar_target(
    dat_baglung_e_raw,
    openxlsx::read.xlsx(
      file.path(path, "Endline/original/Endline_Baglung v0.1.xlsx")
    )
  ),
  tar_target(
    dat_e_date_raw,
    openxlsx::read.xlsx(
      file.path(path, "Endline/original/endline_date.xlsx"),
      sheet = 1,
      detectDates = T)
  ),
  
  # 1.4 load name checks
  tar_target(
    name_check,
    readr::read_csv(
      file.path(path, "Checks/name_mismatch_final.csv"),
      show_col_types = FALSE
      )
    ),
  
  # 2.1 clean data - baseline
  tar_target(
    dat_ktm_b_cleaned,
    clean_dat_ktm_b(dat_ktm_b_raw, name_check)
  ),
  tar_target(
    dat_pokhara_b_cleaned,
    clean_dat_pokhara_b(dat_pokhara_b_raw, name_check)
  ),
  tar_target(
    dat_baglung_b_cleaned,
    clean_dat_baglung_b(dat_baglung_b_raw, name_check)
  ),
  tar_target(
    dat_b_cleaned,
    dplyr::bind_rows(dat_ktm_b_cleaned, dat_pokhara_b_cleaned, dat_baglung_b_cleaned) |> 
      left_join(
        clean_dat_b_date(dat_b_date_raw),
        by = "st_id_b"
      )
  ),
  
  # 2.2 clean data - intervention
  tar_target(
    dat_int_cleaned,
    clean_dat_int(dat_int_form_a_raw, dat_int_form_bc_raw, name_check) |> 
      left_join(
        clean_dat_int_date(dat_int_date_raw),
        by = "st_id_int"
        )
  ),
  
  # 2.3 clean data - endline
  tar_target(
    dat_ktm_e_cleaned,
    clean_dat_ktm_e(dat_ktm_e_raw, name_check)
  ),
  tar_target(
    dat_pokhara_e_cleaned,
    clean_dat_pokhara_e(dat_pokhara_e_raw, name_check)
  ),
  tar_target(
    dat_baglung_e_cleaned,
    clean_dat_baglung_e(dat_baglung_e_raw, name_check)
  ),
  tar_target(
    dat_e_cleaned,
    dplyr::bind_rows(dat_ktm_e_cleaned, dat_pokhara_e_cleaned, dat_baglung_e_cleaned) |> 
      left_join(
        clean_dat_e_date(dat_e_date_raw),
        by = "st_id_e"
      )
  ),
  
  # 3.1 merge data - all waves
  tar_target(
    dat_all_cleaned,
    merge_dat(dat_b_cleaned, dat_e_cleaned, dat_int_cleaned)
  ),
  
  # 3.2 cleaned data with reverse code certain items - all waves
  tar_target(
    dat_all_cleaned_reverse_coded,
    reverse_code_dat(dat_all_cleaned)
  ),
  
  # 4.1 export cleaned data - baseline
  tar_target(
    export_dat_ktm_b_cleaned,
    readr::write_csv(dat_ktm_b_cleaned, file.path(path, "Baseline/cleaned/Baseline_Kathmandu.csv"))
  ),
  tar_target(
    export_dat_pokhara_b_cleaned,
    readr::write_csv(dat_pokhara_b_cleaned, file.path(path, "Baseline/cleaned/Baseline_Pokhara.csv"))
  ),  
  tar_target(
    export_dat_baglung_b_cleaned,
    readr::write_csv(dat_baglung_b_cleaned, file.path(path, "Baseline/cleaned/Baseline_Baglung.csv"))
  ),
  
  # 4.2 export cleaned data - intervention
  tar_target(
    export_dat_int_cleaned_1,
    readr::write_csv(dat_int_cleaned, file.path(path, "Intervention/cleaned/Intervention_response.csv"))
  ),
  
  # 4.3 export cleaned data - endline
  tar_target(
    export_dat_ktm_e_cleaned,
    readr::write_csv(dat_ktm_e_cleaned, file.path(path, "Endline/cleaned/Endline_Kathmandu.csv"))
  ),
  tar_target(
    export_dat_pokhara_e_cleaned,
    readr::write_csv(dat_pokhara_e_cleaned, file.path(path, "Endline/cleaned/Endline_Pokhara.csv"))
  ),  
  tar_target(
    export_dat_baglung_e_cleaned,
    readr::write_csv(dat_baglung_e_cleaned, file.path(path, "Endline/cleaned/Endline_Baglung.csv"))
  ),
  
  # 4.4 export cleaned data - by wave and across wave
  tar_target(
    export_dat_b_cleaned,
    readr::write_csv(dat_b_cleaned, file.path(path, "Cleaned/Baseline_cleaned.csv"))
  ),
  tar_target(
    export_dat_e_cleaned,
    readr::write_csv(dat_e_cleaned, file.path(path, "Cleaned/Endline_cleaned.csv"))
  ),
  tar_target(
    export_dat_int_cleaned_2,
    readr::write_csv(dat_int_cleaned, file.path(path, "Cleaned/Intervention_cleaned.csv"))
  ),
  tar_target(
    export_dat_all_cleaned,
    readr::write_csv(dat_all_cleaned, file.path(path, "Cleaned/All_cleaned.csv"))
  ),
  tar_target(
    export_dat_all_cleaned_reverse_coded,
    readr::write_csv(dat_all_cleaned_reverse_coded, file.path(path, "Cleaned/All_cleaned_reverse_coded.csv"))
  )
)
