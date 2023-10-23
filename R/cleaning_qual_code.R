# library(tidyverse)

# clean qualitative codes
clean_qual_code <- function(qual_code){
  
  # clean variable names
  qual_code_cleaned <- qual_code |> 
    janitor::clean_names() |> 
    select(st_id, pragmatic_values:pragmatic_values_for_others)
  
  # test code below:
  # temp <- qual_code_cleaned %>%
  #   mutate(across(-st_id, ~ifelse(. > 0, 1, 0)))
  # 
  # temp <- temp |> filter(str_detect(st_id, "GR[6-9]|GR10"))
  
  # merge thematic codes
  qual_code_cleaned <- qual_code_cleaned |> 
    mutate(
      # ---- Affirmed Themes ----
      
      # Individual Growth
      theme_t_interest_and_learn_new_things = interest + learn_new_things,
      theme_t_religion_heritage = cultural_heritage_religion,
      theme_t_career_aspirations = pragmatic_values,
      theme_t_non_career_aspirations = domain_specific_growth_and_accomplishment + being_able_to_communicate + health + competition_and_reward,
      theme_t_self_identity = be_independent_and_free + prove_myself + recognition_from_others,
      
      # Relationship building
      theme_t_relationship_with_family = familial_support + help_family_make_family_happy,
      theme_t_relationship_with_friends = support_from_friends + help_support_others + sharing_and_socializing,
      
      # Broader prosocial behavior
      theme_t_prosocial_behavior = help_support_others + give_back_to_society,
      
      # ---- Unaffirmed Themes ----
      theme_c_no_knowledge = no_knowledge_about_it,
      theme_c_no_interest = not_interested,
      theme_c_communication_barriers = communication_barriers,
      theme_c_no_support = friends_not_supportive + family_not_supportive + bad_influence_discriminations_from_friends, #+ systematic_barriers + no_support
      theme_c_other = used_to_be_interested_not_anymore + cannot_do_it_for_other_physical_reasons + cannot_do_it +
        not_relevant_fit_to_young_people + not_beneficial_sustainable + no_time + risk + not_possible_necessary + fear + 
        not_sure_about_future + brings_bad_influence_to_society + social_comparison + economic_pressure + 
        not_in_the_position_to_do_whatever_i_want + issues_in_society + no_progress_in_certain_fields +
        argument_among_others + independence + unfamiliar_with_new_things + others_not_being_obedient +
        only_for_entertainment_hamper_life + cause_problems_for_others + takes_time_to_process + no_success +
        lazy + wish_to_share_moments_with_others + not_a_habit, 
      
      # ---- Affirmed Themes for Others ----
      theme_o_aspirations = pragmatic_values_for_others + domain_specific_growth_for_others,
      theme_o_interest_ability = interest_ability_of_others,
      theme_o_reward = reward_for_others,
      theme_o_support = support_for_or_from_others_family + support_for_or_from_others,
      theme_o_other = others_are_smart + others_dont_have_deafness + 
        appreciation_from_teachers_for_others + similarity_among_others
    ) |> 
    select(st_id, starts_with("theme_"))
  
  # create dummy for the selected themes
  qual_code_cleaned <- qual_code_cleaned %>%
    mutate(across(starts_with("theme_"), ~ifelse(. > 0, 1, 0)))

    return(qual_code_cleaned)
  
}