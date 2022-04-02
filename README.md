# Nepal Self-affirmation Study

## Cleaning Log

### **04.02.2022**:

#### **Kathmandu baseline check:**

See `Baseline/Comments Addressed_29032022.docx` for RIDA's response to my earlier questions. RIDA also provided the missing responses for the last 12 rows, which I manually added to the original KTM data sheet.

Among the pre-test kids, SCH2_GR10_ST3 (the last row) has 37 siblings, which seems in probable. **RIDA please confirm if it should be 3 instead.**

Apart from that, `siblings == 27` is changed to `2` as confirmed by RIDA.

`student_type` is created with value of "Deaf" to match the variable name in Baglung data.

#### **Pokhara baseline check:**

`student_type` is created with value of "Deaf" to match the variable name in Baglung data.

#### **Baglung baseline check:**

No checks from RIDA needed.

| Variable name                                                       | Operations                                                     |
|---------------------------------------------------------------------|----------------------------------------------------------------|
| student_type                                                        | labels set to original text values (for R analysis only)       |
| grade                                                               | Should be numeric value 6-12 instead of texts (e.g. "Grade 6") |
| std_id                                                              | name changed to st_id; "-" changed to "\_"                     |
| gender                                                              | 1/2 labels changed to male/female (for R analysis only)        |
| father_edu                                                          | labels set to original text values (for R analysis only)       |
| mother_edu                                                          | labels set to original text values (for R analysis only)       |
| father_occ                                                          | labels set to original text values (for R analysis only)       |
| mother_occ                                                          | labels set to original text values (for R analysis only)       |
| 5-point survey questions (from capable_person to conclusion_abt_me) | 6 changed to missing (for R analysis only)                     |
| 4-point survey (from bad_grades to pressure_parent_teacher)         | 5 changed to missing (for R analysis only)                     |

### **03.28.2022**:

#### **Kathmandu baseline check:**

The variable names in tab 1 does not correspond perfectly with tab 2. I am going with whatever is the variable names in the actual dataset for now. Also note that there should not be spaces in variable names. The following names are changed (right hand side changed to left hand side):

    worried_other_think = worried_.other_think,

    conclusion_my_perform = conclusion_my_.perform,

    conclusion_abt_me = `_12_conclusion_me`,

    teacher_like_me = teacher_likeme,

    comfortable_who_i_am = comfortable_who_iam,

    not_understand_class = not_.understand_class

The following variables have been cleaned by me, **several checks are needed from RIDA**:

| Variable name                                                       | Operations                                                                                                                                                         |
|---------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| sch_id                                                              | There is one value labeled as 27 instead of 2.                                                                                                                     |
| std_name                                                            | removed white spaces at the end of the string, and set the names to be in title case.                                                                              |
| grade                                                               | changed from numeric to factor (for R analysis only).                                                                                                              |
| std_id                                                              | name changed to st_id; "-" changed to "\_"                                                                                                                         |
| gender                                                              | no gender available for SCH2_GR9_ST4, set to female based on name, **RIDA please check if it is correct**; 1/2 labels changed to male/female (for R analysis only) |
| age                                                                 | no change; **RIDA please check if age \> 22 is indeed correct**                                                                                                    |
| father_edu                                                          | labels set to original text values (for R analysis only)                                                                                                           |
| mother_edu                                                          | labels set to original text values (for R analysis only)                                                                                                           |
| father_occ                                                          | labels set to original text values (for R analysis only)                                                                                                           |
| mother_occ                                                          | labels set to original text values (for R analysis only)                                                                                                           |
| adult_members                                                       | **RIDA please check if 14 is possible.**                                                                                                                           |
| siblings                                                            | **RIDA please check if 27 is possible.**                                                                                                                           |
| 5-point survey questions (from capable_person to conclusion_abt_me) | 6 changed to missing (for R analysis only)                                                                                                                         |
| 4-point survey (from bad_grades to pressure_parent_teacher)         | 5 changed to missing (for R analysis only)                                                                                                                         |

**RIDA please also check what happens to the kids that have missingness for the entire row.**

#### Pokhara baseline check

The variable names in tab 1 does not correspond perfectly with tab 2. I am going with whatever is the variable names in the actual dataset for now. Also note that there should not be spaces in variable names. The following names are changed (right hand side changed to left hand side):

    worried_other_think = worried_.other_think,

    conclusion_my_perform = conclusion_my_.perform,

    conclusion_abt_me = `_12_conclusion_me`,

    teacher_like_me = teacher_likeme,

    comfortable_who_i_am = comfortable_who_iam,

    not_understand_class = not_.understand_class

The following variables have been cleaned by me, **several checks are needed from RIDA**:

| Variable name                                                       | Operations                                                      |
|---------------------------------------------------------------------|-----------------------------------------------------------------|
| std_id                                                              | name changed to st_id                                           |
| gender                                                              | 1/2 labels changed to male/female (for R analysis only)         |
| age                                                                 | no change; **RIDA please check if age \> 22 is indeed correct** |
| father_edu                                                          | labels set to original text values (for R analysis only)        |
| mother_edu                                                          | labels set to original text values (for R analysis only)        |
| father_occ                                                          | labels set to original text values (for R analysis only)        |
| mother_occ                                                          | labels set to original text values (for R analysis only)        |
| 5-point survey questions (from capable_person to conclusion_abt_me) | 6 changed to missing (for R analysis only)                      |
| 4-point survey (from bad_grades to pressure_parent_teacher)         | 5 changed to missing (for R analysis only)                      |
| notes                                                               | What does "Mother expired" mean for row 18?                     |
