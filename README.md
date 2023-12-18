# Nepal Self-Affirmation Study

This git repo contains codes that are used to clean and analyze the data used in the following paper:

\<Values Affirmation Intervention Boosts Academic Performance and Psychological Well- Being in School for Deaf Students in Nepal Evidence from a Randomized Controlled Trial\>

Notes:

1.  We used the `targets` package for reproducible data cleaning. However, we are not able to sharing the raw uncleaned data, because combinations of demographic information may be used to identify the students. We share the cleaning codes here (in the `R/` folder and the `_targets.R` file) to showcase our effect to build a reproducible data cleaning workflow during and after project data collection.

2. In `analysis/codes/bayesian_analysis_gr6-10_osf.Rmd`, you will find all relevant codes to produce the results and graphs in the submitted manuscript. Due to the time-consuming nature of building Bayesian models, we saved our models in an `.Rdata` file and stored it on OSF. You can load the models to your environment and replicate the model results we present in the manuscript.

3. Factor scores are created in `R/create_fscore.R`, based on k-fold cross-validation results in `kfa/` and the procedures described in the manuscript and the supplemental materials.

4. We are currently working on a second paper that uses thematic codes from the values affirmation reflections. We will only share the de-identified text data after we finish working on that paper.
