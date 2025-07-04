# Nepal Self-Affirmation Study

This git repo contains codes that are used to clean and analyze the data used in the following paper:

Wu, Z., Sabarwal, S., Basnet, D. et al. Understanding the effects of values affirmation on deaf students in Nepal: evidence from a randomized controlled trial. Soc Psychol Educ 28, 140 (2025). https://doi.org/10.1007/s11218-025-10097-7

See also [the study page](https://osf.io/4nprt/) on Open Science Framework (OSF).

Notes:

1.  We used the `targets` package for reproducible data cleaning. However, we are not able to sharing the raw uncleaned data, because combinations of demographic information may be used to identify the students. We share the cleaning codes here (in the `R/` folder and the `_targets.R` file) to showcase our effect to build a reproducible data cleaning workflow during and after project data collection.

2. In `analysis/codes/bayesian_analysis_gr6-10_osf.Rmd`, you will find all relevant codes to produce the results and graphs in the submitted manuscript. Due to the time-consuming nature of building Bayesian models, we saved our models in an `.Rdata` file and stored it on OSF. You can load the models to your environment and replicate the model results we present in the manuscript.

3. Factor scores are created in `R/create_fscore.R`, based on k-fold cross-validation results in `kfa/` and the procedures described in the manuscript and the supplemental materials.

4. Please see the [published paper and the appendix](https://link.springer.com/article/10.1007/s11218-025-10097-7#article-info) for detailed information on the qualitative analysis (i.e. Study 2).
