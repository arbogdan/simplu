# Simplu
Simplu: A comprehsive dataset summarization tool

Simplu (Romanian for "simple") is intended to be a single function that can conduct all of the basic data summarization any analyst or data scientist may need when preparing for a larger analysis. Due to the coarse nature of the output from this function, it is not intended to be a replacement for summary tables and graphics. Rather, it is intended to supply important information regarding a dataset and variables to an analyst as succinctly as possible to facilitate their ultimate goal of a complete analysis. 

**Current release**  
Simplu is still in the alpha stages of development. Currently, user modification is required for functionality (just complete the overall loop and include the *function* statement), however the following results are currently output:
  - Determination of appropriate data types based on variable contents
  - Summary table for all continuous variables (including results from Shapiro-Wilks Normality test)
    - Contains: n, n.miss, pct.miss, min, P25, median, mean, P75, max, is.normal?
  - Histograms with KDE curves for all continuous variables
  - Pearson correlations & scatterplots with loess smoothed curves for all continuous variable pairs
  - Summary table for all categorical variables
    - Contains: n, n.miss, pct.miss, levels, values, p.val (for binomial test or Chi^2 goodness-of-fit)
  - P-values for Chi^2 and/or Fisher's exact test for all categorical variable pairs

**Future releases**  
Ideally, future iterations of this function will also allow for the following:
  - Dataset statistics (dimensions, proportions of data types, overall missing data estimates)
  - Bivariate analyses between continuous and categorical variables
  - For larger datasets, selection criteria for identifying important/influential features
  - More polished output graphics
  - Specification of a dependent variable to suggest transformations and statistical tests
  - Display missing data patterns and offere methods for handling missing data
  - Ability to identify longitudinal data and incorporate this information into summary statistics and recommendations
