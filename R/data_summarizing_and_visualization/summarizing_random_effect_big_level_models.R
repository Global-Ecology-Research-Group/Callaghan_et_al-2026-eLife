library(brms)
library(performance)

out1 <- readRDS("model_objects/kingdom_level/animals_and_plants_RE_viirs_model_v2.rds")
summary(out1)

# Population-Level Effects: 
#                                         Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                                  1.38      0.85    -0.30     3.06 1.00     1146     3341
# kingdomPlantae                             0.05      0.92    -1.75     1.86 1.00    20039    14400
# kingdomAnimalia:body_size_scaled_log10     0.21      0.23    -0.25     0.66 1.00     7937    11577
# kingdomPlantae:body_size_scaled_log10      0.64      0.63    -0.68     1.84 1.00    15324    14033

# Result: The mean effect of body size on urban tolerance tends to be greater in plants than animals (0.64 vs 0.21), 
# but in both cases, there is uncertainty, especially for the plants, so the 95% credible intervals (CI) of the mean effects overlap zero
# (plant 95% CI = -0.68, 1.84; animal 95% CI = -0.25, 0.66).

out2 <- readRDS("model_objects/kingdom_level/animals_and_plants_RE_viirs_model_v1.rds")
summary(out2)

# Population-Level Effects: 
#                                       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                                 1.39      0.84    -0.25     3.05 1.00     2756     6221
# body_size_scaled_log10                    0.23      0.22    -0.22     0.67 1.00    12941    15542 # slope for animals
# kingdomPlantae                            0.07      0.91    -1.73     1.82 1.00    37876    14207
# body_size_scaled_log10:kingdomPlantae     0.50      0.62    -0.76     1.69 1.00    24124    14190 # difference in slope for plants

# Result: There is no difference in the body size filtering of urban use between plants and animals (95%CI of difference = -0.76, 1.69)

# Group-Level Effects: 
#   ~class (Number of levels: 40) 
#                                         Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)                             2.34      0.45     1.57     3.32 1.00     9482    12661
# sd(body_size_scaled_log10)                0.27      0.17     0.02     0.65 1.00     7503     8732
# cor(Intercept,body_size_scaled_log10)     0.56      0.41    -0.58     0.99 1.00    13065    12649
# 
# ~class:order (Number of levels: 295) 
#                                         Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)                             1.75      0.24     1.29     2.23 1.00     4493     7781
# sd(body_size_scaled_log10)                0.73      0.20     0.33     1.11 1.00     2269     2203
# cor(Intercept,body_size_scaled_log10)    -0.39      0.24    -0.78     0.14 1.00     3374     4979
# 
# ~class:order:family (Number of levels: 1451) 
#                                         Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)                             2.43      0.09     2.26     2.62 1.00     6885    12176
# sd(body_size_scaled_log10)                1.24      0.09     1.06     1.42 1.00     4710     9365
# cor(Intercept,body_size_scaled_log10)     0.17      0.07     0.02     0.31 1.00     4766     9257
# 
# ~metadata2 (Number of levels: 57) 
#                                         Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)                             1.87      0.31     1.32     2.55 1.00     7800    11479
# sd(body_size_scaled_log10)                0.77      0.22     0.41     1.26 1.00     7341    11577
# cor(Intercept,body_size_scaled_log10)     0.37      0.25    -0.18     0.78 1.00    11455    13292
# 
# ~subrealm (Number of levels: 50) 
#                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     4.21      0.46     3.43     5.21 1.00     3799     7311

# Result: Variation in urban use tends to be greater among families (sd = 2.43) than among orders (sd = 1.75), 
# but only slightly greater than among classes (sd = 2.34). However, variation in the effect of body size is much greater 
# among families (sd = 1.24) than among orders (sd = 0.73) or classes (sd = 0.27). 
# This indicates that body size is best able to predict why some families have higher urban tolerance compared to other families,
# rather than why some orders or classes have higher urban tolerances compared to other orders or classes.
# This makes sense: at higher taxonomic levels, taxa will vary in more diverse ways, in addition to body size, which may obscure
# simple body size effects.

icc(out1)
# icc = the proportion of the variance explained by the grouping structure in the population
Group              |   ICC
--------------------------
class              | 0.081
class:order        | 0.045
class:order:family | 0.084
metadata2          | 0.051
subrealm           | 0.253 # urban tolerances tend to be more similar within same subrealm

# Result: The ICC (intraclass correlation coefficient) agrees - family is a better predictor of urban tolerance, overall, compared to other taxonomic ranks.
# But value is quite low at 0.08, so many other factors explain urban tolerance too.

