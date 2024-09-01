seeBias: Fairness Evaluation and Visualisation
================

- [Demo](#demo)
  - [Usage](#usage)
  - [Details on return values](#details-on-return-values)
  - [Multiple sensitive variables](#multiple-sensitive-variables)
  - [Specify predicted scores](#specify-predicted-scores)

## Demo

### Usage

``` r
library(seeBias)
# Load example data
data("compas")
head(compas)
##   Two_yr_Recidivism Number_of_Priors Age_Above_FourtyFive Age_Below_TwentyFive
## 1                 0                0                    1                    0
## 2                 1                0                    0                    0
## 3                 1                4                    0                    1
## 4                 0                0                    0                    0
## 5                 1               14                    0                    0
## 6                 0                3                    0                    0
##   Misdemeanor        Ethnicity  Sex
## 1           0            Other Male
## 2           0 African_American Male
## 3           0 African_American Male
## 4           1            Other Male
## 5           0        Caucasian Male
## 6           0            Other Male
table(compas$Two_yr_Recidivism, compas$Ethnicity)
##    
##     African_American Asian Caucasian Hispanic Native_American Other
##   0             1514    23      1281      320               6   219
##   1             1661     8       822      189               5   124
# Not sensible to analyse Asian and Native American as separate categories due
# to insufficient observations. Combine into Other.
compas$Ethnicity <- ifelse(compas$Ethnicity %in% c("Asian", "Native_American"),
                           "Other", as.character(compas$Ethnicity))
compas$Ethnicity <- ifelse(compas$Ethnicity == "African_American",
                           "African American", as.character(compas$Ethnicity))
table(compas$Two_yr_Recidivism, compas$Ethnicity)
##    
##     African American Caucasian Hispanic Other
##   0             1514      1281      320   248
##   1             1661       822      189   137
m <- glm(Two_yr_Recidivism ~ ., data = compas, family = "binomial")
# Extracted predicted risk and observations from test data.
# If not specified, the best threshold in ROC analysis is used.
x <- evaluate_prediction_prob(
  y_pred = predict(m, newdata = compas, type = "response"), 
  y_obs = compas$Two_yr_Recidivism, y_pos = "1",
  sens_var = compas$Ethnicity, sens_var_ref = "Caucasian"
)
## Threshold=0.455 set by ROC analysis.
## Configuring sensitive variables ...
##     4 subgroups based on sensitive variables ('sens_var'): African American, Caucasian, Hispanic, Other.
##     Reference group: Caucasian.
## Configuration completed.
x_plots <- plot(x)
```

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
summary(x)
```

| Fairness metric              | African American | Hispanic |  Other |
|:-----------------------------|-----------------:|---------:|-------:|
| Equal opportunity difference |            0.274 |   -0.010 | -0.067 |
| Equalised odds difference    |            0.274 |   -0.036 | -0.083 |
| BER equality difference      |           -0.042 |   -0.013 | -0.008 |

Equal opportunity ensures that different subgroups have the same True
Positive Rate (TPR). This means that the model is equally good at
correctly identifying positive cases across all groups. It is measured
by comparing the TPR of each subgroup to that of a reference group,
either through a ratio or a difference.

Equalised odds ensures that different subgroups have the same True
Positive Rate (TPR) and False Positive Rate (FPR). This means the model
is equally accurate and equally prone to errors across all groups. It is
assessed by separately comparing the TPR and FPR of each subgroup to
those of a reference group, and then taking the larger disparity—whether
it’s in the TPR or FPR—based on the ratio or difference.

Balanced error rate (BER) equality ensures that the Balanced Error Rate
(BER) is consistent across different subgroups. BER is calculated as the
average of the False Positive Rate (FPR) and False Negative Rate (FNR,
which is 1 minus the True Positive Rate \[TPR\]). This means the model’s
overall error rate, considering both false positives and false
negatives, is uniform across all groups. It is assessed by comparing the
BER of each subgroup to that of a reference group, with disparities
measured using either ratios or differences.

### Details on return values

`seeBias` object:

``` r
x$fairness_evaluation$df_prob
## NULL
x$fairness_evaluation$df_metrics
## NULL
x$fairness_evaluation$df_auc
## NULL
x$fairness_evaluation$df_calib
## NULL
head(x$fairness_evaluation$df_roc)
## NULL
```

Individual plots:

``` r
x_plots$metrics
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
x_plots$roc
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
x_plots$calibration_in_large
```

![](README_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
x_plots$calibration
```

![](README_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

``` r
x_plots$score
```

![](README_files/figure-gfm/unnamed-chunk-5-4.png)<!-- -->

### Multiple sensitive variables

``` r
x2 <- evaluate_prediction_prob(
  y_pred = predict(m, newdata = compas, type = "response"), 
  y_obs = compas$Two_yr_Recidivism, y_pos = "1",
  sens_var = cbind(as.character(compas$Ethnicity), as.character(compas$Sex)), 
  sens_var_ref = c("Caucasian", "Male")
)
## Threshold=0.455 set by ROC analysis.
## Configuring sensitive variables ...
##     8 subgroups based on sensitive variables ('sens_var'): African American & Female, Caucasian & Female, Hispanic & Female, Other & Female, African American & Male, Caucasian & Male, Hispanic & Male, Other & Male.
##     Reference group: Caucasian & Male.
## Configuration completed.
## Warning in qf(p = alpha/2, df1 = 2 * x, df2 = 2 * (n - x + 1)): NaNs produced
## Warning in qf(p = 1 - alpha/2, df1 = 2 * (x + 1), df2 = 2 * (n - x)): NaNs
## produced
x_plots2 <- plot(x2, print_statistics = FALSE)
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
summary(x2)
```

| Fairness metric              | African American & Female | African American & Male | Caucasian & Female | Hispanic & Female | Hispanic & Male | Other & Female | Other & Male |
|:-----------------------------|--------------------------:|------------------------:|-------------------:|------------------:|----------------:|---------------:|-------------:|
| Equal opportunity difference |                     0.010 |                   0.242 |             -0.296 |            -0.306 |          -0.034 |         -0.323 |       -0.106 |
| Equalised odds difference    |                    -0.026 |                   0.242 |             -0.296 |            -0.306 |          -0.047 |         -0.323 |       -0.106 |
| BER equality difference      |                    -0.018 |                  -0.024 |              0.053 |             0.029 |          -0.006 |          0.028 |        0.005 |

Equal opportunity ensures that different subgroups have the same True
Positive Rate (TPR). This means that the model is equally good at
correctly identifying positive cases across all groups. It is measured
by comparing the TPR of each subgroup to that of a reference group,
either through a ratio or a difference.

Equalised odds ensures that different subgroups have the same True
Positive Rate (TPR) and False Positive Rate (FPR). This means the model
is equally accurate and equally prone to errors across all groups. It is
assessed by separately comparing the TPR and FPR of each subgroup to
those of a reference group, and then taking the larger disparity—whether
it’s in the TPR or FPR—based on the ratio or difference.

Balanced error rate (BER) equality ensures that the Balanced Error Rate
(BER) is consistent across different subgroups. BER is calculated as the
average of the False Positive Rate (FPR) and False Negative Rate (FNR,
which is 1 minus the True Positive Rate \[TPR\]). This means the model’s
overall error rate, considering both false positives and false
negatives, is uniform across all groups. It is assessed by comparing the
BER of each subgroup to that of a reference group, with disparities
measured using either ratios or differences.

``` r
x_plots2$roc
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
x_plots2$calibration_in_large
```

![](README_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
x_plots2$calibration
```

![](README_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

``` r
x_plots2$score
```

![](README_files/figure-gfm/unnamed-chunk-8-4.png)<!-- -->

### Specify predicted scores

``` r
x3 <- evaluate_prediction_score(
  y_pred = predict(m, newdata = compas), 
  y_obs = compas$Two_yr_Recidivism, y_pos = "1",
  sens_var = cbind(as.character(compas$Ethnicity), as.character(compas$Sex)), 
  sens_var_ref = c("Caucasian", "Male")
)
## Threshold=-0.181 set by ROC analysis.
## Configuring sensitive variables ...
##     8 subgroups based on sensitive variables ('sens_var'): African American & Female, Caucasian & Female, Hispanic & Female, Other & Female, African American & Male, Caucasian & Male, Hispanic & Male, Other & Male.
##     Reference group: Caucasian & Male.
## Configuration completed.
## Warning in qf(p = alpha/2, df1 = 2 * x, df2 = 2 * (n - x + 1)): NaNs produced
## Warning in qf(p = 1 - alpha/2, df1 = 2 * (x + 1), df2 = 2 * (n - x)): NaNs
## produced
x_plots3 <- plot(x3, print_statistics = FALSE)
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
summary(x3)
```

| Fairness metric              | African American & Female | African American & Male | Caucasian & Female | Hispanic & Female | Hispanic & Male | Other & Female | Other & Male |
|:-----------------------------|--------------------------:|------------------------:|-------------------:|------------------:|----------------:|---------------:|-------------:|
| Equal opportunity difference |                     0.010 |                   0.242 |             -0.296 |            -0.306 |          -0.034 |         -0.323 |       -0.106 |
| Equalised odds difference    |                    -0.026 |                   0.242 |             -0.296 |            -0.306 |          -0.047 |         -0.323 |       -0.106 |
| BER equality difference      |                    -0.018 |                  -0.024 |              0.053 |             0.029 |          -0.006 |          0.028 |        0.005 |

Equal opportunity ensures that different subgroups have the same True
Positive Rate (TPR). This means that the model is equally good at
correctly identifying positive cases across all groups. It is measured
by comparing the TPR of each subgroup to that of a reference group,
either through a ratio or a difference.

Equalised odds ensures that different subgroups have the same True
Positive Rate (TPR) and False Positive Rate (FPR). This means the model
is equally accurate and equally prone to errors across all groups. It is
assessed by separately comparing the TPR and FPR of each subgroup to
those of a reference group, and then taking the larger disparity—whether
it’s in the TPR or FPR—based on the ratio or difference.

Balanced error rate (BER) equality ensures that the Balanced Error Rate
(BER) is consistent across different subgroups. BER is calculated as the
average of the False Positive Rate (FPR) and False Negative Rate (FNR,
which is 1 minus the True Positive Rate \[TPR\]). This means the model’s
overall error rate, considering both false positives and false
negatives, is uniform across all groups. It is assessed by comparing the
BER of each subgroup to that of a reference group, with disparities
measured using either ratios or differences.

``` r
x_plots3$roc
```

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
x_plots3$calibration_in_large
```

![](README_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

``` r
x_plots3$calibration
```

![](README_files/figure-gfm/unnamed-chunk-11-3.png)<!-- -->

``` r
x_plots3$score
```

![](README_files/figure-gfm/unnamed-chunk-11-4.png)<!-- -->
