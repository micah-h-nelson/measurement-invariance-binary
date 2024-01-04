Measurement Invariance Testing with Binary Indicators
================
Micah Nelson
2024-1-4

The classic measurement invariance testing approach assumes indicators
of latent variables are continuous. If indicators are binary, the
invariance tests and CFA parameter estimates results can be flawed.

Methodologists have proposed a number of different ways to conduct
categorical MI tests. This example code shows how to use a recent method
proposed by Wu and Estabrook
(<https://doi.org/10.1007/s11336-016-9506-0>) When all indicators are
binary, the options for testing invariance are more limited: Only the
configural, scalar invariance, and residual invariance models can be
estimated.

Here, I use ANES 2020 data to examine differences in the child
rearing/authoritarianism scale by gender

Load in necessary packages and read in the data (note: dataset is
pre-cleaned)

``` r
library(lavaan)
library(semTools)
library(tidyverse)
library(haven)

anes2020 <- read_dta("anes_2020_cleaned.dta")
```

Drop cases in which there are missing data for gender

``` r
anes2020 <- anes2020[!is.na(anes2020$female),]
```

Define model structure

``` r
auth.model <-'
auth =~ auth2 + auth1 + auth3 + auth4
'
```

Create empty matrix to store fit indices from the models we will
estimate

``` r
fit.indices <- matrix(NA, nrow = 3, ncol = 7)
```

Use measEQ.syntax() to generate code for configural model

``` r
configural <- as.character(measEq.syntax(configural.model = auth.model,
                               data = anes2020,
                               ordered = c("auth1", "auth2", "auth3", "auth4"),
                               parameterization = "theta",
                               ID.fac = "std.lv",
                               ID.cat = "Wu.Estabrook.2016",
                               group = "female",
                               group.equal = "configural"))
```

Estimate model, see results, and add fit indices to matrix

``` r
fit.configural <- cfa(model = configural, data = anes2020, group = "female",
                         ordered = c("auth1", "auth2", "auth3", "auth4"),
                         parameterization = "theta")

summary(fit.configural, fit.measures = TRUE)
```

    ## lavaan 0.6.17 ended normally after 67 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        16
    ## 
    ##   Number of observations per group:               Used       Total
    ##     0                                             3299        3763
    ##     1                                             3935        4450
    ## 
    ## Model Test User Model:
    ##                                               Standard      Scaled
    ##   Test Statistic                                59.782      96.560
    ##   Degrees of freedom                                 4           4
    ##   P-value (Chi-square)                           0.000       0.000
    ##   Scaling correction factor                                  0.619
    ##   Shift parameter                                            0.032
    ##     simple second-order correction                                
    ##   Test statistic for each group:
    ##     0                                           28.185      45.525
    ##     1                                           31.597      51.036
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                              7983.853    7124.064
    ##   Degrees of freedom                                12          12
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.121
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.993       0.987
    ##   Tucker-Lewis Index (TLI)                       0.979       0.961
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                         0.942
    ##   Robust Tucker-Lewis Index (TLI)                            0.825
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.062       0.080
    ##   90 Percent confidence interval - lower         0.049       0.067
    ##   90 Percent confidence interval - upper         0.076       0.094
    ##   P-value H_0: RMSEA <= 0.050                    0.067       0.000
    ##   P-value H_0: RMSEA >= 0.080                    0.020       0.520
    ##                                                                   
    ##   Robust RMSEA                                               0.210
    ##   90 Percent confidence interval - lower                     0.178
    ##   90 Percent confidence interval - upper                     0.244
    ##   P-value H_0: Robust RMSEA <= 0.050                         0.000
    ##   P-value H_0: Robust RMSEA >= 0.080                         1.000
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.044       0.044
    ## 
    ## Parameter Estimates:
    ## 
    ##   Parameterization                               Theta
    ##   Standard errors                           Robust.sem
    ##   Information                                 Expected
    ##   Information saturated (h1) model        Unstructured
    ## 
    ## 
    ## Group 1 [0]:
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   auth =~                                             
    ##     auth2   (l.1_)    1.536    0.110   13.975    0.000
    ##     auth1   (l.2_)    1.271    0.087   14.658    0.000
    ##     auth3   (l.3_)    1.123    0.062   18.137    0.000
    ##     auth4   (l.4_)    0.735    0.044   16.579    0.000
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .auth2   (n.1.)    0.000                           
    ##    .auth1   (n.2.)    0.000                           
    ##    .auth3   (n.3.)    0.000                           
    ##    .auth4   (n.4.)    0.000                           
    ##     auth    (a.1.)    0.000                           
    ## 
    ## Thresholds:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     ath2|t1 (a2.1)   -0.167    0.041   -4.054    0.000
    ##     ath1|t1 (a1.1)   -0.818    0.050  -16.403    0.000
    ##     ath3|t1 (a3.1)    0.373    0.034   10.842    0.000
    ##     ath4|t1 (a4.1)    0.757    0.032   23.550    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .auth2   (t.1_)    1.000                           
    ##    .auth1   (t.2_)    1.000                           
    ##    .auth3   (t.3_)    1.000                           
    ##    .auth4   (t.4_)    1.000                           
    ##     auth    (p.1_)    1.000                           
    ## 
    ## Scales y*:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     auth2             0.545                           
    ##     auth1             0.618                           
    ##     auth3             0.665                           
    ##     auth4             0.806                           
    ## 
    ## 
    ## Group 2 [1]:
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   auth =~                                             
    ##     auth2   (l.1_)    1.339    0.081   16.560    0.000
    ##     auth1   (l.2_)    1.075    0.063   16.973    0.000
    ##     auth3   (l.3_)    1.263    0.070   17.996    0.000
    ##     auth4   (l.4_)    0.778    0.042   18.304    0.000
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .auth2   (n.1.)    0.000                           
    ##    .auth1   (n.2.)    0.000                           
    ##    .auth3   (n.3.)    0.000                           
    ##    .auth4   (n.4.)    0.000                           
    ##     auth    (a.1.)    0.000                           
    ## 
    ## Thresholds:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     ath2|t1 (a2.1)   -0.314    0.036   -8.760    0.000
    ##     ath1|t1 (a1.1)   -0.708    0.038  -18.854    0.000
    ##     ath3|t1 (a3.1)    0.363    0.034   10.623    0.000
    ##     ath4|t1 (a4.1)    0.953    0.033   29.225    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .auth2   (t.1_)    1.000                           
    ##    .auth1   (t.2_)    1.000                           
    ##    .auth3   (t.3_)    1.000                           
    ##    .auth4   (t.4_)    1.000                           
    ##     auth    (p.1_)    1.000                           
    ## 
    ## Scales y*:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     auth2             0.598                           
    ##     auth1             0.681                           
    ##     auth3             0.621                           
    ##     auth4             0.789

``` r
fit.indices[1,] <- round(data.matrix(fitmeasures(fit.configural,
                                                      fit.measures = c(
                                                        "chisq.scaled", "df.scaled",
                                                        "pvalue.scaled", "rmsea.scaled",
                                                        "cfi.scaled", "tli.scaled",
                                                        "srmr"))), digits = 3)
```

Constrain thresholds, loadings, and intercepts to test scalar invariance

``` r
scalar <- as.character(measEq.syntax(configural.model = auth.model,
                                 data = anes2020,
                                 ordered = c("auth1", "auth2", "auth3", "auth4"),
                                 parameterization = "theta",
                                 ID.fac = "std.lv",
                                 ID.cat = "Wu.Estabrook.2016",
                                 group = "female",
                                 group.equal = c("thresholds", "loadings", "intercepts")))
```

Estimate model, see results, and add fit indices to matrix

``` r
fit.scalar <- cfa(model = scalar, data = anes2020, group = "female",
                           ordered = c("auth1", "auth2", "auth3", "auth4"),
                           parameterization = "theta")

summary(fit.scalar)
```

    ## lavaan 0.6.17 ended normally after 75 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        22
    ##   Number of equality constraints                     8
    ## 
    ##   Number of observations per group:               Used       Total
    ##     0                                             3299        3763
    ##     1                                             3935        4450
    ## 
    ## Model Test User Model:
    ##                                               Standard      Scaled
    ##   Test Statistic                                70.199     106.037
    ##   Degrees of freedom                                 6           6
    ##   P-value (Chi-square)                           0.000       0.000
    ##   Scaling correction factor                                  0.663
    ##   Shift parameter                                            0.086
    ##     simple second-order correction                                
    ##   Test statistic for each group:
    ##     0                                           34.019      51.383
    ##     1                                           36.180      54.654
    ## 
    ## Parameter Estimates:
    ## 
    ##   Parameterization                               Theta
    ##   Standard errors                           Robust.sem
    ##   Information                                 Expected
    ##   Information saturated (h1) model        Unstructured
    ## 
    ## 
    ## Group 1 [0]:
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   auth =~                                             
    ##     auth2   (l.1_)    1.491    0.102   14.578    0.000
    ##     auth1   (l.2_)    1.339    0.093   14.413    0.000
    ##     auth3   (l.3_)    1.136    0.062   18.414    0.000
    ##     auth4   (l.4_)    0.702    0.038   18.364    0.000
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .auth2   (nu.1)    0.000                           
    ##    .auth1   (nu.2)    0.000                           
    ##    .auth3   (nu.3)    0.000                           
    ##    .auth4   (nu.4)    0.000                           
    ##     auth    (a.1.)    0.000                           
    ## 
    ## Thresholds:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     ath2|t1 (a2.1)   -0.213    0.038   -5.591    0.000
    ##     ath1|t1 (a1.1)   -0.797    0.053  -15.064    0.000
    ##     ath3|t1 (a3.1)    0.357    0.033   10.849    0.000
    ##     ath4|t1 (a4.1)    0.772    0.032   24.473    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .auth2   (t.1_)    1.000                           
    ##    .auth1   (t.2_)    1.000                           
    ##    .auth3   (t.3_)    1.000                           
    ##    .auth4   (t.4_)    1.000                           
    ##     auth    (p.1_)    1.000                           
    ## 
    ## Scales y*:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     auth2             0.557                           
    ##     auth1             0.598                           
    ##     auth3             0.661                           
    ##     auth4             0.818                           
    ## 
    ## 
    ## Group 2 [1]:
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   auth =~                                             
    ##     auth2   (l.1_)    1.491    0.102   14.578    0.000
    ##     auth1   (l.2_)    1.339    0.093   14.413    0.000
    ##     auth3   (l.3_)    1.136    0.062   18.414    0.000
    ##     auth4   (l.4_)    0.702    0.038   18.364    0.000
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .auth2   (nu.1)    0.000                           
    ##    .auth1   (nu.2)    0.000                           
    ##    .auth3   (nu.3)    0.000                           
    ##    .auth4   (nu.4)    0.000                           
    ##     auth    (a.1.)    0.043    0.029    1.474    0.140
    ## 
    ## Thresholds:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     ath2|t1 (a2.1)   -0.213    0.038   -5.591    0.000
    ##     ath1|t1 (a1.1)   -0.797    0.053  -15.064    0.000
    ##     ath3|t1 (a3.1)    0.357    0.033   10.849    0.000
    ##     ath4|t1 (a4.1)    0.772    0.032   24.473    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .auth2   (t.1_)    0.963    0.198    4.869    0.000
    ##    .auth1   (t.2_)    1.382    0.223    6.187    0.000
    ##    .auth3   (t.3_)    0.675    0.108    6.258    0.000
    ##    .auth4   (t.4_)    0.614    0.064    9.614    0.000
    ##     auth    (p.1_)    0.817    0.073   11.150    0.000
    ## 
    ## Scales y*:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     auth2             0.600                           
    ##     auth1             0.593                           
    ##     auth3             0.761                           
    ##     auth4             0.992

``` r
fit.indices[2,] <- round(data.matrix(fitmeasures(fit.scalar,
                                                      fit.measures = c(
                                                        "chisq.scaled", "df.scaled",
                                                        "pvalue.scaled", "rmsea.scaled",
                                                        "cfi.scaled", "tli.scaled",
                                                        "srmr"))), digits = 3)
```

Additionally constrain residuals to test residual invariance

``` r
residual <- as.character(measEq.syntax(configural.model = auth.model,
                                data = anes2020,
                                ordered = c("auth1", "auth2", "auth3", "auth4"),
                                parameterization = "theta",
                                ID.fac = "std.lv",
                                ID.cat = "Wu.Estabrook.2016",
                                group = "female",
                                group.equal = c("thresholds", "loadings", "intercepts", "residuals")))
```

Estimate model, see results, and add fit indices to matrix

``` r
fit.residual <- cfa(model = residual, data = anes2020, group = "female",
                          ordered = c("auth1", "auth2", "auth3", "auth4"),
                          parameterization = "theta")

summary(fit.residual)
```

    ## lavaan 0.6.17 ended normally after 46 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        18
    ##   Number of equality constraints                     8
    ## 
    ##   Number of observations per group:               Used       Total
    ##     0                                             3299        3763
    ##     1                                             3935        4450
    ## 
    ## Model Test User Model:
    ##                                               Standard      Scaled
    ##   Test Statistic                                96.207     127.043
    ##   Degrees of freedom                                10          10
    ##   P-value (Chi-square)                           0.000       0.000
    ##   Scaling correction factor                                  0.760
    ##   Shift parameter                                            0.426
    ##     simple second-order correction                                
    ##   Test statistic for each group:
    ##     0                                           47.962      63.317
    ##     1                                           48.245      63.727
    ## 
    ## Parameter Estimates:
    ## 
    ##   Parameterization                               Theta
    ##   Standard errors                           Robust.sem
    ##   Information                                 Expected
    ##   Information saturated (h1) model        Unstructured
    ## 
    ## 
    ## Group 1 [0]:
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   auth =~                                             
    ##     auth2   (l.1_)    1.469    0.073   20.142    0.000
    ##     auth1   (l.2_)    1.191    0.057   20.937    0.000
    ##     auth3   (l.3_)    1.223    0.054   22.754    0.000
    ##     auth4   (l.4_)    0.778    0.035   22.456    0.000
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .auth2   (nu.1)    0.000                           
    ##    .auth1   (nu.2)    0.000                           
    ##    .auth3   (nu.3)    0.000                           
    ##    .auth4   (nu.4)    0.000                           
    ##     auth    (a.1.)    0.000                           
    ## 
    ## Thresholds:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     ath2|t1 (a2.1)   -0.250    0.036   -6.973    0.000
    ##     ath1|t1 (a1.1)   -0.753    0.036  -21.023    0.000
    ##     ath3|t1 (a3.1)    0.368    0.031   11.868    0.000
    ##     ath4|t1 (a4.1)    0.861    0.026   33.333    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .auth2   (t.1_)    1.000                           
    ##    .auth1   (t.2_)    1.000                           
    ##    .auth3   (t.3_)    1.000                           
    ##    .auth4   (t.4_)    1.000                           
    ##     auth    (p.1_)    1.000                           
    ## 
    ## Scales y*:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     auth2             0.563                           
    ##     auth1             0.643                           
    ##     auth3             0.633                           
    ##     auth4             0.789                           
    ## 
    ## 
    ## Group 2 [1]:
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   auth =~                                             
    ##     auth2   (l.1_)    1.469    0.073   20.142    0.000
    ##     auth1   (l.2_)    1.191    0.057   20.937    0.000
    ##     auth3   (l.3_)    1.223    0.054   22.754    0.000
    ##     auth4   (l.4_)    0.778    0.035   22.456    0.000
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .auth2   (nu.1)    0.000                           
    ##    .auth1   (nu.2)    0.000                           
    ##    .auth3   (nu.3)    0.000                           
    ##    .auth4   (nu.4)    0.000                           
    ##     auth    (a.1.)    0.002    0.028    0.085    0.932
    ## 
    ## Thresholds:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     ath2|t1 (a2.1)   -0.250    0.036   -6.973    0.000
    ##     ath1|t1 (a1.1)   -0.753    0.036  -21.023    0.000
    ##     ath3|t1 (a3.1)    0.368    0.031   11.868    0.000
    ##     ath4|t1 (a4.1)    0.861    0.026   33.333    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .auth2   (t.1_)    1.000                           
    ##    .auth1   (t.2_)    1.000                           
    ##    .auth3   (t.3_)    1.000                           
    ##    .auth4   (t.4_)    1.000                           
    ##     auth    (p.1_)    0.908    0.064   14.238    0.000
    ## 
    ## Scales y*:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     auth2             0.581                           
    ##     auth1             0.661                           
    ##     auth3             0.651                           
    ##     auth4             0.803

``` r
fit.indices[3,] <- round(data.matrix(fitmeasures(fit.residual,
                                                      fit.measures = c(
                                                        "chisq.scaled", "df.scaled",
                                                        "pvalue.scaled", "rmsea.scaled",
                                                        "cfi.scaled", "tli.scaled",
                                                        "srmr"))), digits = 3)
```

Measurement invariance is rejected if model fit declines sufficiently to
suggest that constraining model parameters to equality is inappropriate.

Likelihood ratio test is a common one to assess fit change
(statistically significant test indicates large decrease in fit).

``` r
lavTestLRT(fit.configural, fit.scalar, fit.residual)
```

    ## 
    ## Scaled Chi-Squared Difference Test (method = "satorra.2000")
    ## 
    ## lavaan NOTE:
    ##     The "Chisq" column contains standard test statistics, not the
    ##     robust test that should be reported per model. A robust difference
    ##     test is a function of two standard (not robust) statistics.
    ##  
    ##                Df AIC BIC  Chisq Chisq diff Df diff Pr(>Chisq)    
    ## fit.configural  4         59.782                                  
    ## fit.scalar      6         70.199     14.096       2  0.0008691 ***
    ## fit.residual   10         96.207     30.647       4  3.614e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Conventional wisdom is to look at changes in other fit measures, not
just the LRT. These are stored in the fit_indices matrix.

Rename rows and columns for clarity and display fit measures:

``` r
colnames(fit.indices) <- c(
  "chisq.scaled", "df.scaled",
  "pvalue.scaled", "rmsea.scaled",
  "cfi.scaled", "tli.scaled",
  "srmr")

rownames(fit.indices) <- c(
  "Configural", "Scalar",
  "Residual")
  
fit.indices
```

    ##            chisq.scaled df.scaled pvalue.scaled rmsea.scaled cfi.scaled
    ## Configural       96.560         4             0        0.080      0.987
    ## Scalar          106.037         6             0        0.068      0.986
    ## Residual        127.043        10             0        0.057      0.984
    ##            tli.scaled  srmr
    ## Configural      0.961 0.044
    ## Scalar          0.972 0.044
    ## Residual        0.980 0.046
