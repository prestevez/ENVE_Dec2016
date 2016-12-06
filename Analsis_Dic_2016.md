---
title: "Repeat extortion of Mexican businesses"
author: "Patricio R. Estevez Soto"
email: "patricio.estevez.14@ucl.ac.uk"
date: "6/12/2016"
output:
  md_document:
    variant: "markdown"
pandoc_args: "--smart"
---



# Introduction

Script to analyze patterns of extortion victimization against Mexican businesses.

# Set up, data input and pre-process

## Session info

We first check details of the session and system, and for reproducibility, we set the random seed.


```r
starttime <- proc.time()
date()
```

```
[1] "Tue Dec  6 17:47:12 2016"
```

```r
sessionInfo()
```

```
R version 3.3.1 (2016-06-21)
Platform: x86_64-apple-darwin13.4.0 (64-bit)
Running under: OS X 10.12.1 (Sierra)

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] classInt_0.1-23  glmmADMB_0.8.3.3 MASS_7.3-45      lme4_1.1-12     
 [5] Matrix_1.2-7.1   texreg_1.36.18   Cairo_1.5-9      ggplot2_2.2.0   
 [9] foreign_0.8-67   rmarkdown_1.2    knitr_1.15.1    

loaded via a namespace (and not attached):
 [1] Rcpp_0.12.8      nloptr_1.0.4     R2admb_0.7.13    plyr_1.8.4      
 [5] highr_0.6        class_7.3-14     tools_3.3.1      digest_0.6.10   
 [9] evaluate_0.10    tibble_1.2       gtable_0.2.0     nlme_3.1-128    
[13] lattice_0.20-34  mgcv_1.8-16      yaml_2.1.14      coda_0.18-1     
[17] e1071_1.6-7      stringr_1.1.0    rprojroot_1.1    grid_3.3.1      
[21] minqa_1.2.4      magrittr_1.5     backports_1.0.4  scales_0.4.1    
[25] codetools_0.2-15 htmltools_0.3.5  splines_3.3.1    assertthat_0.1  
[29] colorspace_1.3-1 labeling_0.3     stringi_1.1.2    lazyeval_0.2.0  
[33] munsell_0.4.3   
```

```r
set.seed(42)
options(scipen=0)
```

## Load packages and functions

Next we load the packages that we will use.


```r
library(foreign)
library(ggplot2)
library(Cairo)
library(knitr)
library(texreg)
library(lme4)
library(glmmADMB)
library(classInt)
```

## Load data


```r
enve_all <- read.dbf("enve2014cuest_ciega_2014.dbf")
cat_entidades <- read.csv("cat_entidades.csv", head=TRUE)
homicidios <- read.csv("homicidios_values.csv", header=TRUE)
homicidios <- merge(homicidios, cat_entidades, by="CVE_ENT", all.x=TRUE)
scode <- read.csv("secode.csv", head=TRUE)
scode$Code <- scode$Code*10000

# Prepare data for analysis
# Selecting only the relevant variables

enve_test <- data.frame(extortions=as.integer(as.character(enve_all$P26_10)))

enve_test$extortion_victim <- enve_all$P25_10
enve_test$extortions[enve_test$extortion_victim == 2] <- 0
summary(enve_test$extortions)
```

```
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.5808  0.0000 33.0000      50 
```

```r
table(enve_test$extortions)
```

```

   0    1    2    3    4    5    6    7    8    9   10   11   12   13   14 
2153  109   46   32   25   19    8    5    4    5    4    2    5    4    6 
  15   19   20   21   22   23   25   27   29   32   33 
   5    2    4    2    1    2    1    1    1    3    1 
```

```r
enve_test$rep_extortion_victim <- factor(enve_test$extortions)
levels(enve_test$rep_extortion_victim) <- c(0, 0,
                    rep(1, length(levels(enve_test$rep_extortion_victim)) - 2))

summary(enve_test$rep_extortion_victim)
```

```
   0    1 NA's 
2262  188   50 
```

```r
enve_test$CVE_UNICA <- as.integer(as.character(enve_all$ID_CONSECU))

enve_test$bribes <- as.integer(as.character(enve_all$P33))
summary(enve_test$bribes)
```

```
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.486   3.000  16.000    2218 
```

```r
# 4 bribe cats
enve_test$bribe1 <- enve_all$P29_1
enve_test$bribe2 <- enve_all$P30_1
enve_test$bribe3 <- enve_all$P31_1
enve_test$bribe4 <- enve_all$P32_1

enve_test$bribes[with(enve_test,
                        bribe1 == 2 &
                        bribe2 == 2 &
                        bribe3 == 2 &
                        bribe4 == 2)] <- 0

summary(enve_test$bribes)
```

```
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.2861  0.0000 16.0000      50 
```

```r
enve_test$bribe_victim <- factor(enve_test$bribes)
levels(enve_test$bribe_victim) <- c(0,
                                    rep(1, length(levels(enve_test$bribe_victim)) - 1))
summary(enve_test$bribe_victim)
```

```
   0    1 NA's 
2168  282   50 
```

```r
enve_test$rep_bribe <- factor(enve_test$bribes)
levels(enve_test$rep_bribe) <- c(0, 0, rep(1,
                                           length(levels(enve_test$rep_bribe)) - 2))
summary(enve_test$rep_bribe)
```

```
   0    1 NA's 
2303  147   50 
```

```r
enve_test$bribe_cats <- factor(enve_test$bribes)
levels(enve_test$bribe_cats) <- c(0, 1, 2, rep("3+",
                                            length(levels(enve_test$bribe_cats)) - 3))
summary(enve_test$bribe_cats)
```

```
   0    1    2   3+ NA's 
2168  135   54   93   50 
```

```r
enve_test$CVE_ENT <- as.integer(as.character(enve_all$CVE_ENT))

enve_test$size <- enve_all$ID_ESTRATO
levels(enve_test$size) <- c("Large", "Medium", "Small", "Micro")

enve_test$sector <- enve_all$SECTOR_FIN

# subsector
enve_test$tempsub <- as.integer(as.character(enve_all$P1_1B))
enve_test$subsector <- cut(enve_test$tempsub, scode$Code, right=FALSE)
levels(enve_test$subsector) <- scode$Sector
enve_test$subsector <- droplevels(enve_test$subsector)
enve_test$subsector <- relevel(enve_test$subsector, ref="Retail")
levels(enve_test$subsector)
```

```
 [1] "Retail"         "Mining"         "Utilities"      "Construction"  
 [5] "Manufacturing"  "Wholesale"      "Transport"      "Media"         
 [9] "Finance"        "Real estate"    "Prof. services" "Corporate"     
[13] "Maintenance"    "Education"      "Health"         "Leisure"       
[17] "HotelsRestBar"  "Other"         
```

```r
enve_test$hotrestbar <- enve_test$subsector
hotindx <- which(levels(enve_test$hotrestbar) == "HotelsRestBar")
levels(enve_test$hotrestbar)[-hotindx] <- 0
levels(enve_test$hotrestbar) <- c(0,1)

enve_test$years <- 2013 - as.numeric(as.character(enve_all$P3))
intyears <- classIntervals(enve_test$years, 5, style="quantile")
enve_test$yearsquant <- cut(enve_test$years, intyears$brks, right=TRUE,
                            include.lowest = TRUE)

enve_test <- merge(enve_test, homicidios, by="CVE_ENT", all.x=TRUE)

length(enve_test$extortions[is.na(enve_test$extortions)])
```

```
[1] 50
```

```r
length(enve_test$bribes[is.na(enve_test$bribes)])
```

```
[1] 50
```

```r
## enve_test$extortions[is.na(enve_test$extortions)] <- 0
## enve_test$bribes[is.na(enve_test$bribes)] <- 0
```

# EDA

## Extoriton EDA


```r
# Distribution of extortion victimisations

ext_dist <- data.frame(table(enve_test$extortions))

colnames(ext_dist) <- c("Events", "Prevalence")

ext_dist$Events<- as.integer(as.character(ext_dist$Events))

ext_dist$Incidence <- ext_dist$Events * ext_dist$Prevalence

ext_dist$preval_per <- prop.table(ext_dist$Prevalence)*100

ext_dist$victim_per[2:length(ext_dist$Events)] <- prop.table(
                                                    ext_dist[2:length(
                                                      ext_dist$Events),2])*100

ext_dist$incid_per <- prop.table(ext_dist$Incidence)*100

ext_dist
```

```
   Events Prevalence Incidence  preval_per victim_per incid_per
1       0       2153         0 87.87755102         NA  0.000000
2       1        109       109  4.44897959 36.7003367  7.659874
3       2         46        92  1.87755102 15.4882155  6.465214
4       3         32        96  1.30612245 10.7744108  6.746311
5       4         25       100  1.02040816  8.4175084  7.027407
6       5         19        95  0.77551020  6.3973064  6.676037
7       6          8        48  0.32653061  2.6936027  3.373155
8       7          5        35  0.20408163  1.6835017  2.459592
9       8          4        32  0.16326531  1.3468013  2.248770
10      9          5        45  0.20408163  1.6835017  3.162333
11     10          4        40  0.16326531  1.3468013  2.810963
12     11          2        22  0.08163265  0.6734007  1.546030
13     12          5        60  0.20408163  1.6835017  4.216444
14     13          4        52  0.16326531  1.3468013  3.654252
15     14          6        84  0.24489796  2.0202020  5.903022
16     15          5        75  0.20408163  1.6835017  5.270555
17     19          2        38  0.08163265  0.6734007  2.670415
18     20          4        80  0.16326531  1.3468013  5.621926
19     21          2        42  0.08163265  0.6734007  2.951511
20     22          1        22  0.04081633  0.3367003  1.546030
21     23          2        46  0.08163265  0.6734007  3.232607
22     25          1        25  0.04081633  0.3367003  1.756852
23     27          1        27  0.04081633  0.3367003  1.897400
24     29          1        29  0.04081633  0.3367003  2.037948
25     32          3        96  0.12244898  1.0101010  6.746311
26     33          1        33  0.04081633  0.3367003  2.319044
```

```r
kable(ext_dist)
```



| Events| Prevalence| Incidence| preval_per| victim_per| incid_per|
|------:|----------:|---------:|----------:|----------:|---------:|
|      0|       2153|         0| 87.8775510|         NA|  0.000000|
|      1|        109|       109|  4.4489796| 36.7003367|  7.659873|
|      2|         46|        92|  1.8775510| 15.4882155|  6.465214|
|      3|         32|        96|  1.3061224| 10.7744108|  6.746311|
|      4|         25|       100|  1.0204082|  8.4175084|  7.027407|
|      5|         19|        95|  0.7755102|  6.3973064|  6.676037|
|      6|          8|        48|  0.3265306|  2.6936027|  3.373155|
|      7|          5|        35|  0.2040816|  1.6835017|  2.459592|
|      8|          4|        32|  0.1632653|  1.3468013|  2.248770|
|      9|          5|        45|  0.2040816|  1.6835017|  3.162333|
|     10|          4|        40|  0.1632653|  1.3468013|  2.810963|
|     11|          2|        22|  0.0816327|  0.6734007|  1.546029|
|     12|          5|        60|  0.2040816|  1.6835017|  4.216444|
|     13|          4|        52|  0.1632653|  1.3468013|  3.654252|
|     14|          6|        84|  0.2448980|  2.0202020|  5.903022|
|     15|          5|        75|  0.2040816|  1.6835017|  5.270555|
|     19|          2|        38|  0.0816327|  0.6734007|  2.670415|
|     20|          4|        80|  0.1632653|  1.3468013|  5.621925|
|     21|          2|        42|  0.0816327|  0.6734007|  2.951511|
|     22|          1|        22|  0.0408163|  0.3367003|  1.546029|
|     23|          2|        46|  0.0816327|  0.6734007|  3.232607|
|     25|          1|        25|  0.0408163|  0.3367003|  1.756852|
|     27|          1|        27|  0.0408163|  0.3367003|  1.897400|
|     29|          1|        29|  0.0408163|  0.3367003|  2.037948|
|     32|          3|        96|  0.1224490|  1.0101010|  6.746311|
|     33|          1|        33|  0.0408163|  0.3367003|  2.319044|

## Bribes EDA


```r
bribes_tab <- data.frame(table(enve_test$bribes))
bribes <- data.frame(Events=bribes_tab$Var1, Freq=bribes_tab$Freq)
colnames(bribes)[1] <- "Events"
bribes$Events <- as.integer(as.character(bribes$Events))
obs_b <- data.frame(Events=0:max(bribes$Events))
bribes <- merge(bribes, obs_b, by="Events", all.y=TRUE)
bribes[is.na(bribes[,2]),2] <- 0

bribes
```

```
   Events Freq
1       0 2168
2       1  135
3       2   54
4       3   34
5       4   21
6       5   11
7       6    8
8       7    7
9       8    4
10      9    2
11     10    3
12     11    1
13     12    0
14     13    1
15     14    0
16     15    0
17     16    1
```

```r
kable(bribes)
```



| Events| Freq|
|------:|----:|
|      0| 2168|
|      1|  135|
|      2|   54|
|      3|   34|
|      4|   21|
|      5|   11|
|      6|    8|
|      7|    7|
|      8|    4|
|      9|    2|
|     10|    3|
|     11|    1|
|     12|    0|
|     13|    1|
|     14|    0|
|     15|    0|
|     16|    1|

## Years vs. extortions plot


```r
### using the raw years variable

cor.ext_years <- with(enve_test, cor.test(extortions, years, method="pearson"))
cor.ext_years
```

```

	Pearson's product-moment correlation

data:  extortions and years
t = 0.33501, df = 2448, p-value = 0.7376
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 -0.03283884  0.04635913
sample estimates:
        cor 
0.006770766 
```

```r
# For raw years number
ggplot(enve_test, aes(x=years, y=extortions)) +
                        geom_point() +
                        geom_smooth(method="lm") +
                        xlab("Years") +
                        ylab("Extortions") +
                        theme_bw()
```

```
Warning: Removed 50 rows containing non-finite values (stat_smooth).
```

```
Warning: Removed 50 rows containing missing values (geom_point).
```

![plot of chunk years](figure/years-1.png)

```r
ggplot(enve_test, aes(x=years, y=extortions)) +
                        geom_point() +
                        geom_smooth(method="lm") +
                        xlab("Years (sqrt scale)") +
                        ylab("Extortions") +
                        theme_bw() +
                        coord_trans(x="sqrt")
```

```
Warning: Removed 50 rows containing non-finite values (stat_smooth).

Warning: Removed 50 rows containing missing values (geom_point).
```

![plot of chunk years](figure/years-2.png)

```r
ggplot(enve_test, aes(x=years, y=extortions)) +
                        geom_point() +
                        geom_smooth(method="lm") +
                        xlab("Years (log scale)") +
                        ylab("Extortions") +
                        theme_bw() +
                        coord_trans(x="log1p")
```

```
Warning: Removed 50 rows containing non-finite values (stat_smooth).

Warning: Removed 50 rows containing missing values (geom_point).
```

![plot of chunk years](figure/years-3.png)

```r
ggplot(enve_test, aes(x=years, y=extortions)) +
                        geom_point() +
                        xlab("Years") +
                        ylab("Extortions") +
                        theme_bw() +
                        geom_density_2d()
```

```
Warning: Removed 50 rows containing non-finite values (stat_density2d).
```

```
Warning: Computation failed in `stat_density2d()`:
bandwidths must be strictly positive
```

```
Warning: Removed 50 rows containing missing values (geom_point).
```

![plot of chunk years](figure/years-4.png)

```r
ggplot(enve_test, aes(x=years, y=extortions)) +
                        geom_point() +
                        xlab("Years (sqrt scale)") +
                        ylab("Extortions") +
                        theme_bw() +
                        coord_trans(x="sqrt") +
                        geom_density_2d()
```

```
Warning: Removed 50 rows containing non-finite values (stat_density2d).
```

```
Warning: Computation failed in `stat_density2d()`:
bandwidths must be strictly positive
```

```
Warning: Removed 50 rows containing missing values (geom_point).
```

![plot of chunk years](figure/years-5.png)

```r
ggplot(enve_test, aes(x=years, y=extortions)) +
                        geom_point() +
                        xlab("Years (log scale)") +
                        ylab("Extortions") +
                        theme_bw() +
                        coord_trans(x="log1p") +
                        geom_density_2d()
```

```
Warning: Removed 50 rows containing non-finite values (stat_density2d).
```

```
Warning: Computation failed in `stat_density2d()`:
bandwidths must be strictly positive
```

```
Warning: Removed 50 rows containing missing values (geom_point).
```

![plot of chunk years](figure/years-6.png)

```r
ggplot(enve_test, aes(x=years, y=extortions)) +
                        geom_point() +
                        geom_smooth() +
                        xlab("Years") +
                        ylab("Extortions") +
                        theme_bw()
```

```
`geom_smooth()` using method = 'gam'
```

```
Warning: Removed 50 rows containing non-finite values (stat_smooth).

Warning: Removed 50 rows containing missing values (geom_point).
```

![plot of chunk years](figure/years-7.png)

```r
ggplot(enve_test, aes(x=years, y=extortions)) +
                        geom_point() +
                        geom_smooth() +
                        xlab("Years (sqrt scale)") +
                        ylab("Extortions") +
                        theme_bw() +
                        coord_trans(x="sqrt")
```

```
`geom_smooth()` using method = 'gam'
```

```
Warning: Removed 50 rows containing non-finite values (stat_smooth).

Warning: Removed 50 rows containing missing values (geom_point).
```

![plot of chunk years](figure/years-8.png)

```r
ggplot(enve_test, aes(x=years, y=extortions)) +
                        geom_point() +
                        geom_smooth() +
                        xlab("Years (log scale)") +
                        ylab("Extortions") +
                        theme_bw() +
                        coord_trans(x="log1p")
```

```
`geom_smooth()` using method = 'gam'
```

```
Warning: Removed 50 rows containing non-finite values (stat_smooth).

Warning: Removed 50 rows containing missing values (geom_point).
```

![plot of chunk years](figure/years-9.png)

```r
ggplot(enve_test, aes(x=years, y=extortions)) +
                        geom_point() +
                        geom_smooth(method="loess") +
                        xlab("Years") +
                        ylab("Extortions") +
                        theme_bw()
```

```
Warning: Removed 50 rows containing non-finite values (stat_smooth).

Warning: Removed 50 rows containing missing values (geom_point).
```

![plot of chunk years](figure/years-10.png)

```r
ggplot(enve_test, aes(x=years, y=extortions)) +
                        geom_point() +
                        geom_smooth(method="loess") +
                        xlab("Years (sqrt scale)") +
                        ylab("Extortions") +
                        theme_bw() +
                        coord_trans(x="sqrt")
```

```
Warning: Removed 50 rows containing non-finite values (stat_smooth).

Warning: Removed 50 rows containing missing values (geom_point).
```

![plot of chunk years](figure/years-11.png)

```r
ggplot(enve_test, aes(x=years, y=extortions)) +
                        geom_point() +
                        geom_smooth(method="loess") +
                        xlab("Years (log scale)") +
                        ylab("Extortions") +
                        theme_bw() +
                        coord_trans(x="log1p")
```

```
Warning: Removed 50 rows containing non-finite values (stat_smooth).

Warning: Removed 50 rows containing missing values (geom_point).
```

![plot of chunk years](figure/years-12.png)


# Model


```r
# Negative Binomial GLMM

m <- glmmadmb(extortions ~ bribes + tasahom + yearsquant +
                subsector + size + (1 | NOM_ABR), data=enve_test,
                 family="nbinom", zeroInflation=FALSE,
                 extra.args="-ndi 60000", admb.opts = admbControl(noinit = FALSE), na.action = na.omit)
```

```
Error in glmmadmb(extortions ~ bribes + tasahom + yearsquant + subsector + : unused argument (na.action = na.omit)
```

```r
summary(m)
```

```
Error in summary(m): object 'm' not found
```

Using lme4 instead of glmmadmb


```r
m_lme4 <- glmer.nb(extortions ~ bribes + tasahom + yearsquant +
                subsector + size + (1 | NOM_ABR), data=enve_test,
                na.action = na.omit)
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : Model failed to converge with max|grad| = 0.00277559 (tol =
0.001, component 1)
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: large eigenvalue ratio
 - Rescale variables?
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : unable to evaluate scaled gradient
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : Hessian is numerically singular: parameters are not uniquely
determined
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : unable to evaluate scaled gradient
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : Hessian is numerically singular: parameters are not uniquely
determined
```

```
Warning in (function (fn, par, lower = rep.int(-Inf, n), upper =
rep.int(Inf, : failure to converge in 10000 evaluations
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : unable to evaluate scaled gradient
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : Model failed to converge: degenerate Hessian with 1 negative
eigenvalues
```

```
Warning in (function (fn, par, lower = rep.int(-Inf, n), upper =
rep.int(Inf, : failure to converge in 10000 evaluations
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : unable to evaluate scaled gradient
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : Hessian is numerically singular: parameters are not uniquely
determined
```

```
Warning in (function (fn, par, lower = rep.int(-Inf, n), upper =
rep.int(Inf, : failure to converge in 10000 evaluations
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : unable to evaluate scaled gradient
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : Hessian is numerically singular: parameters are not uniquely
determined
```

```
Warning in (function (fn, par, lower = rep.int(-Inf, n), upper =
rep.int(Inf, : failure to converge in 10000 evaluations
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : unable to evaluate scaled gradient
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : Hessian is numerically singular: parameters are not uniquely
determined
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : unable to evaluate scaled gradient
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : Hessian is numerically singular: parameters are not uniquely
determined
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : unable to evaluate scaled gradient
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : Hessian is numerically singular: parameters are not uniquely
determined
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : unable to evaluate scaled gradient
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : Hessian is numerically singular: parameters are not uniquely
determined
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : unable to evaluate scaled gradient
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : Hessian is numerically singular: parameters are not uniquely
determined
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : unable to evaluate scaled gradient
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : Hessian is numerically singular: parameters are not uniquely
determined
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : unable to evaluate scaled gradient
```

```
Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
$checkConv, : Hessian is numerically singular: parameters are not uniquely
determined
```

```r
summary(m_lme4)
```

```
Warning in vcov.merMod(object, use.hessian = use.hessian): variance-covariance matrix computed from finite-difference Hessian is
not positive definite or contains NA values: falling back to var-cov estimated from RX
```

```
Warning in vcov.merMod(object, correlation = correlation, sigm = sig): variance-covariance matrix computed from finite-difference Hessian is
not positive definite or contains NA values: falling back to var-cov estimated from RX
```

```
Generalized linear mixed model fit by maximum likelihood (Laplace
  Approximation) [glmerMod]
 Family: Negative Binomial(0.0552)  ( log )
Formula: extortions ~ bribes + tasahom + yearsquant + subsector + size +  
    (1 | NOM_ABR)
   Data: enve_test

     AIC      BIC   logLik deviance df.resid 
  3193.9   3361.7  -1568.0   3135.9     2372 

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-0.2330 -0.2258 -0.2226 -0.2177 14.3362 

Random effects:
 Groups  Name        Variance  Std.Dev. 
 NOM_ABR (Intercept) 1.435e-12 1.198e-06
Number of obs: 2401, groups:  NOM_ABR, 32

Fixed effects:
                          Estimate Std. Error z value Pr(>|z|)  
(Intercept)             -2.459e-02  5.773e-01  -0.043   0.9660  
bribes                   2.319e-02  8.316e-02   0.279   0.7804  
tasahom                  6.758e-03  7.375e-03   0.916   0.3595  
yearsquant(8,16]         1.266e-01  2.914e-01   0.434   0.6639  
yearsquant(16,25]       -1.919e-01  2.865e-01  -0.670   0.5029  
yearsquant(25,34]        3.052e-01  2.802e-01   1.089   0.2761  
yearsquant(34,43]        8.675e-02  2.923e-01   0.297   0.7666  
subsectorMining         -1.417e+00  1.061e+00  -1.335   0.1818  
subsectorUtilities      -8.641e-02  8.947e-01  -0.097   0.9231  
subsectorConstruction   -8.742e-02  5.598e-01  -0.156   0.8759  
subsectorManufacturing  -4.197e-01  5.462e-01  -0.768   0.4423  
subsectorWholesale      -7.330e-01  6.686e-01  -1.096   0.2730  
subsectorTransport      -7.216e-01  6.472e-01  -1.115   0.2649  
subsectorMedia           1.481e-01  8.396e-01   0.176   0.8600  
subsectorFinance        -1.312e-01  8.942e-01  -0.147   0.8834  
subsectorReal estate    -1.260e+00  9.500e-01  -1.326   0.1848  
subsectorProf. services  1.008e+00  8.803e-01   1.146   0.2520  
subsectorCorporate      -4.884e-01  8.188e-01  -0.597   0.5508  
subsectorMaintenance    -7.612e-01  6.008e-01  -1.267   0.2052  
subsectorEducation      -1.948e-03  8.911e-01  -0.002   0.9983  
subsectorHealth         -6.329e-01  5.577e-01  -1.135   0.2564  
subsectorLeisure         2.343e-01  8.736e-01   0.268   0.7885  
subsectorHotelsRestBar  -5.332e-01  5.600e-01  -0.952   0.3410  
subsectorOther          -3.308e+01  5.821e+06   0.000   1.0000  
sizeMedium              -2.478e-01  2.613e-01  -0.948   0.3430  
sizeSmall               -4.107e-01  2.647e-01  -1.552   0.1208  
sizeMicro               -6.648e-01  2.607e-01  -2.550   0.0108 *
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```

Correlation matrix not shown by default, as p = 27 > 12.
Use print(x, correlation=TRUE)  or
	 vcov(x)	 if you need it
```

```
convergence code: 0
unable to evaluate scaled gradient
 Hessian is numerically singular: parameters are not uniquely determined
```

```r
screenreg(list(m,m_lme4))
```

```
Error in match(x, table, nomatch = 0L): object 'm' not found
```

# Benchmark stats


```r
endtime <- proc.time()
time <- endtime - starttime
time
```

```
   user  system elapsed 
484.574   5.540 499.274 
```

```r
print(paste("the script took", round(time[3]/60,2),
              "minutes to run.", sep=" "))
```

```
[1] "the script took 8.32 minutes to run."
```
