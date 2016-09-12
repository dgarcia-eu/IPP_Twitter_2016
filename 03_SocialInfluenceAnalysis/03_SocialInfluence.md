# Social Impact in Twitter
David Garcia  
18.08.2016  


```r
download.file("https://www.sg.ethz.ch/media/medialibrary/2016/09/03_IPP_SocialInfluence.zip", destfile="03_Social_Influence.zip")
unzip("03_Social_Influence.zip", exdir = "./") 
file.remove("03_Social_Influence.zip")
```



```r
library(arm)
library(texreg)

d <- read.table("SIdata.dat", header=T, sep="\t")
f <- d$avgRT > 0
d <- data.frame(meanscore=d$meanscore[f], score=d$score[f], avgRT=d$avgRT[f], followers=d$followers[f], incore=d$incore[f])

m1 <- bayesglm(log(d$avgRT) ~ log(d$incore+1) * log(d$followers+1)) 
m2 <- bayesglm(log(d$score) ~ log(d$incore+1) * log(d$followers+1)) 
m3 <- bayesglm(log(d$meanscore) ~ log(d$incore+1) * log(d$followers+1)) 

screenreg(list(m1,m2,m3), digits=4)
```

```
## 
## ======================================================================================
##                                         Model 1          Model 2         Model 3      
## --------------------------------------------------------------------------------------
## (Intercept)                                  2.1263 ***      3.2217 ***     3.5291 ***
##                                             (0.1385)        (0.0190)       (0.0166)   
## log(d$incore + 1)                           -0.3889 ***     -0.0632 ***     0.0707 ***
##                                             (0.1052)        (0.0144)       (0.0120)   
## log(d$followers + 1)                         0.6614 ***      0.1382 ***    -0.0342 ** 
##                                             (0.0982)        (0.0135)       (0.0113)   
## log(d$incore + 1):log(d$followers + 1)      -0.0609 ***      0.0021         0.0005    
##                                             (0.0099)        (0.0014)       (0.0011)   
## --------------------------------------------------------------------------------------
## AIC                                      31939.4027       4955.9694      1874.6203    
## BIC                                      31973.5157       4990.0810      1908.2267    
## Log Likelihood                          -15964.7013      -2472.9847      -932.3102    
## Deviance                                 43911.3198        823.4551       486.6180    
## Num. obs.                                 6786            6784           6132         
## ======================================================================================
## *** p < 0.001, ** p < 0.01, * p < 0.05
```


```r
library(lmtest)

m1 <- bayesglm(log(d$avgRT) ~ log(d$followers+1))                                       
m2 <- bayesglm(log(d$avgRT) ~ log(d$incore+1))     
lrtest(m1,m2)
```

```
## Likelihood ratio test
## 
## Model 1: log(d$avgRT) ~ log(d$followers + 1)
## Model 2: log(d$avgRT) ~ log(d$incore + 1)
##   #Df LogLik Df  Chisq Pr(>Chisq)    
## 1   3 -15996                         
## 2   3 -15992  0 8.0792  < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
m1 <- bayesglm(log(d$meanscore) ~ log(d$followers+1))                                       
m2 <- bayesglm(log(d$meanscore) ~ log(d$incore+1))     
lrtest(m1,m2)
```

```
## Likelihood ratio test
## 
## Model 1: log(d$meanscore) ~ log(d$followers + 1)
## Model 2: log(d$meanscore) ~ log(d$incore + 1)
##   #Df  LogLik Df  Chisq Pr(>Chisq)    
## 1   3 -950.92                         
## 2   3 -937.90  0 26.028  < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

























