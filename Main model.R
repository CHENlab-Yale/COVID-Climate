#####################################################################################################
#### Role of meteorological factors in the transmission of SARS-CoV-2 in the United States       ####
#### Cleaned R code for the main model                                                           ####
#### Yiqun Ma, Sen Pei, Jeffrey Shaman, Robert Dubrow, Kai Chen                                  ####                                        ####
#### Yale School of Public Health, New Haven, CT                                                 ####
#### Mailman School of Public Health, Columbia University, New York, NY                          ####
#### May 16, 2021                                                                                ####
#####################################################################################################

library(dlnm) ;library(splines) ; library(tsModel);library(mgcv);library(dplyr); library(parallel)

#### 1. define crossbasis
### exposure function
varfun = "ns"
vardf=3
argvar <- list(fun=varfun, df=vardf)

### lag function
lag <- 13
lagfun="ns"
lagdf=3

### crossbasis: air temperature
cb.Tmean <- crossbasis(data.all$TMP,lag=lag,argvar=argvar,
                       arglag = list(fun=lagfun, df=lagdf))

### crossbasis: specific humidity
cb.AH <- crossbasis(data.all$SPFH,lag=lag,argvar=argvar,
                    arglag = list(fun=lagfun, df=lagdf))

### crossbasis: ultraviolet radiation
cb.UV <- crossbasis(data.all$UV,lag=lag,argvar=argvar,
                    arglag = list(fun=lagfun, df=lagdf))

#### 2. run the model
cb.gamm <- bam(Rt_log ~ te(as.numeric(date), Latitude, Longitude, bs=c("cr","tp"), d=c(1,2), k=c(30, 200)) +
                 cb.Tmean +
                 cb.AH + 
                 cb.UV +
                 popdensity + pct_blk + hispanic + cen.60plus + medhouseholdincome + pct_owner_occ + education + icu_10K + healthcare_prop +
                 as.numeric(date.100pop) +
                 s(GEOID,bs="re")+s(STATE, bs="re"),
               data=data.all, family=gaussian,
               method = "REML", cluster=makeCluster(10))

