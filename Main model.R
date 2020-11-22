#####################################################################################################
#### Role of air temperature and humidity in the transmission of SARS-CoV-2 in the United States ####
#### Cleaned R code for the main model                                                           ####
#### R codes for other parts of the statistical analyses will be uploaded soon                   ####
#### Yiqun Ma, Nov. 2020                                                                         ####
#### Yale School of Public Health                                                                ####
#### New Haven, CT                                                                               ####
#####################################################################################################

library(dlnm) ;library(splines) ; library(tsModel);library(mgcv);library(dplyr)

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

#### 2. run the model
### Air temperature
Tmeancb.gamm <- gamm(Rt_50 ~ te(as.numeric(date), Longitude,Latitude, bs=c("cr","tp"), d=c(1,2), k=c(30, 30)) +
                       cb.Tmean + 
                       popdensity + pct_blk + hispanic + cen.60plus + medhouseholdincome + pct_owner_occ + education + icu_10K,
                     data=data.all, family=quasi(link="log"), random = list(GEOID=~1))
### Specific humidity
AHcb.gamm <- gamm(Rt_50 ~ te(as.numeric(date), Longitude,Latitude, bs=c("cr","tp"), d=c(1,2), k=c(30, 30)) +
                    cb.AH + 
                    popdensity + pct_blk + hispanic + cen.60plus + medhouseholdincome + pct_owner_occ + education + icu_10K,
                  data=data.all, family=quasi(link="log"), random = list(GEOID=~1))


