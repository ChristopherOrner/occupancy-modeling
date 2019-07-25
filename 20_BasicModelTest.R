# STATIC VARIABLES ###############################################################
# 
# Next I'll need to add in variables like elevation, stream order, watershed size
#    that won't ever change, so I just need to add them once add they'll be the 
#    same for each LL_PP - it'll just be one 60 (or 75) lenght vector for each
#    variable.
# The output from ArcMap is an excel table, so I can convert it into a CSV to read
#    it in. 
# I also need to have the LL_PP line up 

# MODELING ########################################################################
# Evently get to using the colext, multi-seaon model
#    psiformula     = regression model (logistic?) for First-year occupancy
#    gammaformula   = regression model to estimate colonization rate
#    epsilonformula = regression model to estimate local extinction (1-survival)
#                     rate 
#    pformula       = regression to model detection probability

# load the unmarked library
library(unmarked)

# load the data in the format the model uses
umf <- unmarkedMultFrame(
               y=present_enough_list[[1]],        # dectection history
               siteCovs=first_occup_covar,  # vary between sites but not years
               yearlySiteCovs=list(Years=years_m, # varies between years at each site
                                   Rain=precip_m),
               obsCovs=list(Cloud=cloud_m,        # varies between surveys
                            Date=date_m,
                            Obsvr=obsvr_m,
                            Sky=sky_m,
                            Temp=temps_m,
                            Time=time_m,
                            Wind=wind_m),        
               numPrimary=6)                 # number of years of data

# This one below assumes that all are constant and is a good test/counterfactual to
#    to models that I try out. If this preforms as good or better than my models, 
#    then my models don't do good job to capture how variables impact occupancy
#    or suggest these variable and occupany might not be related strongly.

 m0 <- colext(psiformula = ~1, 
              gammaformula = ~ 1, 
              epsilonformula = ~ 1, 
              pformula = ~ 1,
              data = umf)
 
 m1<- colext(psiformula = ~1, 
              gammaformula = ~ 1, 
              epsilonformula = ~ 1, 
              pformula = ~ Date,
              data = umf)
 
 m2 <- colext(psiformula = ~1, 
              gammaformula = ~ 1, 
              epsilonformula = ~ 1, 
              pformula = ~ Cloud,
              data = umf)

 m3 <- colext(psiformula = ~1, 
              gammaformula = ~ 1, 
              epsilonformula = ~ 1, 
              pformula = ~ Cloud+Date,
              data = umf)

 m4 <- colext(psiformula = ~ Slope_Mean, 
              gammaformula = ~ (Years-1)+Rain, 
              epsilonformula = ~ 1, 
              pformula = ~ Cloud+Date+Sky+Temp+Time+Wind,
              data = umf)
 
 m5 <- colext(psiformula = ~ Creek+Elevation+Slope_Mean, 
              gammaformula = ~ Rain, 
              epsilonformula = ~ Rain, 
              pformula = ~ Cloud+Date+Sky+Temp+Time+Wind,
              data = umf)
 
summary(m0)
summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)

rm(m0,m1,m2,m3,m4,m5)
