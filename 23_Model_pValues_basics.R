# MODELING ########################################################################
# Evently get to using the colext, multi-seaon model
#    psiformula     = regression model (logistic?) for First-year occupancy
#    gammaformula   = regression model to estimate colonization rate
#    epsilonformula = regression model to estimate local extinction (1-survival)
#                     rate 
#    pformula       = regression to model detection probability

# load the unmarked library
library(unmarked)

# load the data in the format the model uses - this is for CAQU
     umf_CAQU <- unmarkedMultFrame(               
               y=present_enough_list[[1]],        # dectection history
               siteCovs=first_occup_covar,        # vary between sites but not years
               yearlySiteCovs=list(Years=years_m, # varies between years at each site
                                   Rain=precip_m),
               obsCovs=list(Cloud=cloud_m,        # varies between surveys
                            Date=date_m,
                            Obsvr=obsvr_m,
                            Sky=sky_m,
                            Temp=temps_m,
                            Time=time_m,
                            Wind=wind_m),        
               numPrimary=6)                      # number of years of data


# run the model with all variables
     m0_CAQU <- colext(psiformula = ~ Creek+Elevation+Green_Veg, 
                  gammaformula = ~ (Years-1)+Rain, 
                  epsilonformula = ~ (Years-1)+Rain, 
                  pformula = ~ Cloud+Date+Obsvr+Sky+Temp+Time+Wind,
                  data = umf_CAQU)
     
     m0_CAQU_nogreen <- colext(psiformula = ~ Creek+Elevation, 
                       gammaformula = ~ (Years-1)+Rain, 
                       epsilonformula = ~ (Years-1)+Rain, 
                       pformula = ~ Cloud+Date+Obsvr+Sky+Temp+Time+Wind,
                       data = umf_CAQU)
     
     m0_CAQU_noobsvr <- colext(psiformula = ~ Creek+Elevation+Green_Veg, 
                       gammaformula = ~ (Years-1)+Rain, 
                       epsilonformula = ~ (Years-1)+Rain, 
                       pformula = ~ Cloud+Date+Sky+Temp+Time+Wind,
                       data = umf_CAQU)
     
     

# check out the results
show(m0_CAQU)

# Likelihood Ratio Test - looks at signification of variables across all cagetories of a 
#    variable, e.g. observer. I should use this to evaluate all my variables because I 
#    I have a good chuck that I have to compare this way including cloud, sky, etc. 

stats_stack[[1]][3,] <- unlist(LRT(m1=m0_CAQU,m2=m0_CAQU_nogreen))

LRT(m1=m0_CAQU,m2=m0_CAQU_noobsvr)