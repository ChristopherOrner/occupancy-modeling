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

# create an empty list to then slot the unmarked frames into
all_species_umf <-list(NULL)

# load the data in the format the model uses
for (i in 1:length(present_enough_list)){
     umf <- unmarkedMultFrame(               
               y=present_enough_list[[i]],        # dectection history
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
     
     # slot the unmarked frame into a new list
     all_species_umf[[i]] <- umf
}

# names the list of unmarked frames so I know which belows to which species
names(all_species_umf) <- names(present_enough_list)

# This one below assumes that all are constant and is a good test/counterfactual to
#    to models that I try out. If this preforms as good or better than my models, 
#    then my models don't do good job to capture how variables impact occupancy
#    or suggest these variable and occupany might not be related strongly.

# make an empty list to slot the models
umf_models <- list(NULL)

# loop through all the models for each of the species
for (i in 1:length(all_species_umf)){
     
     m0 <- colext(psiformula = ~ 1, 
                  gammaformula = ~ 1, 
                  epsilonformula = ~ 1, 
                  pformula = ~ 1,
                  data = all_species_umf[[i]])
     
     m1 <- colext(psiformula = ~ 1, 
                  gammaformula = ~ 1, 
                  epsilonformula = ~ 1, 
                  pformula = ~ Cloud,
                  data = all_species_umf[[i]])
     
     m2 <- colext(psiformula = ~ 1, 
                  gammaformula = ~ 1, 
                  epsilonformula = ~ 1, 
                  pformula = ~ Date,
                  data = all_species_umf[[i]])
     
     m3 <- colext(psiformula = ~ 1, 
                  gammaformula = ~ 1, 
                  epsilonformula = ~ 1, 
                  pformula = ~ Obsvr,
                  data = all_species_umf[[i]])
     
     m4 <- colext(psiformula = ~ 1, 
                  gammaformula = ~ 1, 
                  epsilonformula = ~ 1, 
                  pformula = ~ Sky,
                  data = all_species_umf[[i]])
     
     m5 <- colext(psiformula = ~ 1, 
                  gammaformula = ~ 1, 
                  epsilonformula = ~ 1, 
                  pformula = ~ Temp,
                  data = all_species_umf[[i]])
     
     m6 <- colext(psiformula = ~ 1, 
                  gammaformula = ~ 1, 
                  epsilonformula = ~ 1, 
                  pformula = ~ Time,
                  data = all_species_umf[[i]])
     
     m7 <- colext(psiformula = ~ 1, 
                  gammaformula = ~ 1, 
                  epsilonformula = ~ 1, 
                  pformula = ~ Wind,
                  data = all_species_umf[[i]])
     
     # put all the models for one speices into one list item (become list of 
     #    lists), so they're clustered by species. Also name the models so they're
     #    easier to keep track of.
     umf_models[[i]] <- list(basic=m0, Cloud=m1, Date=m2, Obsvr=m3,
                             Sky=m4, Temp=m5, Time=m6, Wind=m7)
}

# name list by species to keep track of the lists grouped by species
names(umf_models) <- names(all_species_umf)

# I can go in a view each model summary by using the first double brackets to go 
#    to the species I'm interested in and the second double brackets to go to the 
#    model I'm interested in.
summary(umf_models[["CAQU"]][[7]])
