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
     
     m0 <- colext(psiformula = ~ Creek+Elevation+Green_Veg, 
                  gammaformula = ~ (Years-1)+Rain, 
                  epsilonformula = ~ (Years-1)+Rain, 
                  pformula = ~ Cloud+Date+Obsvr+Sky+Temp+Time+Wind,
                  data = all_species_umf[[i]])
     
     umf_models[[i]] <- m0
}
# name list by species to keep track of the lists grouped by species
names(umf_models) <- names(all_species_umf)
projected(umf_models[["CAQU"]])
projected(umf_models[["MODO"]])
projected(umf_models[["HOWR"]])
projected(umf_models[["ATFL"]])

######################################################################
# pulling something out for the thesis presentation
# get the predicted occupancy values
fitted_ATFL <- fitted(umf_models[["ATFL"]])
predict(umf_models[["ATFL"]], type = "")

fitted_ATFL_avg <- matrix(nrow = 60, ncol = 6)

for (i in 1:6){
     fitted_ATFL_avg[,i] <- apply(fitted_ATFL[,(3*i-2):(3*i)], MARGIN = 1, mean)
}

# name the matrix rows by the location & site they're from
rownames(fitted_ATFL_avg) <- l_p

# name the matrix columns by the year & visit they're from
years <- 2013:2018
occ_y <- paste0("occ_", years)
colnames(fitted_ATFL_avg) <- occ_y

write.csv(fitted_ATFL_avg, "output/ATFL_OccProb.csv")

# loop for a bunch
fitted_avg <- matrix(nrow = 60, ncol = 6*21)

for (i in 1:21){
     fitted_species <- fitted(umf_models[[i]])
     for (j in 1:6){
          fitted_avg[,(6*i-(6-j))] <- apply(fitted_species[,(3*j-2):(3*j)], 
                                                 MARGIN = 1, mean)
     }
}

fitted_avg_sum <- matrix(nrow = 60, ncol = 6)
for (i in 1:6){
     fitted_avg_sum[,i] <- fitted_avg[,i]+fitted_avg[,i+6]+fitted_avg[,i+12]+
                              fitted_avg[,i+18]+fitted_avg[,i+24]+fitted_avg[,i+30]+
                              fitted_avg[,i+36]+fitted_avg[,i+42]+fitted_avg[,i+48]+
                              fitted_avg[,i+54]+fitted_avg[,i+60]+fitted_avg[,i+66]+
                              fitted_avg[,i+72]+fitted_avg[,i+78]+fitted_avg[,i+84]+
                              fitted_avg[,i+90]+fitted_avg[,i+96]+fitted_avg[,i+102]+
                              fitted_avg[,i+108]+fitted_avg[,i+114]+fitted_avg[,i+120]
}

rownames(fitted_avg_sum) <- l_p

# name the matrix columns by the year & visit they're from
years <- 2013:2018
occ_y <- paste0("occ_", years)
colnames(fitted_avg_sum) <- occ_y
write.csv(fitted_avg_sum, "output/Sum_OccProb.csv")

######################################################################


### Withouth clouds, skys, and wind
### 
# make an empty list to slot the models
umf_models1 <- list(NULL)

for (i in 1:length(all_species_umf)){
     
     m1 <- colext(psiformula = ~ Creek+Elevation+Green_Veg, 
                  gammaformula = ~ (Years-1)+Rain, 
                  epsilonformula = ~ (Years-1)+Rain, 
                  pformula = ~ Date+Obsvr+Temp+Time,
                  data = all_species_umf[[i]])
     
     umf_models1[[i]] <- m1
}

# name list by species to keep track of the lists grouped by species
names(umf_models1) <- names(all_species_umf)

# I can go in a view each model summary by using the first double brackets to go 
#    to the species I'm interested in and the second double brackets to go to the 
#    model I'm interested in.
summary_MODO <- summary(umf_models1[["MODO"]])
