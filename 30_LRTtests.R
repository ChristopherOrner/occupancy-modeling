###############################################################################
# This code calculates the Likelihood Ratio Test statistics for differnt 
#    variables by running models that don't include the covariate.
###############################################################################

# load the unmarked library
library(unmarked)

### First, I need to load the data in the proper format, an unmarkedMultiFrame.
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

### Second I need to run the models. One model with all variables and then one 
###       model without each of the covariates of interest. I'm mostly interested
###       in the detection covariates and occupancy covariates. I think I'll code
###       up the model to do it for colonization and extinction too, but those 
###       aren't of focus for me right now.

     # make empty lists to slot the models
     umf_model_all <- list(NULL)
          # first year occupancy covariates
          umf_model_noCreek <- list(NULL)
          umf_model_noElev <- list(NULL)
          umf_model_noVeg <- list(NULL)
          # colonization covariates
          umf_model_col_noYear <- list(NULL)
          umf_model_col_noRain <- list(NULL)
          # extinction 
          umf_model_ext_noYear <- list(NULL)
          umf_model_ext_noRain <- list(NULL)
          # detection covariates
          umf_model_noCloud <- list(NULL)
          umf_model_noDate <- list(NULL)
          umf_model_noObsvr <- list(NULL)
          umf_model_noSky <- list(NULL)
          umf_model_noTemp <- list(NULL)
          umf_model_noTime <- list(NULL)
          umf_model_noWind <- list(NULL)
     
     # loop through all the models for each of the species
     for (i in 1:length(all_species_umf)){
          
          m0 <- colext(psiformula = ~ Creek+Elevation+Green_Veg, 
                       gammaformula = ~ (Years-1)+Rain, 
                       epsilonformula = ~ (Years-1)+Rain, 
                       pformula = ~ Cloud+Date+Obsvr+Sky+Temp+Time+Wind,
                       data = all_species_umf[[i]])
          
          umf_model_all[[i]] <- m0
          
          # first year occupancy covariates
               # no Creek
               noCreek <- colext(psiformula = ~ Elevation+Green_Veg, 
                            gammaformula = ~ (Years-1)+Rain, 
                            epsilonformula = ~ (Years-1)+Rain, 
                            pformula = ~ Cloud+Date+Obsvr+Sky+Temp+Time+Wind,
                            data = all_species_umf[[i]])
               umf_model_noCreek[[i]] <- noCreek
               
               # no Elevation
               noElev <- colext(psiformula = ~ Creek+Green_Veg, 
                            gammaformula = ~ (Years-1)+Rain, 
                            epsilonformula = ~ (Years-1)+Rain, 
                            pformula = ~ Cloud+Date+Obsvr+Sky+Temp+Time+Wind,
                            data = all_species_umf[[i]])
               umf_model_noElev[[i]] <- noElev
               
               # no Green_Veg
               noVeg <- colext(psiformula = ~ Creek+Elevation, 
                            gammaformula = ~ (Years-1)+Rain, 
                            epsilonformula = ~ (Years-1)+Rain, 
                            pformula = ~ Cloud+Date+Obsvr+Sky+Temp+Time+Wind,
                            data = all_species_umf[[i]])
               umf_model_noVeg[[i]] <- noVeg
               
          # colonization covariates
               # no Year
               col_noYear <- colext(psiformula = ~ Creek+Elevation+Green_Veg, 
                            gammaformula = ~ Rain, 
                            epsilonformula = ~ (Years-1)+Rain, 
                            pformula = ~ Cloud+Date+Obsvr+Sky+Temp+Time+Wind,
                            data = all_species_umf[[i]])
               umf_model_col_noYear[[i]] <- col_noYear
               
               # no Rain
               col_noRain <- colext(psiformula = ~ Creek+Elevation+Green_Veg, 
                            gammaformula = ~ (Years-1), 
                            epsilonformula = ~ (Years-1)+Rain, 
                            pformula = ~ Cloud+Date+Obsvr+Sky+Temp+Time+Wind,
                            data = all_species_umf[[i]])
               umf_model_col_noRain[[i]] <- col_noRain
               
          # extinction 
               # no Year
               ext_noYear <- colext(psiformula = ~ Creek+Elevation+Green_Veg, 
                            gammaformula = ~ (Years-1)+Rain, 
                            epsilonformula = ~ Rain, 
                            pformula = ~ Cloud+Date+Obsvr+Sky+Temp+Time+Wind,
                            data = all_species_umf[[i]])
               umf_model_ext_noYear[[i]] <- ext_noYear
               
               # no Rain
               ext_noRain <- colext(psiformula = ~ Creek+Elevation+Green_Veg, 
                            gammaformula = ~ (Years-1)+Rain, 
                            epsilonformula = ~ (Years-1), 
                            pformula = ~ Cloud+Date+Obsvr+Sky+Temp+Time+Wind,
                            data = all_species_umf[[i]])
               umf_model_ext_noRain[[i]] <- ext_noRain
               
          # detection covariates
               # no Cloud
               noCloud<- colext(psiformula = ~ Creek+Elevation+Green_Veg, 
                            gammaformula = ~ (Years-1)+Rain, 
                            epsilonformula = ~ (Years-1)+Rain, 
                            pformula = ~ Date+Obsvr+Sky+Temp+Time+Wind,
                            data = all_species_umf[[i]])
               umf_model_noCloud[[i]] <- noCloud
               
               # no Date
               noDate <- colext(psiformula = ~ Creek+Elevation+Green_Veg, 
                            gammaformula = ~ (Years-1)+Rain, 
                            epsilonformula = ~ (Years-1)+Rain, 
                            pformula = ~ Cloud+Obsvr+Sky+Temp+Time+Wind,
                            data = all_species_umf[[i]])
               umf_model_noDate[[i]] <- noDate
               
               # no Observer
               noObsvr <- colext(psiformula = ~ Creek+Elevation+Green_Veg, 
                            gammaformula = ~ (Years-1)+Rain, 
                            epsilonformula = ~ (Years-1)+Rain, 
                            pformula = ~ Cloud+Date+Sky+Temp+Time+Wind,
                            data = all_species_umf[[i]])
               umf_model_noObsvr[[i]] <- noObsvr
               
               # no Sky
               noSky <- colext(psiformula = ~ Creek+Elevation+Green_Veg, 
                            gammaformula = ~ (Years-1)+Rain, 
                            epsilonformula = ~ (Years-1)+Rain, 
                            pformula = ~ Cloud+Date+Obsvr+Temp+Time+Wind,
                            data = all_species_umf[[i]])
               umf_model_noSky[[i]] <- noSky
               
               # no Temp
               noTemp <- colext(psiformula = ~ Creek+Elevation+Green_Veg, 
                            gammaformula = ~ (Years-1)+Rain, 
                            epsilonformula = ~ (Years-1)+Rain, 
                            pformula = ~ Cloud+Date+Obsvr+Sky+Time+Wind,
                            data = all_species_umf[[i]])
               umf_model_noTemp[[i]] <- noTemp
               
               # no Time
               noTime <- colext(psiformula = ~ Creek+Elevation+Green_Veg, 
                            gammaformula = ~ (Years-1)+Rain, 
                            epsilonformula = ~ (Years-1)+Rain, 
                            pformula = ~ Cloud+Date+Obsvr+Sky+Temp+Wind,
                            data = all_species_umf[[i]])
               umf_model_noTime[[i]] <- noTime
               
               # no Wind
               noWind <- colext(psiformula = ~ Creek+Elevation+Green_Veg, 
                            gammaformula = ~ (Years-1)+Rain, 
                            epsilonformula = ~ (Years-1)+Rain, 
                            pformula = ~ Cloud+Date+Obsvr+Sky+Temp+Time,
                            data = all_species_umf[[i]])
               umf_model_noWind[[i]] <- noWind
     }
     
     # name all the models to help keep track of them     
          # make empty lists to slot the models
          names(umf_model_all) <- names(present_enough_list)
          # first year occupancy covariates
          names(umf_model_noCreek) <- names(present_enough_list)
          names(umf_model_noElev) <- names(present_enough_list)
          names(umf_model_noVeg) <- names(present_enough_list)
          # colonization covariates
          names(umf_model_col_noYear) <- names(present_enough_list)
          names(umf_model_col_noRain) <- names(present_enough_list)
          # extinction 
          names(umf_model_ext_noYear) <- names(present_enough_list)
          names(umf_model_ext_noRain) <- names(present_enough_list)
          # detection covariates
          names(umf_model_noCloud) <- names(present_enough_list)
          names(umf_model_noDate) <- names(present_enough_list)
          names(umf_model_noObsvr) <- names(present_enough_list)
          names(umf_model_noSky) <- names(present_enough_list)
          names(umf_model_noTemp) <- names(present_enough_list)
          names(umf_model_noTime) <- names(present_enough_list)
          names(umf_model_noWind) <- names(present_enough_list)
          
### Now use the LRT() function to test the significance. 
     # make matrices to hold all the stats for each species
     # I'm testing for 14 different variables so I need 14 rows & 3 columns
     template_stats <- matrix(nrow = 14, ncol = 4) 
     
     # names the template stats, so they're easy to read
     colnames(template_stats) <- c("Chisq","DF","Pr(>Chisq)","Coefficient")
     rownames(template_stats) <- c("Occ_Creek","Occ_Elev","Occ_GreenVeg",
                                   "Col_Year","Col_Rain",
                                   "Ext_Year","Ext_Rain",
                                   "Det_Cloud","Det_Date","Det_Obsvr","Det_Sky",
                                        "Det_Temp","Det_Time","Det_Wind")
     
     # rep the list so I can slot the stats into it
     stats_stack <- rep(list(template_stats), times=length(all_species_umf))
     
     # name the list, so it's easy to keep track of things
     names(stats_stack) <- names(present_enough_list)
     
     # now loop through and test them all for each species
     #    I need to use the unlist() function because the LRT() function spits out a
     #    dataframe, so I need to unlist() it into a vector to then slot it into the
     #    matrix
     for (i in 1:length(all_species_umf)){
          # first year occupancy variable tests
          stats_stack[[i]][1,1:3] <- unlist(LRT(m1=umf_model_all[[i]],
                                             m2=umf_model_noCreek[[i]]))
          stats_stack[[i]][2,1:3] <- unlist(LRT(m1=umf_model_all[[i]],
                                             m2=umf_model_noElev[[i]]))
          stats_stack[[i]][3,1:3] <- unlist(LRT(m1=umf_model_all[[i]],
                                             m2=umf_model_noVeg[[i]]))
          
          # colonization variable tests
          stats_stack[[i]][4,1:3] <- unlist(LRT(m1=umf_model_all[[i]],
                                             m2=umf_model_col_noYear[[i]]))
          stats_stack[[i]][5,1:3] <- unlist(LRT(m1=umf_model_all[[i]],
                                             m2=umf_model_col_noRain[[i]]))
          
          # extinction variable tests
          stats_stack[[i]][6,1:3] <- unlist(LRT(m1=umf_model_all[[i]],
                                             m2=umf_model_ext_noYear[[i]]))
          stats_stack[[i]][7,1:3] <- unlist(LRT(m1=umf_model_all[[i]],
                                             m2=umf_model_col_noRain[[i]]))
          
          # detection variable tests
          stats_stack[[i]][8,1:3] <- unlist(LRT(m1=umf_model_all[[i]],
                                             m2=umf_model_noCloud[[i]]))
          stats_stack[[i]][9,1:3] <- unlist(LRT(m1=umf_model_all[[i]],
                                             m2=umf_model_noDate[[i]]))
          stats_stack[[i]][10,1:3] <- unlist(LRT(m1=umf_model_all[[i]],
                                              m2=umf_model_noObsvr[[i]]))
          stats_stack[[i]][11,1:3] <- unlist(LRT(m1=umf_model_all[[i]],
                                              m2=umf_model_noSky[[i]]))
          stats_stack[[i]][12,1:3] <- unlist(LRT(m1=umf_model_all[[i]],
                                              m2=umf_model_noTemp[[i]]))
          stats_stack[[i]][13,1:3] <- unlist(LRT(m1=umf_model_all[[i]],
                                              m2=umf_model_noTime[[i]]))
          stats_stack[[i]][14,1:3] <- unlist(LRT(m1=umf_model_all[[i]],
                                              m2=umf_model_noWind[[i]]))
     }


# add the coefficients of the continuous variables
for (i in 1:length(stats_stack)){
     
     # Occupancy
          # Creeks - nope, category
          
          # Elevation
          stats_stack[[i]][2,4] <- coef(
               umf_model_all[[i]]["psi"])["psi(Elevation)"]
          
          # Green vegetation
          stats_stack[[i]][3,4] <- coef(
               umf_model_all[[i]]["psi"])["psi(Green_Veg)"]
     
     # Colonization
          # Year - nope, category
          
          # Rain
          stats_stack[[i]][5,4] <-coef(
               umf_model_all[[i]]["col"])["col(Rain)"]
     # Extinction
          # Year - nope, category
          
          # Rain
          stats_stack[[i]][7,4] <-coef(
               umf_model_all[[i]]["ext"])["ext(Rain)"]
     
     # Detection
          # Cloud - nope, category
          
          # Date
          stats_stack[[i]][9,4] <-coef(
               umf_model_all[[i]]["det"])["p(Date)"]
          
          # Observer - nope, category
          
          # Sky - nope category
          
          # Temperature
          stats_stack[[i]][12,4] <-coef(
               umf_model_all[[i]]["det"])["p(Temp)"]
          
          # Time
          stats_stack[[i]][13,4] <-coef(
               umf_model_all[[i]]["det"])["p(Time)"]
          
          # Wind - nope, category
}

# get the stats all into one table
     # make the table
     stats_table <- as.data.frame(matrix(nrow = 21, ncol = 21))
     
     # name the rows to be species
     rownames(stats_table) <- names(stats_stack)
     
     # names the columns to be the variables
     variables <- c(rownames(stats_stack[[1]]),"Elevation",
                    "Greenness","Rain","Rain","Date",
                    "Temperature","Time")
     colnames(stats_table) <- variables

     # populate the table
     for (i in 1:21){
          for (j in 1:14){
               stats_table[i,j] <- stats_stack[[i]][j,3]
          }
          # Elevation
          stats_table[i,15] <- coef(umf_model_all[[i]]["psi"])["psi(Elevation)"]
          
          # Green vegetation
          stats_table[i,16] <- coef(umf_model_all[[i]]["psi"])["psi(Green_Veg)"]
          
          #colonization
          # Rain
          stats_table[i,17] <-coef(umf_model_all[[i]]["col"])["col(Rain)"]
          
          # Extinction
          # Rain
          stats_table[i,18] <-coef(umf_model_all[[i]]["ext"])["ext(Rain)"]
          
          # Detection
          # Date
          stats_table[i,19] <-coef(umf_model_all[[i]]["det"])["p(Date)"]
          
          # Temperature
          stats_table[i,20] <-coef(umf_model_all[[i]]["det"])["p(Temp)"]
          
          # Time
          stats_table[i,21] <-coef(umf_model_all[[i]]["det"])["p(Time)"]
     }
 
# write the table to a csv to edit and review
write.csv(stats_table, file="output/stats_table.csv")

# try to make nice tables with stargazer
library(stargazer)

for (i in 1:21){
     stargazer(stats_stack[[i]], summary=F, rownames=T)
}
     
stargazer(stats_table[,15:21], summary=F, rownames=T)

# how to see how many species were detected across each year
sum(apply(yearly_DPs, 1, prod)>0)
