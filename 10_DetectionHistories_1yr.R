# TO DO #########################################################################
# Clean up species codes, so I can run the code without random floating species
#    that don't correspond to any actual birds.
# Figure out how to incorporate multiple year data into this, but it should be 
#    fairly similar (I hope) just changing the ncol=#  and the number of repeats

# DATA FORMATING ####################################################################
# first I made all the entries without a distance have Fly Over = 1 for True
# I also removed > or ~ characters in the distance that would have distance be
#    read as factors instead of numbers, right now it's being read as integers.
# I deleted Sacatara, so determining occupancy would be easier and the table()
#    would work nicely
# I renamed all the Locations with their initials
# I added an ID column of YYYY_V_LL_PT
# 
# read in data
obs <- read.csv("Testing_Code/Copy_2018.csv")

# subset to exclude fly overs
obs1 <- subset(obs, Fly_Over!=1) # all entries without fly overs
nrow(subset(obs, Fly_Over==1)) # 228 entries were fly overs out of 2434

# subset to only include 100 meters or less
obs2 <- subset(obs1, Distance<=100) # all the entries at or within 100 meters
nrow(subset(obs1, Distance>100)) # 296 entries were more than 100 meters away out
                                 #      2206

# subset to exclude Sacatara
#obs3 <- subset(obs2, Location!="Sacatara") # all entries at AC, CH, EL, LT, & TU
#nrow(subset(obs2, Location=="Sacatara")) # 126 entries were at Sacatara

# I think I might have to build a dataframe from scratch by plugging in data
#    from the obs# dataset. This way I can get absences filled in with 0 by using
#    a logical statment

# sites as the rows
location <- rep(c("AC","CH","EL","LT","TU"), each = 15)
points <- rep(c(1,10:15,2:9), times = 5)
l_p <- paste(location, points,  sep = "_")

# years_visits at the columns
years <- rep(c(2018), each=3)
visits <- rep(1:3, times=1)
y_v <- paste(years, visits, sep = "_")

# now to subset by a species
species_list <- as.character(unique(obs2$Species)) # there's 103 levels, but     
                                                   #   I still neeed to clean up 
                                                   #   the codes

# loop through and create occupancy histories for each species
for (i in 1:length(species_list)) {
     
     # isolate just the entries with the spcies of interest
     species<-obs2[obs2$Species==species_list[i],]
     
     # make a "table" that really looks like a vector BUT it summaries the number
     #    of entries per ID, which is a good consilidation of all entries into 
     #    the max 225 unique visit/location/point combinations per year
     counts <- table(species$ID)
     
     # turn the table count summaries into binary presence/absence information
     PA <- as.integer(counts[] >= 1)
     
     # reformate the table into a matrix so each column represents a diff visit
     PA <- matrix(PA, ncol = 3)
     
     # name the matrix rows by the location & site they're from
     rownames(PA) <- l_p
     # name the matrix columns by the year & visit they're from
     colnames(PA) <- y_v
     
     # give the presence/absence data for the given species a name
     assign(paste0("PA_",species_list[i]), PA)
}

# MODELING ########################################################################
# Evently get to using the colext, multi-seaon model
#    psiformula     = regression model (logistic?) for First-year occupancy
#    gammaformula   = regression model to estimate colonization rate
#    epsilonformula = regression model to estimate local extinction (1-survival)
#                     rate 
#    pformula       = regressio to model detection probability

# This one below assumes that all are constant and is a good test/counterfactual to
#    to models that I try out. If this preforms as good or better than my models, 
#    then my models don't do good job to capture how variables impact occupancy
#    or suggest these variable and occupany might not be related strongly.

# m0 <- colext(psiformula= ~1, gammaformula = ~ 1, epsilonformula = ~ 1, pformula = ~ 1, data = simUMF, method="BFGS")
# summary(m0)