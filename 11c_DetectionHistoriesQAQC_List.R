################################################################################# 
# I think I might have to build a dataframe from scratch by plugging in data
#    from the obs_sub dataset. This way I can get absences filled in with 0 by
#    using a logical statement.
#################################################################################
     
# DETECTION HISTORIES & FIRST YEAR DETECTION PRECENTS ###########################
# 
# Okay I need to format the data into the detection histories that are required
#    of the unmarked package and colext model. The format is with units sites as 
#    rows, i.e. AC_01, AC_02,..., LT_15, and the surveys as the columns, i.e.
#    2013_1, 2013_2,..., 2018_3.
# 
# Okay the once I have a bunch of detection histories, then I want to 
#    know how often a bird species was detected across all sites during the 
#    first year of surveys because the model predicts occupancy onward from 
#    first year occupancy probabilities (Felix recommend this, so I want to 
#    follow up about a rigorous reason - intuitively I follow though).
# I need detection percents for the model because I can't feed it species with
#    very low recorded detections or very high because the model will have a hard
#    time assessing how the speices's detection (and then occupancy) co-varies
#    with the variables I tell the model to use - the model would just see the
#    bird's no where, so nothing is significant (of things are falsely 
#    significant by chance) because  nothing improves the likeliness of detecting
#    the bird OR the bird's everywhere, so nothing is significant because the
#    nothing co-varies with the bird, it just lives wherever it wants.
# 
# sites as the rows - getting the labels ready
     # location as the two letter abbreviation
     #    each = 15 to match up with the 15 points at each
     location <- rep(c("AC","CH","EP","LT"), each = 15)
     
     # I need the zero's in front, so I'm writing them as text, 
     #    so R keeps the zero.
     #    times = 4, for each of the 4 locations
     points <- rep(c("01","02","03","04","05","06","07","08","09","10","11",
                     "12","13","14","15"), times = 4)
     
     # put the location and point together for unique site labels
     l_p <- paste(location, points,  sep = "_")
     
     # clean up the workspace
     rm(location)
     rm(points)

# years_visits as the columns - getting the labels ready
     # year for all of the location as the two letter abbreviation
     #    each = 3 to match up with the 3 visits every year
     years <- rep(2013:2018, each = 3)
     
     # times = 6, for each of the 6 years
     visits <- rep(1:3, times = 6)
     
     # put the years and visits together for unique survey labels
     y_v <- paste(years, visits, sep = "_")
     det_y_v <- paste0("Det_",y_v)
     # clean up the workspace
     rm(years)
     rm(visits)
     
# create a list, so I can subset by individual species
species_list <- as.character(unique(obs_sub$Species))
     # there's 101 entries, but I still neeed to clean up the codes

# make an empty vector to populate with 1ST year detection percents later
firstyear_DPs <- 1:length(species_list)

# make an empty dataframe for a yearly detection precents
yearly_DPs <- data.frame(NULL) # Don't worry about yearly averages

# make an empty list to populate with detection history data frames later
all_species_list<-list(NULL)

# loop through for each individual species
for (i in 1:length(species_list)) {

     # isolate just the entries with the species of interest by subsetting
     species<-obs_sub[obs_sub$Species==species_list[i],]
     
     # make a "table" that really looks like a vector BUT it summaries the
     #    number of entries per ID, which is a good consilidation of all entries
     #    into the max 180 unique visit/location/point combinations per year.
     #    Use ID column instead of Sort column because the sort column was to 
     #    get all the points 1 to 15 within each visit within each location. 
     #    If I used the Sort Column, then visits would end up in the same column, 
     #    but I want all of the 1st visit at AC, CH, EP, and LT all in the first
     #    column, then all their second visits in the second column, etc. Since
     #    ID is structured YYYY_V_LL_PP (instead of YYYY_LL_V_PP like the Sort 
     #    column) I it'll summarize by Year and Visit before moving to Location
     #    and Point, so I know if I put the data into 18 columns, each column 
     #    will be a YYYY_V like I want it to be.
     counts <- table(species$ID)
     
     # turn the table count summaries into binary presence/absence information
     PA <- as.integer(counts[] >= 1)
     
     # reformate the table into a matrix so each column represents a diff visit
     PA <- matrix(PA, ncol = 18)
          # there's 18 YYYY_V combinations, so ncol = 18,
          # I have to be careful if I add sites that it still works out
     
     # name the matrix rows by the location & site they're from
     rownames(PA) <- l_p
     
     # name the matrix columns by the year & visit they're from
     colnames(PA) <- det_y_v
     
     # turn the matrix into a data frame (not necessary, but I like better)
     PA <- as.data.frame(PA)
     
     # Felix recommended calculating the percent detection from the first year
     #    of data and using a cut off for those values.
     # the detection percentage is the number of times detected divide by the 
     #    total number of survey points (rows) and survey times (columns).
     #    This tells R to stick this number into the place holder vector.
     # I modified the code to calulate the detection percent for each year, so
     #    I can come compare it with the first year to see if there's a 
     #    different
     
     # first year only
     firstyear_DPs[i] <- (sum(PA[,1:3])/(3*nrow(PA[,1:3])))*100
     
     # loop for all years | Don't worry about for now
     for (j in 1:6){
          yearly_DPs[i,j] <- (sum(PA[,((3*j)-2):(3*j)])/
                                   (3*nrow(PA[,((3*j)-2):(3*j)])))*100
     }
     
     # slot the presence/absence data for the given species into the list
     all_species_list[[i]]<- PA
}

# name each entriy in the firstyear_DPs vector so it corresponds to its species
#    and year
names(firstyear_DPs) <- species_list

# name each row and column for yearly DPs
rownames(yearly_DPs) <- species_list
colnames(yearly_DPs) <- paste0("DP_",2013:2018)

# calculate the average of detection percent across all years | Don't worry 
avg_DPs <- rowMeans(yearly_DPs)
yearly_DPs$Average <- avg_DPs

# round the decimal places so they're easier to read
yearly_DPs_round <- round(yearly_DPs,digits = 2)

# # compare the number of entries | Don't worry about it
length(firstyear_DPs[firstyear_DPs>=10])               # 21 species
length(yearly_DPs$Average[yearly_DPs$Average>=10])     # 17 species
#      # I think I'll go with the first year because I suspect birds my 
#      #    be hard to detect going forward, or may be moving out as the climate
#      #    changes & the model should model that with the first year detections.

# name each page in the list by the species it corresponds with
names(all_species_list) <- species_list


# The structure to search a list double brackets [[]] to grab specifc lits
#    and single brackets [] to look in rows and columns like before. So together
#    its list[[i]][j,k]

# see which birds are detected at different precents
     # 25% 
     DPs_25 <- firstyear_DPs[firstyear_DPs>=25] # that's 8 species...
     # 20%
     DPs_20 <- firstyear_DPs[firstyear_DPs>=20] # that's 11 species...
     # 15%
     DPs_15 <- firstyear_DPs[firstyear_DPs>=15] # that's 14 species...
     # 10% of the time
     DPs_10 <- firstyear_DPs[firstyear_DPs>=10] # that's 21 species...
     # 5% of the time
     DPs_05 <- firstyear_DPs[firstyear_DPs>=5]  # that's 32 species...


# okay, so let's say I only want to use species that are detected 10% of the time.
#   I need to get their names and keep just their PAs

# get the names of species that are there 10% of the time
present_enough <- names(firstyear_DPs[firstyear_DPs>=10])

# make empty list
present_enough_list <- list(NULL)

# make separate list of just the species' detection histories that are present 
#    enough
for (i in 1:length(present_enough)){
     present_enough_list[[i]] <- all_species_list[[present_enough[i]]]
}

# name the dataframes in the list, so I can keep track of them
names(present_enough_list) <- present_enough

write.csv(yearly_DPs, file="output/Yearly_Detection_Precentages.csv")

# clean up
rm(i,PA,counts,species)
rm(DPs_05,DPs_10,DPs_15,DPs_20,DPs_25)
rm(det_y_v)
rm(j,present_enough,avg_DPs)
rm(firstyear_DPs)
rm(species_list)
rm(yearly_DPs, yearly_DPs_round)

# END DETECTION HISTORIES & FIRST YEAR DETECTION PRECENTS SECTION ###############