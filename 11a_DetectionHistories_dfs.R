################################################################################# 
# I think I might have to build a dataframe from scratch by plugging in data
#    from the obs_sub dataset. This way I can get absences filled in with 0 by
#    using a logical statement.
#################################################################################
     
# DETECTION HISTORIES ###########################################################
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
species_list <- as.character(unique(obs_sub$Species)) # there's 145 entries, but    
                                                   #   I still neeed to clean up
                                                   #   the codes
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
     
     # give the presence/absence data for the given species a name
     assign(paste0("PA_",species_list[i]), PA)
     
     # write it to a csv if I want to check it out for accuracy
     write.csv(x = paste0("PA_",species_list[i]), 
                file = paste0("temp_species_PAs/PA_",species_list[i],".csv"))
}

# clean up
rm("x","i","PA","counts","species")

# END DETECTION HISTORIES SECTION ################################################

# DETECTION PRECENTAGE ###########################################################
#
# Okay I just made a bunch of detection history data frames, and now I want to 
#    know how often a bird species was detected across all sites. I need to 
#    know for the model because I can't feed it species with very low recorded
#    detections or very high because the model will have a hard time assessing 
#    how the speices's detection (and then occupancy) co-varies with the variables
#    I tell the model to use - the model would just see the bird's no where, so 
#    nothing is significant (of things are falsely significant by chance) because
#    nothing improves the likeliness of detecting the bird OR the bird's
#    everywhere, so nothing is significant because the nothing co-varies with the 
#    bird, it just lives wherever it wants.
#    
# I need to be able to summarize each of the PA_&&&& that I just made
# Get all the names of objects in the workspace including all the PAs I made by 
#    asking R to list out everyting in the workspace
workspace <- ls()

# subset the list by grabbing only things that have "PA_" in their name, which 
#    should only be the PA_&&&&s
PAs <- workspace[grep("PA_", workspace[])]

# I want to have the species names sorted alphabetically, so the final product
#    will be easier to read (just stick with it for now because the final 
#    product will be one table/data frame)
sl_sorted <- sort(species_list)

# Create a space holder vector that'll be used to fill in detection precentages. 
#    I need on space for every species, so it's 1 through the length of the 
#    species list
DPs <- 1:length(sl_sorted)

# loop through each of the species presence/absence dataframes to calculate the
#    percent of the time they were observed
for (i in 1:length(sl_sorted)){
     
     # read the name of a PA, from the list I made above, as the name of an 
     #    object instead of a character string because R won't grab the object
     #    from the workspace then
     PA <- as.name(PAs[i])
     
     # R still doesn't want to grab the PA from the workspace to do anaylsis with
     #    it, but this eval() function basically copies it and I can then use that
     #    for analysis
     PA_object<- eval(PA)
     
     # the detection percentage is the number of times detected divide by the 
     #    total number of survey points (rows) and survey times (columns).
     #    This tells R to stick this number into the place holder vector.
     DPs[i] <- (sum(PA_object)/(nrow(PA_object)*ncol(PA_object)))*100
}

# name the values in the vector, so I know what detection percentage refers to 
#    what species
names(DPs) <- sl_sorted

# turn the vector into a data fram because I prefer this format
DPs_df <- as.data.frame(DPs)
View(DPs_df)

# see which birds are detected at least 10% of the time
DP_GTET10 <- DPs[DPs>=10] # wooha that's only 17 species...

# what about at least 5% of the time
DP_GTET05 <- DPs[DPs>=5] # that's only 33 species...

# what about 25% of the time
DP_GTET25 <- DPs[DPs>=25] # that's only 8 species...

# okay, so let's say I only want to use species that are detected 25% of the time
#    to keep things simpler. I need to get their names and keep just their PAs

# get the names of species that are there 25% of the time
present_enough <- names(DP_GTET25)

# remove those species from the list of species to get the list of species PAs
#    I'd like to remove to keep the workspace clean
not_enough <- species_list[!species_list %in% present_enough]

# make a list of the PAs to remove
PAs_LT25 <- paste0("PA_", not_enough)

# now remove them all
rm(list=PAs_LT25)

# Now I'm left with species that I think have a enough detections to run through 
#    the model. I can back to change this cut off to something Felix and I agree
#    is an appropriate cut-off because I just randomly chose 25% to have something
#    to work with for now.

# clean up 
rm("workspace", "species_list", "sl_sorted","PAs","PA_object", "DPs", "DPs_df", "DP_GTET05", "DP_GTET10", "DP_GTET25", 
   "not_enough", "PAs_LT25")
     # I might want to use the present_enough vector later for looping through
     #    species to add other variables to the dataframe, so I'll hang on to 
     #    it for now.

# END DETECTION PERCENTAGE SECTION ###############################################