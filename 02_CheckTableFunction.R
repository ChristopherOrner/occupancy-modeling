# CHECK TABLE() FUNCTION IS WORKING HOW I THINK IT IS ###############################
#
# I'm having issues with the data frames having detection history that match up 
#    with what I'm manually checking on the master spreadsheet, so I'm going to
# I suspect that there's some YYYY_V_LL_PP that are disappearing when I'm 
#    subsetting the data to remove fly overs and birds greater than 100 meters
#    away. If there's no entries I'm going to populate with a species called
#    "no_birds", in lower case, so it's clear that the point didn't have any 
#    birds when it was surveyed.
# I'm concerned the table function isn't using the subsetted data, but grabbing
#    stuff from the original obs dataframe.

# list of all the sites by the YYYY_V_LL_PP
all <- unique(obs2$ID) 
# there's 1080 like I need it to be

# test out to make sure the matrix I make later is lining up how I think it is
all_matrix <- matrix(all, ncol = 18) 
# now I get a warning that [1075] is not sub-multiple or multiple of the 
# number of rows [60]

# check out where it's going wrong
# toss in random i because I just need to check the subsetting
i <- 1

# workflow from DETCTION HISTORIES
species_list <- as.character(unique(obs2$Species))
species<-obs2[obs2$Species==species_list[i],]
counts <- table(species$ID)
PA <- as.integer(counts[] >= 1)

# Check out if it's lining up properly
View(counts) 
# okay the names are lining up how I want them to for in the counts 
#    table
View(PA)
# ah okay so PA loses the names on the table and I need to look at 
# the counts to check

# it also looks like the counts is being read as a named vector, so I can't
#    only isolate the names as is. I'll turn it into a data frame, so the 
#    names become it's own column.
counts_df <- as.data.frame(counts)

# Check it out
View(counts_df)
# the sites are in their own column now, labeled Var1

# grab just the Var1 column with the sites from remaining by obs2 after 
#    subsetting
site_check <- counts_df$Var1
# the workspace says there's 1080 variables there, so I'm confused

# I'll try fitting the site_check into a matrix to see if it's lining up 
#    correctly now
site_check_matrix <- matrix(site_check, ncol = 18)
# workflow from DETCTION HISTORIES
location <- rep(c("AC","CH","EP","LT"), each = 15)
points <- rep(c("01","02","03","04","05","06","07","08","09","10","11",
                "12","13","14","15"), times = 4)
l_p <- paste(location, points,  sep = "_")
rm(location)
rm(points)
years <- rep(2013:2018, each = 3)
visits <- rep(1:3, times = 6)
y_v <- paste(years, visits, sep = "_")
rm(years)
rm(visits)
rownames(site_check_matrix) <- l_p
colnames(site_check_matrix) <- y_v

# Check out if everything is lining up
View(site_check_matrix)
# alright, well that looks correct

# so when I use obs2$ID to do it, there's only 1075 unique IDs, but when I
#    use the table() function it picks those missing 5 back up. I'm worries
#    that it's then picking up species entries that got subsetted out. A 
#    work around would be to write the subsetted data, obs2, to it's own
#    csv, but then I suspect table() won't pick up the missing IDs again. 
#    I could get around this by adding one entry of "no_birds" to each ID,
#    so I know for a fact that each ID will be in there. However "no_birds"
#    could get confusing, so I think I'll add "fake" so it's clear that it's
#    just a filler entry.
# I'm going to test the loop for DETECTION HISTORIES to see if I still get 
#    errors or if the detection histories are actually being counted from
#    the full spreadsheet obs. I can check by picking a species with a lot
#    of fly overs and looking at it's detection history between the obs and
#    obs2 dataframes and seeing which one matches the PA_&&&& dataframe.
# the loop runs without errors

# see what species has a lot of flyovers to then compare
# put out the spe
flyovers <- subset(obs, Fly_Over==1)
flyovers$ID
# flyovers2 <- obs[obs$Fly_Over==1,]
#      flyovers2$ID
# The result is the same as above with all 1080 levels

# check out what the flyovers look like & make sure it's only flyovers
View(flyovers)

# I want to pull out a species with a lot of flyovers, so there's a good
#    chance I'll easily see a different between the two final dataframes.
#    A good result would be seeing fewer 1's for detection at sites 
#    because then at that site_survey the species was only a flyover or
#    more than 100m aways, so then the species were successfully sub-
#    setted out.
flyover_counts <- table(flyovers$Species)
# remember this comes out as a named vector

# find the species with the most flyovers
flyover_highscore <- max(flyover_counts)

# get the name of the species with the most flyovers
u <- names(flyover_counts[flyover_counts==flyover_highscore])
# the species is EUST btw

# see how many obersations there are in the unsubsetted data and the 
#    subsetted data.
compare_obs <- subset(obs, Species==u) 
# has 593 observations
compare_obs2 <- subset(obs2, Species==u)
# has 469 observations
# okay, this is a promising result. There's fewer records of the 
#    EUST in the subsetted sheet. There were 109 flyovers & there's
#    124 fewer enteries, so the extra are from over 100m - so far
#    so good. 

# Check if they're IDs are the same, if compare_obs2 has fewer than 1080
#    than were does it grab the IDs that eventually become absences.
compare_obs$ID
compare_obs2$ID
# both say they have 1080 levels... aka 1080 IDs in them...
# so I guess that's good that the subsetted data has all of the IDs,
#    so that means it's holding onto them and wouldn't have to grab
#    them from the unsubsetted data (and therefore possibly grab 
#    entries that I had tried to subset away).

# run the unsubsetted and subsetted data through to see what it's detection 
#    history would look like. This will be the test to see if the subsetted 
#    data is going back and actually grabbing all the entries with the table()
#    function - if the subsetted data yields the same detection history as
#    the unsubsetted data, then Houston I got a problem. I'd have to do the
#    whole add fake entries after subsetting the data and writing it to a 
#    new CSV file to ensure all the IDs are used in the table function 
#    without the table function grabbing entries that shouldn't be there.

# run the unsubsetted data - workflow from DETCTION HISTORIES
species_obs<-obs[obs$Species==u,]
counts_obs <- table(species_obs$ID)
PA_obs <- as.integer(counts_obs[] >= 1)
PA_obs_matrix <- matrix(PA_obs, ncol = 18) 
PA_obs_df <- as.data.frame(PA_obs_matrix)
rownames(PA_obs_df) <- l_p
colnames(PA_obs_df) <- y_v

# run the subsetted data - workflow from DETCTION HISTORIES 
species_obs2<-obs2[obs2$Species==u,]
counts_obs2 <- table(species_obs2$ID)
PA_obs2 <- as.integer(counts_obs2[] >= 1)
PA_obs2_matrix <- matrix(PA_obs2, ncol = 18) 
PA_obs2_df <- as.data.frame(PA_obs2_matrix)
rownames(PA_obs2_df) <- l_p
colnames(PA_obs2_df) <- y_v

# substract the subsetted from the unsubsetted
#    I should only get 1s and 0s. 0s for where they match-up and 1s for 
#    where the unsubsetted data had a flyover or record over 100m that
#    isn't present in the subsetted data anymore. If I get a -1 then that
#    means the subsetted data has a detection where the unsubsetted data
#    didn't even have a record, and then Houston I got a problem.
delta_obs_obs2 <- PA_obs_df-PA_obs2_df

# check out the results structure because I wasn't sure if I could do 
#    the dataframe substraction like that
str(delta_obs_obs2)
# yeah it works!

# check out the values
min(delta_obs_obs2)      # min of 0
max(delta_obs_obs2)      # max of 1
# so far so good, no -1s

# write it to a CSV for closer inspection and comparsion with the input
#    excel with all the filters in place: <=100m, species=="EUST",
#    Fly_Over==1.
write.csv(PA_obs2_df, file = "temp/PA_obs2_df_EUST.csv")

# I generated my own presence/absence table from the original spreadsheet
#    by subsetting the data with filters of <=100m and Fly_Over==1. Then
#    I made a new tab, copied over all the Sort Column entries (I really
#    should have done ID, but the end result should be the same), then
#    I had excel delete all duplicate entries, then I constructed a grid 
#    of the LL_PP as rows and YYYY_V as columns and matched up the unique
#    Sort Column entries without duplicates into their cell on the grid
#    (this took a long time and I'm thankful I don't have to do this for
#    every species manually if R works how I want it to work), then had
#    excel find and replace all text with 1 and all blanks with 0, and 
#    finally I compared it to the PA_obs2_df that I generated above. 
#    ANNNNNNNNND THEY MATCH! So the table function is working how I want 
#    it to work and I know it's fitting the value into the matrix properly 
#    too instead of 2013_2 for AC_1 going right under AC_15 for 2013_1, it
#    is going into the next column - I was worried the sort trick I did 
#    on the excel spreadsheet to line the data up wasn't working either.

# In conclusion, this multi-hour, multi-day digression has shored up my 
#    confidence that:
#    1) The table() function is summarizing detection histories using the 
#    subsetted data.
#    2) I have sorted the data properly with the trick on the excel spreadsheet, 
#    and using the $ID in the table() function, so the named vector the table()
#    function produces is being fit into the matrix how I need it to be to have
#    LL_PP as the rows and YYYY_V as the columns.

# Clean up the workspace
rm("all", "all_matrix", "species_list", "species", "counts", "PA", "counts_df", 
   "site_check", "site_check_matrix", "l_p", "y_v", "flyovers", 
   "flyover_counts", "flyover_highscore", "flyovers2", "u", "compare_obs", 
   "compare_obs2", "species_obs", "counts_obs", "PA_obs", "PA_obs_matrix", 
   "PA_obs_df", "species_obs2", "counts_obs2", "PA_obs2", "PA_obs2_matrix", 
   "PA_obs2_df","delta_obs_obs2")
rm("counts_df_obs", "counts_df_obs2", "i")