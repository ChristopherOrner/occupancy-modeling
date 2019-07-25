# TO DO #############################################################################
# 
# Decide what to do with Sacatara & Tunis
#    # I think I'll type different codes to get get TU added & do '14,'16,&'18
#    # I'll start with just the other 4 for now
# 
# END TO DO #########################################################################

# DATA FORMATING IN EXCEL ###########################################################
# 
# Can be read on the Record_of_QAQC_Edits_CO sheet!
#
# END EXCEL FORMATTING ##############################################################

# read in data
obs <- read.csv("survey_data/AllYears_Processed_QAQC_CO_4.3.csv")

# immediately remove Sacatara & TU entries, write to CSV & re-read to 
#    help with formating later, so the NO POINT doesn't continuosly show  
#    up with the table() function
     
     # confirm NO POINT is there as a level
     unique(obs$Point) 
     
     # check I have the code right 
     length(obs$Point[obs$Point=="NO POINT"])
          # says there's 15
     
     # check I have the code right 
     length(obs$Location[obs$Location=="SA"])
          # says there's 385
     
     # check I have the code right 
     length(obs$Location[obs$Location=="TU"])
          # says there's 2050
     
     # total of 2450 entires should be removed max
     
     # check how many have both
     nrow(subset(obs, Point=="NO POINT" & (Location=="SA" | Location=="TU")))
          # says 12 are both NO POINT and SA or TU
     
     # total of 2438 should be removed
     
     # remove the NO POINT points
     obs1 <- subset(obs, Point!="NO POINT" & Location!="SA" & Location!="TU")
          # matches that 2438 were removed by checking workspace by going
          #    from 12108 to 9670
     
     # confirm
     unique(obs1$Point) 
          # NO POINTS have been removed but remain as a level
     unique(obs1$Location)
          # SA & TU have been revmoed by remain as a level
     
     # write to a CSV
     write.csv(obs1, file="temp/removed_nopoints.csv")
     
     # read it back in 
     obs2 <- read.csv("temp/removed_nopoints.csv")
     
     # confirm NO POINTs is no longer a level
     unique(obs2$Point)
          # NO POINTS have been removed!!
     unique(obs2$Location)
          # SA and TU have been removed!!

# turn all dates into Julian dates
     # I think the as.Date() function would work fine without first converting
     #    the dates to characters from factors. I'm just not so confident about
     #    it, so I'm converting the dates to characters first, so I know R
     #    will read the it in as numbers in text and not the factor numbers 
     #    instead of the actual numbers

     obs2$Date <- as.character(obs2$Date)
     
     require(lubridate) # let's me use yday() function to get Julian days
     
     # convert the text in the date column to dates
     x <- as.Date(obs2$Date)
     
     # stick the dates back in to the obs table as Julian dates
     obs2$Date <- yday(x)
     rm(x)

# subset to exclude fly overs
     # get fly overs read in a characters
     obs2$Fly_Over <- as.character(obs2$Fly_Over)
     
     # subset to entries with only blank fly over column entry
     obs3 <- subset(obs2, Fly_Over=="") # all entries without fly overs
     
     # check to see how many were removed
     nrow(subset(obs2, Fly_Over!="")) # 632 entries were fly overs

# subset to only include 100 meters or less
     # get distances from factors to characters b/c there's non-numeric entries
     obs3$Distance <- as.character(obs3$Distance)
     
     # check out which entries need to be removed.
     unique(obs3$Distance)
     
     # check how many there are of each
     length(obs3$Distance[obs3$Distance=="?"])         # 13 entry
     length(obs3$Distance[obs3$Distance=="X"])         # 1
     length(obs3$Distance[obs3$Distance==""])          # 93
          ### the empty entries includes NO BIRDS points too ###
     length(obs3$Distance[obs3$Distance=="110->55"])  # 1 -> 0
          # keep because both it came within 100 meters
     #length(obs3$Distance[obs3$Distance=="8,20"])     # 1 -> 0
          # keep because both are within 100 meters
     #length(obs3$Distance[obs3$Distance=="86,60"])    # 1 -> 0
          # keep because both are within 100 meters
     #length(obs3$Distance[obs3$Distance=="10, 20"])   # 1 -> 0
          # keep because both are within 100 meters
                                                       # total = 134 -> 107
          # the 107 matches the spreadsheet number when filtered by Flyover
          #    & distance (now with excluding SA and TU too)
     
     # check that I have the code right to remove then by making sure it 
     #    grabs all of them
     length(obs3$Distance[obs3$Distance %in% c("?","X","")])
          # the code checks out, it finds 107 entries
     
     # remove the entries
     obs4 <- subset(obs3, !Distance %in% c("?","X",""))
     
     # double check remaining entries
     unique(obs4$Distance) # looks okay at a glace
     
     # changes the data from characters to numeric
     obs4$Distance <- as.numeric(obs4$Distance)
          # "110->55", "8,20", "86,60", and "10, 20" were coerced to NAs
     
     # make NAs 0.23, so it's clear they're replaced, but won't get filtered out
     obs4$Distance[is.na(obs4$Distance)] <- 0.23
     
     # check that only 4 were replaced
     length(obs4$Distance[obs4$Distance==0.23])
     
     # check it still looks good
     unique(obs4$Distance)
     
     # subset to only entries within 100 meters or less
     obs5 <- subset(obs4, Distance<=100)
     
     # see how many rows were removed
     nrow(subset(obs4, Distance>100)) # 324 entries were more than 100 meters
     
     # check the numbers look good
     unique(obs5$Distance)

# subset out the entries that were marked for removal
     # check if I have the right code to tease them out
     length(obs5$Delete.for.Analysis[obs5$Delete.for.Analysis=="Y"])
          # says there's 21

     # subset the data to only keep the data without Ys
     obs6 <- subset(obs5, Delete.for.Analysis!="Y")

# the NO BIRDS points were subsetted out already when I subsetted out 
#    points that didn't have a distance recorded

          
### Clean up the rest of the data

# make structure the data structures are good
     # check it out
     str(obs6)
     
     # make copy for edits just in case
     obs7 <- obs6
     #1  X.5          integer                  - old row numbers added from reading in CSV 2nd time, remove
     #2  SORT         factor with 1080 levels  - good
     #3  ID           factor with 1080 levels  - good
     #4  Year         integer                  - fine? check with Felix for model
     #5  Location     factor with 4 levels     - good b/c category
     #6  Visit        integer                  - fine b/c not used in model
     #7  Date         numeric                  - good
     #8  Observer     factor w/ 10 levels      - good b/c category
     #9  Temp         integer                  - fine
     #10 Cloud Cover  integer                  - bad, need it to be factor
     #11 Wind         integer                  - bad, need it to be factor
     #12 Sky          integer                  - bad, need it to be factor
     #13 Time         factor                   - fine b/c dealt with later
     #14 Point        integer                  - fine b/c not used in model
     #15 Species      factor                   - fine
     #16 Age          factor                   - won't use
     #17 Sex          factor                   - won't use
     #18 Behavior     factor                   - won't use
     #19 Fly Over     character                - already processed
     #20 Detection    factor                   - won't use
     #21 Distance     numeric                  - already processed
     #22 Cluster Num  factor                   - won't use
     #23 Cluster Code factor                   - won't use
     #24 Notes        factor                   - fine
     #25 Data Notes   factor                   - fine
     #26 Checked      factor                   - won't use
     #27 QAQC Notes   factor                   - fine
     #28 Delete for   factor                   - already processed
     #29 X            logical                  - drifter, remove
     #30 X.1          logical                  - drifter, remove
     #31 X.2          logical                  - drifter, remove
     #32 X.3          logical                  - drifter, remove
     #33 X.4          logical                  - drifter, remove
     
     # turn cloud cover into factors  
          # see the levels there
          unique(obs7$Cloud_Cover_...)
               # there's 4,1,3,2
          
          # turn into a factor
          obs7$Cloud_Cover_... <- factor(obs6$Cloud_Cover_...,
                                         labels=c("1","2","3","4"))
     
     # turn wind into factors
          # see the levels there
          unique(obs7$Wind)
               # there's 0,2,1,3
          
          # turn into a factor
          obs7$Wind <- factor(obs6$Wind, labels=c("0","1","2","3"))
     
     # turn sky conditions into a factor
          # see the levels there
          unique(obs7$Sky_Cond)
               # there's 2,1,0,3,4,5
          
          # turn into a factor
          obs7$Sky_Cond <- factor(obs6$Sky_Cond, labels=c("0","1","2",
                                                          "3","4","5"))     

# remove the columns I won't be working with and aren't QAQC-ed: Age, Sex, 
#    Behavoir, Detection, Distance, Cluster Number, Cluster Code, and Checked
     # grab all the rows & just columns I need using column numbers
     obs8 <- obs7[,c(2:15,24,25,27)]

# rename the object to a permanent name I'll use in other scripts that won't change.
obs_sub <- obs8

# clean up intermediary products I won't use later
rm(obs1,obs3,obs4,obs5,obs6,obs7,obs8)
