# TO DO #############################################################################
# Clean up species codes, so I can run the code without random floating species
#    that don't correspond to any actual birds.
# Clean up wind codes, sky condition codes, times, cloud cover codes, dates, temps

# DATA FORMATING IN EXCEL ###########################################################
# first I made all the entries without a distance have Fly Over = 1 for True
#    I made all other entries with something in the Fly Over column besides N for
#    no also be 1.
# I also removed > or ~ characters in the distance that would have distance be
#    read as factors instead of numbers, right now it's being read as integers.
#    If there were 2 numbers listed, I just took the first # to get speedy test
#    data ready. I coded fake numbers as 999 that would be removed by 
#    distance subsetting.
# I deleted Sacatara, so determining occupancy would be easier and the table()
#    would work nicely
# I deleted Tunis because it wasn't recored for a year & then turning the data
#    into a matrix works nicely.
# I renamed all the Locations with their initials
# I added an ID column of YYYY_V_LL_PP
# I sorted all of the observations, so point run 1 to 15 so the tables get 
#    labeled properly, otherwise 15 to 1 visits get labed like they're still 
#    1 to 15. I did this in excel by renaming the points with text instead 
#    of numeric, so it was '01 instead of 1. Then I used =CONCAT() to create
#    YYYY_LL_V_PP, so visits by location stayed together.
# I had to add missing YYYY_LL_V_PP and YYYY_V_LL_PP in so the rows and columns
#    came out right. Something changed and I had to do this after getting the
#    code to workin this afternoon just fine (2/24/19)
# I had to go into excel and reformat the dates in there as YYYY-MM-DD for
#    R to read them correctly to turn into Julian dates.
# Remove AMs from time stamps and just have them as HH:MM
# END EXCEL FORMATTING ############################################################

# read in data
obs <- read.csv("prep_data/Copy_All.csv")

# turn all dates into Julian dates

# I think the as.Date() function would work fine without first converting
#    the dates to characters from factors. I'm just not so confident about
#    it, so I'm converting the dates to characters first, so I know R
#    will read the it in as numbers in text and not the factor numbers 
#    instead of the actual numbers
obs$Date <- as.character(obs$Date)

require(lubridate) # let's me use yday() function to get Julian days

# convert the text in the date column to dates
x <- as.Date(obs$Date)

# stick the dates back in to the obs table as Julian dates
obs$Date <- yday(x)
rm(x)

# subset to exclude fly overs
obs1 <- subset(obs, Fly_Over!=1) # all entries without fly overs
nrow(subset(obs, Fly_Over==1)) # 708 entries were fly overs out of 9675

# subset to only include 100 meters or less
obs2 <- subset(obs1, Distance<=100) # all the entries at or within 100 meters
nrow(subset(obs1, Distance>100)) # 326 entries were more than 100 meters away out
#      8967

unique(obs2$Location) # had two EP so went to excel and sorted to all EP and 
# replace, so then only 1 EP showed - prob random space
unique(obs2$Point)     # there's only 15 points, so that's good

# rename the object to a permanent name I'll use in other scripts that won't change.
obs_sub <- obs2