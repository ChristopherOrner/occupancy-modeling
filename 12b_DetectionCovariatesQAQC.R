#################################################################################        
# Build a dataframe from scratch by plugging in data from the obs dataset. To get
#    detection covariates - all the stuff written down at the start of a survey
#    and then also the time at the start of a point.
# I need to scale CONTINUOUS variables
#################################################################################

# SURVEY VISIT VARIABLES #########################################################
# 
# Theres are variables that visit between years, sites, and surveys so I need 
#    a column for each YYYY_V for each site.
# 
# I have all the detection histories made, and now I need to create the survey 
#    visit variables, like temperature, time, cloud cover etc. 
# There's two chucks to this: 
#    1) the variables like temperature that are the same for all points across a 
#    visit and 
#    2) time that is specific to each point


library(dplyr) # need dplyr package to use distinct() function

### TEMPERATURE ### - continuous
#
# Okay, so for this I'm basically doing the detection histories all over again
#    where I'm formatting the data in from the original spreadsheet into the 
#    proper format for the model. However, thankfully I don't have to manipulate
#    the data as much by compression the number of detections to just 1s and 0s
#    because I'm just taking the values as they are and putting it into a nice 
#    matrix. Additionally, these variables don't vary with the species, so I can
#    use these for all the species.

# grab just the ID and temperature columns. I'm formating temperatures, and I need
#    the IDs to properly align the temperatures for the matrix.
temps <- obs2[,c("ID","Temp_.F.")]
     # I'm using the obs data because I want to grab it for all points, including
     #    points that only had fly-overs and far away birds.

# There's a line for every species observation, and the temperature is the same 
#    for every point (and even every visit because it's just recorded once at the 
#    start of the survey), so I only need one line from every point.
# The distinct() function finds all unique values in a column and pulls out just 
#    one of those rows them (again every row for one unique ID is the same, so I 
#    can just grab 1 row without worrying). temps is the dataframe, ID is what 
#    the distinct() function will use to identify unique values, and .keep_all = T
#    means distinct() will pull the rest of the columns too (otherwise it would 
#    just be the unique() function).
#         # I even tried both and yeah, they come out the same.
temps_unique <- distinct(temps, ID, .keep_all = TRUE)

# take the output from distinct() and sort the rows by ID, so the temperature data
#    is in the right configuration to be pulled out and put into a matrix.
temps_sort <- temps_unique[order(temps_unique$ID),]

# grab just the temperature values - this will go into a vector as expected/
#    planned for
temps_vec <- temps_sort$Temp_.F.

# scale the values, so they have mean 0 and SD of 1 because that data is 
#    continuous
temps_scaled <- scale(temps_vec)

# put the temparature vector into a matrix, like I did for Detection Histories
temps_m <- matrix(temps_scaled, ncol = 18)
     # there's 18 YYYY_V combinations, so ncol = 18,
     # I have to be careful if I add sites that it still works out

# name the matrix rows by the location & site they're from
rownames(temps_m) <- l_p

# name the matrix columns by the year & visit they're from
temps_y_v <- paste0("Temp_",y_v)
colnames(temps_m) <- temps_y_v

# clean up
rm(temps,temps_unique,temps_sort,temps_vec,temps_y_v,temps_scaled)

# Tada! Temperatures should be arrayed properly now to then be joined up with the 
#    presence/absence dataframes. 
# Now to repeat for Year, Location, Date, Observer, Cloud Cover, Wind, 
#    Sky Conditions, and Time. I'm not sure if I'll use all of these, like 
#    observer, so I'll have to go over that with Felix & justify why I include 
#    what I include, but it's easy to make it and then just not use it for now.

### DATE ### - continuous, in Julian days
# same workflow as temperature
     date <- obs2[,c("ID","Date")]
     date_unique <- distinct(date, ID, .keep_all = TRUE)
     date_sort <- date_unique[order(date_unique$ID),]
     date_vec <- date_sort$Date
     date_scaled <- scale(date_vec) # scale because continuous
     date_m <- matrix(date_scaled, ncol = 18)
     rownames(date_m) <- l_p
     date_y_v <- paste0("Date_",y_v)
     colnames(date_m) <- date_y_v
# clean up
rm(date,date_unique,date_sort,date_vec,date_y_v,date_scaled)

### OBSERVER ### - category
# same workflow as temperature
     obsvr <- obs2[,c("ID","Observer")]
     obsvr_unique <- distinct(obsvr, ID, .keep_all = TRUE)
     obsvr_sort <- obsvr_unique[order(obsvr_unique$ID),]
     obsvr_vec <- obsvr_sort$Observer
     obsvr_m <- matrix(obsvr_vec, ncol = 18)
     rownames(obsvr_m) <- l_p
     obsvr_y_v <- paste0("Obsvr_",y_v)
     colnames(obsvr_m) <- obsvr_y_v
# clean up
rm(obsvr,obsvr_unique,obsvr_sort,obsvr_vec,obsvr_y_v)

### CLOUD COVER ### - category
# same workflow as temperature
     cloud <- obs2[,c("ID","Cloud_Cover_...")]
     cloud_unique <- distinct(cloud, ID, .keep_all = TRUE)
     cloud_sort <- cloud_unique[order(cloud_unique$ID),]
     cloud_vec <- cloud_sort$Cloud_Cover_...
     cloud_m <- matrix(cloud_vec, ncol = 18)
     rownames(cloud_m) <- l_p
     cloud_y_v <- paste0("Cloud_",y_v)
     colnames(cloud_m) <- cloud_y_v
# clean up
rm(cloud,cloud_unique,cloud_sort,cloud_vec,cloud_y_v)

### WIND ### - category
# same workflow as temperature
     wind <- obs2[,c("ID","Wind")]
     wind_unique <- distinct(wind, ID, .keep_all = TRUE)
     wind_sort <- wind_unique[order(wind_unique$ID),]
     wind_vec <- wind_sort$Wind
     wind_m <- matrix(wind_vec, ncol = 18)
     rownames(wind_m) <- l_p
     wind_y_v <- paste0("Wind_",y_v)
     colnames(wind_m) <- wind_y_v
# clean up
rm(wind,wind_unique,wind_sort,wind_vec,wind_y_v)

### SKY CONDITIONS ### - category
# same workflow as temperature
     sky <- obs2[,c("ID","Sky_Cond")]
     sky_unique <- distinct(sky, ID, .keep_all = TRUE)
     sky_sort <- sky_unique[order(sky_unique$ID),]
     sky_vec <- sky_sort$Sky_Cond # note capital $S is important
     sky_m <- matrix(sky_vec, ncol = 18)
     rownames(sky_m) <- l_p
     sky_y_v <- paste0("Sky_",y_v)
     colnames(sky_m) <- sky_y_v
# clean up
rm(sky,sky_unique,sky_sort,sky_vec,sky_y_v)

### TIME ### - continuous
# I want time as a continuous variable which requires some manipulation

# Run other code chunk, "12a_CivilTwlight" and then come back.

### I don't need to merge the variables together because I'll just feed each in as its
#    own list item. That will be fast, easier, and less confusing.

# Okay I have all the survey visit variables formatted separately. I can now 
#    merge them all into one large dataframe to later merge with each species'
#    detection history.

#detect_covar <- as.data.frame(cbind(temps_m, date_m, obsvr_m, cloud_m, wind_m, sky_m, time_m))

# I could clean up with workspace too, now they're all merged together
# rm(temps_m, date_m, obsvr_m, cloud_m, wind_m, sky_m, time_m)

# END SURVEY VISIT / DETECTION COVARIATES ########################################

# Removed Variables ##############################################################
#
# I don't need Year to have a column for every survey because it's a
#    yearly-site-covariate, so I just need column one per year.
# ### YEAR ###
#      # same workflow as temperature
#      years <- obs[,c("ID","Year")]
#      years_unique <- distinct(years, ID, .keep_all = TRUE)
#      years_sort <- years_unique[order(years_unique$ID),]
#      years_vec <- years_sort$Year
#      years_m <- matrix(years_vec, ncol = 18)
#      rownames(years_m) <- l_p
#      years_y_v <- paste0("Year_",y_v)
#      colnames(years_m) <- years_y_v
#      years_df <- as.data.frame(years_m)
#      # clean up
#      rm("years","years_unique","years_sort","years_vec","years_m","years_y_v")
#
# I don't need Location to have a column for every survey or every year, because
#    it only varies amoung sites & it doesn't vary between years or surveys.
# ### LOCATION ###
#      # same workflow as temperature
#      loc <- obs[,c("ID","Location")]
#      loc_unique <- distinct(loc, ID, .keep_all = TRUE)
#      loc_sort <- loc_unique[order(loc_unique$ID),]
#      loc_vec <- loc_sort$Location
#      loc_m <- matrix(loc_vec, ncol = 18)
#      rownames(loc_m) <- l_p
#      loc_y_v <- paste0("Loc_",y_v)
#      colnames(loc_m) <- loc_y_v
#      loc_df <- as.data.frame(loc_m)
#      # clean up
#      rm("loc","loc_unique","loc_sort","loc_vec","loc_m","loc_y_v")

# END SURVEY VARIABLES SECTION ###################################################