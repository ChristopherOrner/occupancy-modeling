#################################################################################        
# Build a dataframe from scratch by plugging in data from the obs dataset. To get
#    detection covariates - all the stuff written down at the start of a survey
#    and then also the time at the start of a point.
# I need to scale CONTINUOUS variables
#################################################################################

# SURVEY VISIT VARIABLES #########################################################
# 
# I need to get the civil twilight values into the obs_sub spreadsheet as a row.
#    I think I can do this by having r look at the list of times in the CT_2018
#    spreadsheet that I made.

# Read in the civil twilight sheet
CT <- read.csv("Civil_Twilight/CT_2018.csv")

# This copies data from the CT table I just read in. It says look at the CT_Rise
#    column of CT and use the values in obs_sub$Data and CT's Julian.Data column 
#    to find the matching CT_Rise values and then stick that in 
#    obs_sub$Civil_Twlight
obs2$Civil_Twilight<-with(CT, CT_Rise[match(obs2$Date, Julian.Date)])

library(dplyr) # need dplyr package to use distinct() function

### TIME ### - continuous
# I want time as a continuous variable which requires some manipulation

# define a function to convert time from hours, minutes, and seconds into
#    a continuous variable
time_cont <- function(x) {
     (hours(x)*60) + (minutes(x))
}

# library(lubridate) # needed for the hours() and minutes() functions
library(chron)

# modified workflow from temperature
     time <- obs2[,c("ID","Time","Civil_Twilight")]
     time_unique <- distinct(time, ID, .keep_all = TRUE)
     time_sort <- time_unique[order(time_unique$ID),]
     
     # note capital $T is important
     time_vec <- time_sort$Time
     ct_vec <-time_sort$Civil_Twilight
     
     # get times in format for function
     time_hm <- chron(times=time_vec)
     ct_hm <-chron(times=ct_vec)
     
     # use function to convert into minutes
     time_minutes <- time_cont(time_hm)
     ct_minutes <- time_cont(ct_hm)
     
     # sutract civil time to get minutes after civil time
     time_aftercivil <- time_minutes-ct_minutes
      
     # scale because continuous
     time_scaled <- scale(time_aftercivil) 
     
     time_m <- matrix(time_scaled, ncol = 18)
     rownames(time_m) <- l_p
     time_y_v <- paste0("Time_",y_v)
     colnames(time_m) <- time_y_v
     
     
# clean up
rm(time,time_unique,time_sort,time_vec,time_y_v,time_scaled)
rm(ct_hm,ct_minutes,ct_vec,time_aftercivil,time_hm,time_minutes)
rm(CT)

# Okay I have all the survey visit variables formatted separately. I can now 
#    merge them all into one large dataframe to later merge with each species'
#    detection history.