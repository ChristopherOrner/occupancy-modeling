# Save workspace for back-up
save(list=ls(), file="temp/backup_4-17.RData")

# clear the Environments panel
rm(list=ls())

# load back in the objects to the workspace
load("temp/backup_4-17.RData")
