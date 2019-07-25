# turning the dataframe into a long vector by columns
as.vector(as.matrix(all_species_list[[1]]))

# turning the dataframe into a long vector by columns
as.vector(cloud_m)

# bind them together to get ready for tapply function
cloud_bind <- data.frame(cbind(as.vector(as.matrix(all_species_list[[1]])), as.vector(cloud_m)))

str(cloud_bind)

# check it out
cloud_bind[,1] <- as.numeric(as.character(cloud_bind[,1]))

str(cloud_bind)

tapply(cloud_bind[,1], cloud_bind[,2], length)


# turning the dataframe into a long vector by columns
as.vector(as.matrix(all_species_list[[1]]))

# turning the dataframe into a long vector by columns
as.vector(obsvr_m)

# bind them together to get ready for tapply function
obsvr_bind <- data.frame(cbind(as.vector(as.matrix(all_species_list[[1]])), as.vector(obsvr_m)))

str(obsvr_bind)

# check it out
obsvr_bind[,1] <- as.numeric(as.character(obsvr_bind[,1]))

str(obsvr_bind)

tapply(obsvr_bind[,1], obsvr_bind[,2], mean)
tapply(obsvr_bind[,1], obsvr_bind[,2], length)


# turning the dataframe into a long vector by columns
year_dect <- matrix(nrow = 60, ncol = 6)

# turning the dataframe into a long vector by columns
for (i in 1:60){
for (j in 1:6){
     if (sum(all_species_list[["ACWO"]][i,((3*j)-2):(3*j)])>0)
          {year_dect[i,j] <- 1}
     else 
          {year_dect[i,j] <- 0}
}
}

creek_vec <- rep(rep(c("AC","CH","EP","LT"), each = 15), times = 6)

# bind them together to get ready for tapply function
creek_bind <- data.frame(cbind(as.vector(year_dect), creek_vec))

str(creek_bind)

# check it out
creek_bind[,1] <- as.numeric(as.character(creek_bind[,1]))

str(creek_bind)

tapply(creek_bind[1:60,1], creek_bind[1:60,2], mean)
tapply(creek_bind[,1], creek_bind[,2], length)
