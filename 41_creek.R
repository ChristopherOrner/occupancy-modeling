### Make plots for all creeks ###

# load the package just incase I need too
library(unmarked)

# use the model to predict how occupancy changes with the amount of green vegetation 
#    by creek generate the values of green vegetation to use in the predict
green.vals <- seq(
               summary(all_species_umf[["PHAI"]]@siteCovs$Green_Veg)["Min."], 
               summary(all_species_umf[["PHAI"]]@siteCovs$Green_Veg)["Max."], 
               by = (summary(all_species_umf[["PHAI"]]@siteCovs$Green_Veg)["Max."]-
                    summary(all_species_umf[["PHAI"]]@siteCovs$Green_Veg)["Min."])/59)

# loop through to do it for all of them quickly 
for (i in 1:21){
     # the predict function wants to used the original data format (unmarkedMultiFrame?)
     #    as best we can tell, so we need to copy the siteCovs 
     temp.data <- data.frame(all_species_umf[[i]]@siteCovs)
     
     # add a column with the green vegetation to predict with, just use 0 for 
     #    mean elevation to keep it consistent. 
     # Do this for all of the creeks, so I can plot each creek. The predict() 
     #    function requires at least two factors, so I can have all AC, so I'll
     #    leave one LT as the 60th entry to it works.
     temp.data$Green_Veg <- green.vals[1:60]
     temp.data$Creek[1:59]<-"AC"
     temp.data$Elevation <- 0
     
     # Same for CH
     temp.data2 <- data.frame(all_species_umf[[i]]@siteCovs)
     temp.data2$Green_Veg <- green.vals[1:60]
     temp.data2$Creek[1:59]<-"CH"
     temp.data2$Elevation <- 0
     
     # Same for EP
     temp.data3 <- data.frame(all_species_umf[[i]]@siteCovs)
     temp.data3$Green_Veg <- green.vals[1:60]
     temp.data3$Creek[1:59] <-"EP"
     temp.data3$Elevation <- 0
     
     # Same for LT, but add a last AC because LT is the last one
     temp.data4 <- data.frame(all_species_umf[[i]]@siteCovs)
     temp.data4$Green_Veg <- green.vals[1:60]
     temp.data4$Creek[1:59] <- "LT"
     temp.data4$Creek[60] <- "AC"
     temp.data4$Elevation <- 0
     
     # use the predict function() to predict the first year occupany of 
     #    speices, hence type = "psi", using the data generated above
     creek.predict<-predict(umf_model_all[[i]], 
                           type = "psi", newdata = temp.data, appendData=TRUE)
     creek.predict2<-predict(umf_model_all[[i]], 
                            type = "psi", newdata = temp.data2, appendData=TRUE)
     creek.predict3<-predict(umf_model_all[[i]], 
                            type = "psi", newdata = temp.data3, appendData=TRUE)
     creek.predict4<-predict(umf_model_all[[i]], 
                            type = "psi", newdata = temp.data4, appendData=TRUE)
     
     # name a title for the graph that can change with each species
     title <- paste0("Predicted Occupancy Across Vegetation Levels for ",
                     names(all_species_umf)[i])
     
     # give a name to the pdf 
     pdf_name <- paste0("output/creek_pics/creek_", names(all_species_umf)[i],".pdf")
     
     # open up a pdf file that the plot will be plotted on to and saved
     pdf(as.character(pdf_name))
     
     # start plotting 
     #    I have the color is white here because it uses points and I just want 
     #    lines, so I'll add the same data with lines right after and have the points
     #    be white so they're not visible.
     plot(creek.predict$Green_Veg, creek.predict$Predicted*100, 
          xlab="Standard Deviations of Greenness",
          ylab="Predicted Occupancy (%)",
          xlim=c(summary(all_species_umf[["PHAI"]]@siteCovs$Green_Veg)["Min."], 
                 summary(all_species_umf[["PHAI"]]@siteCovs$Green_Veg)["Max."]),
          ylim=c(0,100),
          col="white",
          main=title)
     
     # plot each creek
     lines(creek.predict2$Green_Veg[1:59],creek.predict$Predicted[1:59]*100, 
          col = "Black", lwd=3)
     lines(creek.predict2$Green_Veg[1:59],creek.predict2$Predicted[1:59]*100, 
           col = "Red", lwd=3)
     lines(creek.predict2$Green_Veg[1:59],creek.predict3$Predicted[1:59]*100, 
           col = "Green", lwd=3)
     lines(creek.predict2$Green_Veg[1:59],creek.predict4$Predicted[1:59]*100, 
           col = "Blue", lwd=3)
     
     # add a legend
     legend("bottomright",legend=c("AC", "CH","EP","LT"),
            col=c("black","red", "green","blue"), lty=1, cex=0.8, lwd=3)
     
     # turn off the pdf graphics, so the file is completed
     dev.off()
}
