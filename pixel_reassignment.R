# Pixel reclassification based on neighbourhood superclass counts
#
# Author: F Baker (21/02/19)
#
# libraries required for this exercise:
library(rgdal) # projections; read and write vector data
library(sp) # spatial datatypes in R
library(rgeos) # topology functions
library(raster) # library for handling raster data
#
setwd("<<- YOUR DIRECTORY HERE ->>") # set working directory to read and write files
list.files() # examine files in directory
#
cla.ras <- raster("class_raster.tif") # read-in classification raster
store <- cla.ras # store unaltered raster for further reclassification
#
cla.ras[cla.ras%in%c(13,14)] <- NA # identify pixels with redundant values (for shaded pixels) and reassign as NA
#
## data frame with class raster values and associated map classes
cla.names <- data.frame(vals=unique(values(cla.ras))[order(unique(values(cla.ras)))],
                        class=as.character(c("Bare_Earth","Artificial","Deciduous",
                                                             "Evergreen","Grass","Water","NA")),stringsAsFactors = F) 
#
remPTS <- readOGR(".","shaded_points",stringsAsFactors = F) # read-in point data to reassign pixels
# Background info: shaded points are either for vegetative or non-vegetative surfaces - this determines candidate class groups for reassignment
# e.g. vegetative shaded pixels can only be reassigned to vegetation or water classes
#
choo_len <- length(remPTS) # number of points to use for processing below - processing may be slow for full dataset
#
remPTS <- remPTS[1:choo_len,] # assign points based on choo_len
remPTS$Class_name <- ifelse(remPTS$Class_name=="nnbr__Shaded_Vegetation","VEG","NVEG") # rename point classes
#
# Variables to control pixel reassignment below:
#
def_bwid <- 100 # radius (m) of ciruclar buffer around each point
buf_int <- 10 # buffer radius incrementer if no majority class is found
#
plot_flag <- T # controls plot processes below - slows down processing time considerably if T
wtm <- 0.8 # pause (seconds) betweeen plotting functions
# 
cla.val <- character() # vector to store new pixel values per point
proc_time <- numeric() # vector to store processing time for each iteration
# 
if(length(cla.val)==0){ # conditional statement to ensure variable is ready for assignment
  for(i in 1:length(remPTS)){
      #
      bwid <- def_bwid
      bwid_flag <- T # controls conditional statement if number of raster pixel counts are tied
      #
      while(bwid_flag){ 
        st <- Sys.time() # stores start of processing time
        buf <- gBuffer(remPTS[i,],width = bwid,byid=T) # creates zonal buffer for pixel counts
        plex <- gBuffer(buf,20,byid = T) # for plotting purposes only
        cr <- mask(crop(cla.ras,extent(buf)),buf) # create raster within buf extents
        #
        ##
        if(plot_flag){ # plot_flag = T :: wrap for plot functions 
          plot(extent(plex),axes=F,ann=F,bty="n")
          plot(remPTS[i,],pch=19,col="blue",add=T)
          Sys.sleep(wtm)
          #
          plot(buf,add=T,border="red",lwd=3)
          Sys.sleep(wtm)
          #
          plot(cr,add=T,legend=F)
          plot(remPTS[i,],pch=19,col="blue",add=T)
          plot(buf,add=T,border="red",lwd=3)
          Sys.sleep(wtm)
        }
        #
        t <- as.data.frame.table(table(values(cr))) # create dataframe with all pixel counts
        t$class <- as.character(cla.names$class[match(t$Var1,cla.names$vals)]) # add class names for graph plot
        #
        if(remPTS$Class_name[i]=="VEG"){ # statement determines point class, and thus pixel classes for reassignment
          choo <- t[t$Var1%in%c(3,4,5,8),]
        } else {choo <- t[t$Var1%in%c(0,1,8),]}
        #
        if(length(choo$Freq[choo$Freq==max(choo$Freq)])>1){ # assesses if pixel counts are tied - if so buffer radius is incremented
          bwid_flag = T
          bwid <- bwid + buf_int # set incremented buffer width
        } else {
        #
        if(plot_flag){ # plots graph to demonstrate pixel counting
          plot.new()
          Sys.sleep(wtm)
          barplot(choo$Freq,axes=F,names.arg = choo$class)
          Sys.sleep(wtm)
        }
        #
        cla.val <- c(cla.val,as.character(choo$Var1[which(choo$Freq==max(choo$Freq))])) # stores relevant pixel class value for point
        bwid_flag = F # sets conditional variable to end processing for this point
      } 
    }
    proc_time <- c(proc_time,Sys.time()-st) # stores time in seconds for this process
    #
    print(paste(i," out of ",length(remPTS),"| Estimated time remaining: ",round((mean(proc_time)*(length(remPTS)-i))/60,3)," minutes",sep = ""))
    print("#")
  }
} else {
  print("reset cla.val vector") # error message
}
# 
# Use pixel class counts to reassign shaded pixels
change_cells <- cellFromXY(cla.ras,coordinates(remPTS)) # identify cell index numbers for pixels
store[change_cells] <- as.numeric(cla.val) # assign pixel values to cells
#
writeRaster(store,"reclass.tif",format="GTiff",datatype="INT4S",overwrite=T) # write output raster