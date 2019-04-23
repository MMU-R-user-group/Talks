## Creating thiessen polygons in R
#
# Author: F Baker (21.02.19) 
#
# libraries required for this exercise: 
library(sp) # spatial datatypes
library(rgdal) # projections and read/write functions
library(spatstat) # point pattern analysis and dirichlet function for 
library(maptools) # create processing window - "owin" style
library(rgeos) # topology functions
library(raster) # not sure if this is needed but it is here anyway
#
setwd("G:/PhD Project/1. General Admin/1.4 Conferences Presentations/R users") # Working directory where the data is stored and outputted
list.files() # see what files are in the folder
# read in shapefiles using readOGR function
trh <- readOGR(".","Tree_heights",stringsAsFactors = F)
trl <- readOGR(".","Tree_lines",stringsAsFactors = F)
# - Examine data (background information on Spatial data types)
summary(trh) # displays spatial data class, attribute data and other information
summary(trl)
# --> trh is SpatialPolygonsDataFrame; trl is SpatialPolygonsDataFrame
str(trh)
str(trl) # trl represents complex shapes - more complex datatype structure - see documentation for more details!
#
trl@polygons[[1]]@Polygons[[1]]@area # use list indexing and @ script to access data slots
gArea(trl,byid=T)[1]
# - e.g. area (in sq. metres) for polygon 1 in data set
#
plot(trl,col="wheat",border="wheat") # all tree lines
plot(trh,cex=0.01,add=T,col="red") # all tree heights
#
proj4string(trh) == proj4string(trl) # Check coordinate systems match! - required for further processes
anyNA(over(trh,trl)[,1]) # check if all spatial points fall inside trl polygons
#
all <- over(trh,trl) # over() - overlay between spatial data; returns attribute info of polygons (2nd argument)
# to contained query geometries e.g. points (1st argument)
#
trh$trl <- all$id # attach overlay data to new field - output of over() in row order of query data
#
### Plot polygons and points - not neccessary but shows the process has worked so far!
for(i in 1:length(trl$id)){
  plot(trl[i,])
  Sys.sleep(0.5)
  plot(trh[trh$trl==trl$id[i],],add=T)
  Sys.sleep(0.5)
}
# 
# - coordinates() displays coordinates for points, centroids of polygons/lines etc.
trh$X <- as.numeric(coordinates(trh)[,1]) # assign x coordinates for points to new field
trh$Y <- as.numeric(coordinates(trh)[,2]) # assign y coordinates
#
i <- 1
plot_flag <- T # demonstration only - set to FALSE for faster processing
plot_no <- 15 # how many processes to visualise - in the loop below
wtm <- 1 # time between plot processes (in seconds)
thies <- list() # list to store each spdf object from loop
#
if(is.logical(plot_flag)&is.numeric(plot_no)){
  for(i in 1:length(trl$id)){
    #
    W <- as(trl[i,],"owin") # sets local polygonal boundary for generating thiessen polygons
    #
    tp <- trh[trh$trl==trl[i,]$id,] # subset reqired points for each trl polygon
    #
    ap <- ppp(tp$X,tp$Y,window = W) # create point pattern dataset
    ap.dat <- as(dirichlet(ap),"SpatialPolygons") # creates dirichlet (Voronoi or Thiessen tessellation) - and converts to Spatial polygons datatype
    proj4string(ap.dat) <- proj4string(trl) # Set polygons to correct coordinate system
    #
    o <- over(ap.dat,trh) # attach point data to containing polygons
    ap.dat <- SpatialPolygonsDataFrame(ap.dat,data.frame(id=o$id,ht=o$ht,stringsAsFactors = F)) # spdf object with with polygons and attribute data
    #
    ap.dat$ht_class <- numeric(length(ap.dat$id)) # create new field for height class
    # ifelse statements to reclassify (quick fix - probably a better way to do this!)
    ap.dat$ht_class <- ifelse(ap.dat$ht < 2,1,2)
    ap.dat@data[ap.dat@data$ht>2,]$ht_class <- ifelse(ap.dat@data[ap.dat@data$ht>2,]$ht > 5,3,2)
    #
    for(j in 1:length(slot(ap.dat,"polygons")))slot(slot(ap.dat,"polygons")[[j]],"ID") <- ap.dat$id[j] # give each polygon object unique ID
    # plotting functions
    if(plot_flag == T & (i < (plot_no+1))){
      plot(trl[i,])
      Sys.sleep(wtm)
      plot(tp,add=T)
      Sys.sleep(wtm)
      plot(ap.dat,add=T,border="red")
      Sys.sleep(wtm)
      plot(ap.dat,add=T,border=ap.dat$ht_class,col=ap.dat$ht_class)
      Sys.sleep(wtm)
    }
    #
    thies[[i]] <- ap.dat # 
  }
} else {
  print("enter TRUE or FALSE value for plot_flag; enter numeric value for plot_no")
}
#
thies <- do.call(rbind,thies) # combine all thiessen polygons into 1 spatial polygons data set
#
plot(thies,col=thies$ht_class,border=thies$ht_class) # visualise data 
### Create single features for each class type
tmp_pol <- list()
#
for(i in 1:length(unique(thies$ht_class))) tmp_pol[[i]] <- gUnaryUnion(thies[thies$ht_class==i,]) # merge polygon features according to ht_class
#
tmp_pol <- do.call(bind,tmp_pol) # merge all polygons into one - total of three features for three ht_class values
#
trh_ext <- SpatialPolygonsDataFrame(tmp_pol,data=data.frame(id=c("1","2","3"),ht_class=c("Shrubs","Tall Shrubs","Trees"))) # create spdf by attaching tree class
# data for each polygon
#
writeOGR(trh_ext,".","Tree_class",driver = "ESRI Shapefile",overwrite_layer = T) # Export data









