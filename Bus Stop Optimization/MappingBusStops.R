#### Feature Engineering ####
# Let's assign geocoded data to new variables to save geocoded datasets from changes
EmplAddr <- geocoded
BusStopPoints <- geocodedBus

BusStopPoints<- unique(BusStopPoints, by = "id")

# Mapping spatial data points on Google Map ####
devtools::install_github("dkahle/ggmap")
devtools::install_github("hadley/ggplot2")
library(maps)
library(ggmap)
library(data.table)

# plotting the map with some points on it
#dev.off()
SFmap  <- get_map(location = 'San Francisco, USA', zoom = 12,
                  maptype = "roadmap")

ggmap(SFmap) + geom_point(data=EmplAddr, aes(x=long, y=lat, colour = "red")) + scale_color_discrete("Employee Addresses")


# Clustering data points ####
library(geosphere)

geo.dist = function(df) {
  require(geosphere)
  d <- function(i,z){         # z[1:2] contain long, lat
    dist <- rep(0,nrow(z))
    dist[i:nrow(z)] <- distHaversine(z[i:nrow(z),1:2],z[i,1:2])
    return(dist)
  }
  dm <- do.call(cbind,lapply(1:nrow(df),d,df))
  return(as.dist(dm))
}

df<- na.omit(geocoded)

summary(df)

# Clustering with Hclust 
clusters = cutree(hclust(dist(df[, 1:2],method = "manhattan")), k=10) # get 10 clusters

plot(clusters)

# function to find medoid in cluster if
clust.centroid = function(i, dat, clusters) {
  ind = (clusters == i)
  colMeans(dat[ind,])
}

sd<- sapply(unique(clusters), clust.centroid, df[, 1:2], clusters)
sd<- t(sd)
sd <- as.data.frame(sd)
df$clust <- clusters

# Mapping our clusters on the map
MapSF <- get_map(location = 'San Francisco, USA', zoom = 12,
                 maptype = "roadmap")

ggmap(MapSF) + geom_point(data = df, aes(x = long, y = lat, colour = factor(clust)) )+ scale_color_discrete("Clusters") +
  geom_point(data=sd, aes(x=long, y=lat), color="black")


## Finding closest Bus Stops to Cluster's Centroids
library(dplyr)
DB1 <- data.frame(location_id=1:10,LATITUDE=sd$lat,LONGITUDE=sd$long)
DB2 <- data.frame(location_id=11:130,LATITUDE=BusStopPoints$lat,LONGITUDE=BusStopPoints$long)


DistFun <- function(ID)
{
  TMP <- DB1[DB1$location_id==ID,]
  TMP1 <- distGeo(TMP[,3:2],DB2[,3:2])
  TMP2 <- data.frame(DB1ID=ID,DB2ID=DB2[which.min(TMP1),1],DistanceBetween=min(TMP1)      ) 
  print(ID)
  return(TMP2)
}

DistanceMatrix <- bind_rows(lapply(DB1$location_id, DistFun))
DistanceMatrix

library(magrittr)
busStopID <- rownames(DB2[DB2$location_id %in% DistanceMatrix$DB2ID , ])
busStopID

# Getting optimal bus stop intersections
# Detecting NAs and reordering observations
NewBusStop <-BusStops[c(as.numeric(busStopID)), ] 
rownames(NewBusStop) <- 1:nrow(NewBusStop)
NewBusStop$Intersection

#BusStopIntersections <- NewBusStop[rownames(NewBusStop) %in% busStopID,]$Intersection
#BusStopIntersections

#finally write it all to the output files
#path <- "/Users/bekterra/Desktop/Hackatons/Bus Stop Optimization/Bus/PotentailBus.csv"
#saveRDS(BusStopIntersections, paste0(path ,".rds"))
#write.table(BusStopIntersections, file=paste0(path ,".csv"), sep=",", row.names=FALSE)



# Plotting potential 10 bus stops
MapSF <- get_map(location = 'San Francisco, USA', zoom = 12,
                 maptype = "roadmap")

ggmap(MapSF) + geom_point(data = df, aes(x = long, y = lat, colour = factor(clust)) )+ scale_color_discrete("Clusters") +
  geom_point(data=sd, aes(x=long, y=lat), color="black")



MapSF <- get_map('San Francisco, USA', zoom = 12, maptype = "roadmap", legend = 'topright')

MapSF + geom_point(aes(x = long, y = lat, colour = factor(clust)), data = df )+ scale_color_discrete("Clusters") +
  geom_point(data=locations, aes(x=lon, y=lat), shape=23, fill="black", color="darkred",size=3)
geom_label(aes(fill = "black"), colour = "white", fontface = "bold")
