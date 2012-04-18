#######################################################################################
####### Examples of the osmar-presentation at the Fossgis2012 in Dessau(Germany)#######
#######################################################################################

library("osmar")
library("fBasics")
library("plotrix")

par(ask=TRUE)
readline("Press <Enter> to continue")

#### Download (or load) 4 km² (2*2 km) bounding box from dessau
### downloading data from osm api
src<- osmsource_api()
bb<-center_bbox(12.232221,51.840577, 2000,2000)
bb<-center_bbox(-100.318909, 25.691038, 2000,2000)
dessau<-get_osm(bb, source=src)
###

### download pre-computed data from website
download.file("http://osmar.r-forge.r-project.org/fossgis2012.RData",
              "fossgis2012.RData")
load("fossgis2012.RData")

dessau

#### structure of dessau with cut-out of examples
str(dessau,2,give.attr=FALSE)

dessau$nodes$attrs[1:6,]
dessau$ways$tags[1:4,]
dessau$ways$refs[1:4,]

#### summary of osmar-object
summary(dessau$nodes)
summary(dessau$ways)
summary(dessau$relations)
summary(dessau)$ways$keyval[1:6,]

par(ask=TRUE)
readline("Press <Enter> to continue")


###### making of the benchmap   ############################

#### function for reducing osmar object to original area
reduce_osmar<- function(object, boundbox){
  id<- find(object,
            node(attrs(lon>=boundbox[["left"]] & lon<=boundbox[["right"]] & lat>=boundbox[["bottom"]] & lat<=boundbox[["top"]])))
  ids<- find_up(object, node(id))
  ret<-subset(object, ids=ids)
  ret
}

#### defining original bounding box and
#### reducing the osmar object to this bounding box
bb<-center_bbox(12.232221,51.840577, 2000,2000)
red_dessau<-reduce_osmar(dessau,bb)
summary(red_dessau)

par(ask=TRUE)
readline("Press <Enter> to continue")

## 1) find amenity=bench in nodes
## 2) subset of these nodes ->new osmar-object
## 3) find ways with maxspeed=30
## 4) find corresponding nodes
## 5) subset of these nodes/ways ->new osmar-object
bench_ids<- find(red_dessau, node(tags(k=="highway" & v=="residential")))
bench_dessau<-subset(red_dessau, node_ids=bench_ids)
footway_ids<- find(red_dessau,way(tags(k=="highway" & v=="oneway")))
footway_ids<- find_down(red_dessau, way(footway_ids))
footway_dessau<-subset(red_dessau,ids=footway_ids)

par(ask=TRUE)
readline("Press <Enter> to continue")

## stepwise plotting of benchmap
## green (ways with maxspeed=30), red (bench), blue (location of Fossgis2012)
plot_ways(red_dessau, col=gray(0.4))
title("benches in dessau")
plot_ways(footway_dessau, col="darkgreen", add=TRUE, lwd=2)
plot_nodes(bench_dessau, col=2, add=TRUE, pch=8)
points(12.228295,51.840113, col="blue", pch=8, cex=2)

par(ask=TRUE)
readline("Press <Enter> to continue")


##### data.frame expansion with osmar-package ##########################
##### bench example                           ##########################

## finding benches and saving in data.frame with lon and lat variables
bench_ids<- find(dessau, node(tags(k=="amenity" & v=="bench")))
bench_dessau<-subset(dessau, node_ids=bench_ids)
bench_coords<-data.frame(id=bench_ids, lon=bench_dessau$nodes$attrs$lon, lat=bench_dessau$nodes$attrs$lat)
bench_coords[1:10,]

## downloading 2500m² bounding box around every bench
##
## bench_list<-vector("list", nrow(bench_coords))
## for(i in 1:nrow(bench_coords)){
##  tmp_bb<-center_bbox(bench_coords[i,"lon"],bench_coords[i,"lat"], 50,50)
##  tmp_osm<-get_osm(tmp_bb, source=src)
##  bench_list[[i]] <- tmp_osm
## }

bench_list[1:5]

par(ask=FALSE)
readline("Press <Enter> to continue")

## building new data.frame with counts of street-types and buildings
## for every data point

bench_data<-bench_coords
for(i in 1:nrow(bench_data)){
  bench_data$footway[i]<-summary(bench_list[[i]]$ways$tags$v)["footway"]
  bench_data$secondary[i]<-summary(bench_list[[i]]$ways$tags$v)["secondary"]
  bench_data$primary[i]<-summary(bench_list[[i]]$ways$tags$v)["primary"]
  bench_data$residential[i]<-summary(bench_list[[i]]$ways$tags$v)["residential"]
  bench_data$building[i]<-summary(bench_list[[i]]$ways$tags$k)["building"]
}
bench_data[is.na(bench_data)]<-0
head(bench_data)

par(ask=TRUE)
readline("Press <Enter> to continue")
##### Node count map ###################################################
#### red_dessau2: 10km x 10km area of dessau reduced to 10km x 10km
#### sp_dessau2: sp object as a result of as_sp(red_dessau2)

#### building a grid and making an osmar-object for every grid-cell ->bboxlist
summary(red_dessau2$nodes)$bbox->box
long <- seq(box[1,2], box[2,2], length.out=35)
lat <- seq(box[1,1], box[2,1], length.out=35)

coordslist <- vector("list", 34*34)
i <- 1
for(lo in 1:(length(long)-1)){
  for(la in 1:(length(lat)-1)){
    coordslist[[i]] <- c(left=long[lo],right=long[lo+1], bottom=lat[la],top=lat[la+1])
    i<-i+1
  }
}

bboxlist<-vector("list", 34*34)
for(i in 1:length(bboxlist)){
  bboxlist[[i]] <- reduce_osmar(red_dessau2, coordslist[[i]])
}


#### counting nodes in every grid.cell
nodecounts<-NA
for(i in 1:length(bboxlist))
  nodecounts[i]<- summary(bboxlist[[i]]$nodes)$n[["nodes"]]


#### plotting the map with help of the graphic packages
library(fBasics)
library(plotrix)
plot(sp_dessau2[[1]], col="white", axes=TRUE)
colors <- seqPalette(max(nodecounts)+1, "Reds")
for(i in 1:length(bboxlist)){
  tmp<-coordslist[[i]]
  rect(tmp[1],tmp[3],tmp[2],tmp[4], border=NA,angle=180, col=colors[summary(bboxlist[[i]]$nodes)$n[["nodes"]]])
}

color.legend(12.15, 51.891, 12.31, 51.894, rect.col=colors,
              legend=c(0,rep("",99),100,rep("",99),200,rep("",99),300, rep("",99),400,rep("",45)))

#### for orientation buildings and the river "Elbe" are plotted
build_id <- find(red_dessau2, way(tags(k=="building")))
build_ids<- find_down(red_dessau2, ids=way(build_id))
build_dessau2<-subset(red_dessau2, ids=build_ids)
plot_ways(build_dessau2, col=gray(0.6),add=TRUE)

elb_id<-find(red_dessau2, way(tags(v=="Elbe")))
elb_ids<-find_down(red_dessau2, ids=way(elb_id))
elbe<-subset(red_dessau2, ids=elb_ids)
plot_ways(elbe, col="lightblue", add=TRUE, lwd=2)

title("Count of Nodes in Dessau Area with buildings and the Elbe", line=3)




library(maptools)
library(maps)
library(mapdata)
library(rjson)
library(stringr)
library(RCurl)
library(ggplot2)


GetDirections <- function(from, to) {
  Sys.sleep(1) #let's be nice internet citizens and wait 1s
  baseurl <- "http://maps.google.com/maps/nav?output=js&q=from:"
  url <- str_c(baseurl, curlEscape(str_c(
                                         str_c(" ", from),
                                         str_c(" to: ", to, collapse = " ")
                                         )))
  fromJSON(paste(readLines(url), collapse=""))
}

#Google polyline decoder borrowed from:
#http://facstaff.unca.edu/mcmcclur/GoogleMaps/EncodePolyline/decode.js
DecodeLineR <- function(encoded) {
  len = str_length(encoded)
  encoded <- strsplit(encoded, NULL)[[1]]
  index = 1
  N <- 100000
  df.index <- 1
  array = matrix(nrow = N, ncol = 2)
  lat = 0
  dlat = 0
  lng = 0
  dlng = 0
  b = 0
  shift = 0
  result = 0

  while(index <= len) {
    shift = 0
    result = 0
    
    repeat {
      b = as.integer(charToRaw(encoded[index])) - 63
      index <- index + 1
      result = bitOr(result, bitShiftL(bitAnd(b, 0x1f), shift))
      shift = shift + 5
      if(b < 0x20) break
    }
    dlat = ifelse(bitAnd(result, 1),
                 -(result - (bitShiftR(result, 1))),
                 bitShiftR(result, 1))
    lat = lat + dlat;
    
    shift = 0
    result = 0
    b = 0
    repeat {
      b = as.integer(charToRaw(encoded[index])) - 63
      index <- index + 1
      result = bitOr(result, bitShiftL(bitAnd(b, 0x1f), shift))
      shift = shift + 5
      if(b < 0x20) break
    }
    dlng = ifelse(bitAnd(result, 1),
                  -(result - (bitShiftR(result, 1))),
                  bitShiftR(result, 1))
    lng = lng + dlng

    array[df.index,] <- c(lat = lat * 1e-05, lng = lng * 1e-5)
    df.index <- df.index + 1
  }
  
  ret <- data.frame(array[1:df.index - 1,])
  names(ret) <- c("lat", "lng")
  return(ret)
}

src <- '
  std::string encoded = as<std::string>(a);

  int index = 0;
  int len = encoded.size();
  int df_index = 0;
  long double lat = 0;
  long double lng = 0;
  std::vector<long double> longitude(0);
  std::vector<long double> latitude(0);

  if(encoded.size() == 0)
    return R_NilValue;

  longitude.reserve(30000);
  latitude.reserve(30000);

  while(index < len) {
   int b;
   int shift = 0;
   int result = 0;
   
   do {
     b = encoded[index++] - 63;
     result |= (b & 0x1f) << shift;
     shift += 5;
   } while(b >= 0x20);
   long double dlat = ((result & 1) ? ~(result >> 1) : (result >> 1));
   lat += dlat;
   latitude.push_back(lat * 1e-5);

   shift = 0;
   result = 0;
   do {
     b = encoded[index++] - 63;
     result |= (b & 0x1f) << shift;
     shift += 5;
   } while(b >= 0x20);
   long double dlng = ((result & 1) ? ~(result >> 1) : (result >> 1));
   lng += dlng;
   longitude.push_back(lng * 1e-5);
   df_index++;
 }

 return DataFrame::create( _["lat"] = latitude,  _["lng"] = longitude );
'
  

if (require("Rcpp") & require("inline")){
  DecodeLine <- cxxfunction(signature(a = "character"),
                   src, plugin = "Rcpp")
} else {
  DecodeLine <- DecodeLineR
}

#http://code.google.com/apis/maps/documentation/utilities/polylinealgorithm.html
poly.official <- data.frame(lat = c(38.5, 40.7, 43.252),
                   lng = c(-120.2, -120.95, -126.453))

all.equal(DecodeLine("_p~iF~ps|U_ulLnnqC_mqNvxq`@"),
          poly.official)
all.equal(DecodeLineR("_p~iF~ps|U_ulLnnqC_mqNvxq`@"),
          poly.official)

identical(DecodeLine("_p~iF~ps|U_ulLnnqC_mqNvxq`@"),
          DecodeLineR("_p~iF~ps|U_ulLnnqC_mqNvxq`@"))



library(compiler)
DecodeLineRC <- cmpfun(DecodeLineR)

library(rbenchmark)

benchmark(DecodeLine("_p~iF~ps|U_ulLnnqC_mqNvxq`@"),
          DecodeLineR("_p~iF~ps|U_ulLnnqC_mqNvxq`@"),
          DecodeLineRC("_p~iF~ps|U_ulLnnqC_mqNvxq`@"),
          columns=c("test", "replications", "elapsed", "relative"),
          order="relative", replications=1000)


#from Acapulco to Reynosa
dir <- GetDirections(from = "Acapulco, Mexico",
                     to = c("Mexico city, Mexico",
                            "Juarez, Chihuahua, Mexico"))
dir$Directions$Duration$html
dir$Directions$Distance$meters

system.time(dir$Decodedline <- DecodeLine(dir$Directions$Polyline$points))
#The R version is slow
system.time(dir$Decodedline <- DecodeLineR(dir$Directions$Polyline$points))

#Draw a map with the optimal highway
mexico <- data.frame(map("worldHires", "mexico", plot=FALSE)[c("x","y")])
mexico.map <- qplot(x, y, data=mexico, geom="path") + coord_map()
mexico.map + geom_path(data = dir$Decodedline, aes(lng, lat))
