library(raster)
library(maptools)

load("I:/BAM/BAMData/data_package_2016-04-18.Rdata")	
boreal <- raster("I:/GIS/basemaps/boreal_boundaries/naboreal4kmDD.asc")
provstate <- readShapeSpatial("I:/GIS/basemaps/province_state_line_DD.shp")
id <- raster("J:/CRUTS31/new_anomalies/id.asc")
nalc <- raster("I:/GIS/landcover/NALC/LandCover_IMG/NA_LandCover_2005/data/NA_LandCover_2005/NA_LandCover_2005_LCC.img")

ecoregion <- raster("L:/Boreal/InterannualVariability/boreal_ecoregions.asc")
m <- c(41.9, 42.1, 41,  45.9, 47.1, 41, 36.9, 37.1, 43, 32.9, 33.1, 32, 28.9, 29.1, 18, 5.9, 7.1, 5, 3.9, 4.1, 2, 15.9, 16.1, 15, 16.9, 17.1, 21, 9.9, 10.1, 14, 10.9, 12.1, 13, 0.9, 1.1, NA, 37.9, 38.1, NA) #combine ecoregions with little data
m2 <- c(45.9, 47.1, NA, 42.9, 43.1, NA, 37.9, 38.1, NA, 6.9, 7.1, NA, 3.9, 4.1, NA, 9.9, 11.1, NA, 12.9, 13.1, NA, 0.9, 1.1, NA) #drop ecoregions with less than 300 records
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rclmat2 <- matrix(m2, ncol=3, byrow=TRUE)
ecoreg2 <- reclassify(ecoregion, rclmat)
ecoreg3 <- reclassify(ecoregion, rclmat2)
writeRaster(ecoreg2, file="K:/Boreal/InterannualVariability/boreal_ecoregions_mod2.asc",overwrite=TRUE)
writeRaster(ecoreg3, file="K:/Boreal/InterannualVariability/boreal_ecoregions_drop.asc",overwrite=TRUE)

surveypts <- merge(SS[,1:4],PKEY[,c(1:5,7:8)], by=c("SS","PCODE"))
gridpoints <- rasterToPoints(id)

annclim1991 <- stack("K:/Boreal/InterannualVariability/annclim1991.grd")																
annclim1992 <- stack("K:/Boreal/InterannualVariability/annclim1992.grd")								
annclim1993 <- stack("K:/Boreal/InterannualVariability/annclim1993.grd")	
annclim1994 <- stack("K:/Boreal/InterannualVariability/annclim1994.grd")
annclim1995 <- stack("K:/Boreal/InterannualVariability/annclim1995.grd")
annclim1996 <- stack("K:/Boreal/InterannualVariability/annclim1996.grd")
annclim1997 <- stack("K:/Boreal/InterannualVariability/annclim1997.grd")
annclim1998 <- stack("K:/Boreal/InterannualVariability/annclim1998.grd")
annclim1999 <- stack("K:/Boreal/InterannualVariability/annclim1999.grd")
annclim2000 <- stack("K:/Boreal/InterannualVariability/annclim2000.grd")
annclim2001 <- stack("K:/Boreal/InterannualVariability/annclim2001.grd")	
annclim2002 <- stack("K:/Boreal/InterannualVariability/annclim2002.grd")
annclim2003 <- stack("K:/Boreal/InterannualVariability/annclim2003.grd")
annclim2004 <- stack("K:/Boreal/InterannualVariability/annclim2004.grd")
annclim2005 <- stack("K:/Boreal/InterannualVariability/annclim2005.grd")
annclim2006 <- stack("K:/Boreal/InterannualVariability/annclim2006.grd")
annclim2007 <- stack("K:/Boreal/InterannualVariability/annclim2007.grd")
annclim2008 <- stack("K:/Boreal/InterannualVariability/annclim2008.grd")
annclim2009 <- stack("K:/Boreal/InterannualVariability/annclim2009.grd")
annclim2010 <- stack("K:/Boreal/InterannualVariability/annclim2010.grd")
annclim2011 <- stack("K:/Boreal/InterannualVariability/annclim2011.grd")
annclim2012 <- stack("K:/Boreal/InterannualVariability/annclim2012.grd")
annclim2013 <- stack("K:/Boreal/InterannualVariability/annclim2013.grd")

XY1991 <- surveypts[surveypts$YEAR == 1991,]
XY1991<-cbind(XY1991,extract(annclim1991,as.matrix(cbind(XY1991[,3],XY1991[,4]))))
XY1992 <- surveypts[surveypts$YEAR == 1992,]
XY1992<-cbind(XY1992,extract(annclim1992,as.matrix(cbind(XY1992[,3],XY1992[,4]))))
XY1993 <- surveypts[surveypts$YEAR == 1993,]
XY1993<-cbind(XY1993,extract(annclim1993,as.matrix(cbind(XY1993[,3],XY1993[,4]))))
XY1994 <- surveypts[surveypts$YEAR == 1994,]
XY1994<-cbind(XY1994,extract(annclim1994,as.matrix(cbind(XY1994[,3],XY1994[,4]))))
XY1995 <- surveypts[surveypts$YEAR == 1995,]
XY1995<-cbind(XY1995,extract(annclim1995,as.matrix(cbind(XY1995[,3],XY1995[,4]))))
XY1996 <- surveypts[surveypts$YEAR == 1996,]
XY1996<-cbind(XY1996,extract(annclim1996,as.matrix(cbind(XY1996[,3],XY1996[,4]))))
XY1997 <- surveypts[surveypts$YEAR == 1997,]
XY1997<-cbind(XY1997,extract(annclim1997,as.matrix(cbind(XY1997[,3],XY1997[,4]))))
XY1998 <- surveypts[surveypts$YEAR == 1998,]
XY1998<-cbind(XY1998,extract(annclim1998,as.matrix(cbind(XY1998[,3],XY1998[,4]))))
XY1999 <- surveypts[surveypts$YEAR == 1999,]
XY1999<-cbind(XY1999,extract(annclim1999,as.matrix(cbind(XY1999[,3],XY1999[,4]))))
XY2000 <- surveypts[surveypts$YEAR == 2000,]
XY2000<-cbind(XY2000,extract(annclim2000,as.matrix(cbind(XY2000[,3],XY2000[,4]))))
XY2001 <- surveypts[surveypts$YEAR == 2001,]
XY2001<-cbind(XY2001,extract(annclim2001,as.matrix(cbind(XY2001[,3],XY2001[,4]))))
XY2002 <- surveypts[surveypts$YEAR == 2002,]
XY2002<-cbind(XY2002,extract(annclim2002,as.matrix(cbind(XY2002[,3],XY2002[,4]))))
XY2003 <- surveypts[surveypts$YEAR == 2003,]
XY2003<-cbind(XY2003,extract(annclim2003,as.matrix(cbind(XY2003[,3],XY2003[,4]))))
XY2004 <- surveypts[surveypts$YEAR == 2004,]
XY2004<-cbind(XY2004,extract(annclim2004,as.matrix(cbind(XY2004[,3],XY2004[,4]))))
XY2005 <- surveypts[surveypts$YEAR == 2005,]
XY2005<-cbind(XY2005,extract(annclim2005,as.matrix(cbind(XY2005[,3],XY2005[,4]))))
XY2006 <- surveypts[surveypts$YEAR == 2006,]
XY2006<-cbind(XY2006,extract(annclim2006,as.matrix(cbind(XY2006[,3],XY2006[,4]))))
XY2007 <- surveypts[surveypts$YEAR == 2007,]
XY2007<-cbind(XY2007,extract(annclim2007,as.matrix(cbind(XY2007[,3],XY2007[,4]))))
XY2008 <- surveypts[surveypts$YEAR == 2008,]
XY2008<-cbind(XY2008,extract(annclim2008,as.matrix(cbind(XY2008[,3],XY2008[,4]))))
XY2009 <- surveypts[surveypts$YEAR == 2009,]
XY2009<-cbind(XY2009,extract(annclim2009,as.matrix(cbind(XY2009[,3],XY2009[,4]))))
XY2010 <- surveypts[surveypts$YEAR == 2010,]
XY2010<-cbind(XY2010,extract(annclim2010,as.matrix(cbind(XY2010[,3],XY2010[,4]))))
XY2011 <- surveypts[surveypts$YEAR == 2011,]
XY2011<-cbind(XY2011,extract(annclim2011,as.matrix(cbind(XY2011[,3],XY2011[,4]))))
XY2012 <- surveypts[surveypts$YEAR == 2012,]
XY2012<-cbind(XY2012,extract(annclim2012,as.matrix(cbind(XY2012[,3],XY2012[,4]))))
XY2013 <- surveypts[surveypts$YEAR == 2013,]
XY2013<-cbind(XY2013,extract(annclim2013,as.matrix(cbind(XY2013[,3],XY2013[,4]))))


climdat <- rbind(XY1991, XY1992, XY1993, XY1994, XY1995, XY1996, XY1997, XY1998, XY1999, XY2000,
				 XY2001, XY2002, XY2003, XY2004, XY2005, XY2006, XY2007, XY2008, XY2009, XY2010, XY2011, XY2012, XY2013)
climdat$point <- 1		

combo <- merge(PCTBL[,2:5], climdat[,c(1,3:ncol(climdat))], by=c("PKEY", "SS"))
speclist <- read.csv("I:/BAM/BAMData/SpeciesClassesModv5.csv")
spec <- read.csv("I:/BAM/BAMData/species.csv")
speclist <- merge(speclist,spec[,c(1,4)], by.x="spp", by.y="SPECIES")
LDspec <- speclist[speclist$DATABASE_MIG_TYPE=="LD",c(1,8,17)]
LDspec <- LDspec[LDspec$Boreal10==1,]
combo <- merge(combo,LDspec, by.x="SPECIES",by.y="spp")

combo <-cbind(combo,extract(ecoreg2,as.matrix(cbind(combo[,5],combo[,6]))))
names(combo)[57] <- "ecoreg"
combo$ecoreg <- as.factor(as.character(combo$ecoreg))

XYDD <- combo[,5:6]
coordinates(XYDD) <- c("X", "Y")  # Identify spatial coordinate fields
proj4string(XYDD) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") 	# Define projection of coordinates
XYLCC <- as.data.frame(spTransform(XYDD, CRS(projection(nalc))))

combo <-cbind(combo[,1:57],extract(nalc,as.matrix(cbind(XYLCC[,1],XYLCC[,2]))))
names(combo)[58] <- "nalc"

combo <-cbind(combo,extract(ecoregion,as.matrix(cbind(combo[,5],combo[,6]))))
names(combo)[59] <- "ecoregion"
combo$ecoregion <- as.factor(as.character(combo$ecoregion))

combo <-cbind(combo,extract(ecoreg3,as.matrix(cbind(combo[,5],combo[,6]))))
names(combo)[60] <- "ecodrop"
combo$ecodrop <- as.factor(as.character(combo$ecodrop))

save.image("L:/Boreal/InterannualVariability/Summary19912013.RData")

records <- table(combo$ecoreg, combo$YEAR)
write.csv(as.data.frame(records), file="L:/Boreal/InterannualVariability/records_by_year.csv", row.names=FALSE)

origrecords <- table(combo$ecoregion, combo$YEAR)