library(raster)
library(maptools)

wintid <- raster("G:/Boreal/InterannualVariability/ceclevel2nsa1.tif")

annclim2000 <- stack("G:/Boreal/InterannualVariability/annclim2000.grd")
annclim2001 <- stack("G:/Boreal/InterannualVariability/annclim2001.grd")	
annclim2002 <- stack("G:/Boreal/InterannualVariability/annclim2002.grd")
annclim2003 <- stack("G:/Boreal/InterannualVariability/annclim2003.grd")
annclim2004 <- stack("G:/Boreal/InterannualVariability/annclim2004.grd")
annclim2005 <- stack("G:/Boreal/InterannualVariability/annclim2005.grd")
annclim2006 <- stack("G:/Boreal/InterannualVariability/annclim2006.grd")
annclim2007 <- stack("G:/Boreal/InterannualVariability/annclim2007.grd")
annclim2008 <- stack("G:/Boreal/InterannualVariability/annclim2008.grd")
annclim2009 <- stack("G:/Boreal/InterannualVariability/annclim2009.grd")
annclim2010 <- stack("G:/Boreal/InterannualVariability/annclim2010.grd")
annclim2011 <- stack("G:/Boreal/InterannualVariability/annclim2011.grd")
annclim2012 <- stack("G:/Boreal/InterannualVariability/annclim2012.grd")
annclim2013 <- stack("G:/Boreal/InterannualVariability/annclim2013.grd")

annclim2000 <- raster::resample(annclim2000,wintid)
z00 <- as.data.frame(zonal(annclim2000, wintid, fun='mean', na.rm=TRUE))
annclim2001 <- raster::resample(annclim2001,wintid)
z01 <- as.data.frame(zonal(annclim2001, wintid, fun='mean', na.rm=TRUE))
annclim2002 <- raster::resample(annclim2002,wintid)
z02 <- as.data.frame(zonal(annclim2002, wintid, fun='mean', na.rm=TRUE))
annclim2003 <- raster::resample(annclim2003,wintid)
z03 <- as.data.frame(zonal(annclim2003, wintid, fun='mean', na.rm=TRUE))
annclim2004 <- raster::resample(annclim2004,wintid)
z04 <- as.data.frame(zonal(annclim2004, wintid, fun='mean', na.rm=TRUE))
annclim2005 <- raster::resample(annclim2005,wintid)
z05 <- as.data.frame(zonal(annclim2005, wintid, fun='mean', na.rm=TRUE))
annclim2006 <- raster::resample(annclim2006,wintid)
z06 <- as.data.frame(zonal(annclim2006, wintid, fun='mean', na.rm=TRUE))
annclim2007 <- raster::resample(annclim2007,wintid)
z07 <- as.data.frame(zonal(annclim2007, wintid, fun='mean', na.rm=TRUE))
annclim2008 <- raster::resample(annclim2008,wintid)
z08 <- as.data.frame(zonal(annclim2008, wintid, fun='mean', na.rm=TRUE))
annclim2009 <- raster::resample(annclim2009,wintid)
z09 <- as.data.frame(zonal(annclim2009, wintid, fun='mean', na.rm=TRUE))
annclim2010 <- raster::resample(annclim2010,wintid)
z10 <- as.data.frame(zonal(annclim2010, wintid, fun='mean', na.rm=TRUE))
annclim2011 <- raster::resample(annclim2011,wintid)
z11 <- as.data.frame(zonal(annclim2011, wintid, fun='mean', na.rm=TRUE))
annclim2012 <- raster::resample(annclim2012,wintid)
z12 <- as.data.frame(zonal(annclim2012, wintid, fun='mean', na.rm=TRUE))
annclim2013 <- raster::resample(annclim2013,wintid)
z13 <- as.data.frame(zonal(annclim2013, wintid, fun='mean', na.rm=TRUE))

z00$YEAR <- 2000
z01$YEAR <- 2001
z02$YEAR <- 2002
z03$YEAR <- 2003
z04$YEAR <- 2004
z05$YEAR <- 2005
z06$YEAR <- 2006
z07$YEAR <- 2007
z08$YEAR <- 2008
z09$YEAR <- 2009
z10$YEAR <- 2010
z11$YEAR <- 2011
z12$YEAR <- 2012
z13$YEAR <- 2013
zz <- rbind(z00, z01, z02, z03, z04, z05, z06, z07, z08, z09, z10, z11, z12, z13)
write.csv(zz, file="G:/Boreal/InterannualVariability/annclimDat19912013_CEC.csv", row.names=FALSE)

