library(raster)
library(glmertree)
library(data.table)
library(reshape)
library(stringr)
library(ggplot2)
library(rasterVis)
require(gridExtra) # also loads grid
require(lattice)
library(maptools)
library(rgdal)

clust <- raster("G:/Boreal/InterannualVariability/boreal_ecoregions_drop.asc")
prov <- rgdal::readOGR("E:/GIS/basemaps/province_state.shp")
boreal <- rgdal::readOGR("G:/Boreal/InterannualVariability/boreal_ecoregions.shp")
wintid <- raster("G:/Boreal/InterannualVariability/ceclevel2nsa1.tif")

is_try_error <- function(x) inherits(x, "try-error")
intersect <- function(x, y) y[match(x, y, nomatch = 0)]
rainbow15 <- c("grey","black","brown","maroon","red","orange","yellow","lightgreen","green","darkgreen","turquoise","blue","purple","violet","pink")
indices <- read.csv("G:/Boreal/InterannualVariability/nao_pdo_soi_amo_annual.csv")
ind <- indices[,c(1:25,32:35,44:47)]

diverge0 <- function(p, ramp) {
  # p: a trellis object resulting from rasterVis::levelplot
  # ramp: the name of an RColorBrewer palette (as character), a character 
  #       vector of colour names to interpolate, or a colorRampPalette.
  require(RColorBrewer)
  require(rasterVis)
  if(length(ramp)==1 && is.character(ramp) && ramp %in% 
     row.names(brewer.pal.info)) {
    ramp <- suppressWarnings(colorRampPalette(brewer.pal(11, ramp)))
  } else if(length(ramp) > 1 && is.character(ramp) && all(ramp %in% colors())) {
    ramp <- colorRampPalette(ramp)
  } else if(!is.function(ramp)) 
    stop('ramp should be either the name of a RColorBrewer palette, ', 
         'a vector of colours to be interpolated, or a colorRampPalette.')
  rng <- range(p$legend[[1]]$args$key$at)
  s <- seq(-max(abs(rng)), max(abs(rng)), len=1001)
  i <- findInterval(rng[which.min(abs(rng))], s)
  zlim <- switch(which.min(abs(rng)), `1`=F:(1000+1), `2`=1:(i+1))
  p$legend[[1]]$args$key$at <- s[zlim]
  p$par.settings$regions$col <- ramp(1000)[zlim[-length(zlim)]]
  p
}

speclist <- read.csv("F:/BAM/BAMData/SpeciesClassesModv5.csv")
spec <- read.csv("F:/BAM/BAMData/species.csv")
speclist <- merge(speclist,spec[,c(1,4)], by.x="spp", by.y="SPECIES")
speclist$spp <- gsub("YWAR","YEWA",speclist$spp)
speclist <- speclist[speclist$spp != "PIWA",]
LDspec <- as.factor(as.character(speclist[speclist$DATABASE_MIG_TYPE=="LD",1]))

zz <- read.csv("G:/Boreal/InterannualVariability/ClusterClimDat19912013_drop.csv")
zz <- zz[zz$YEAR > 2000,]
ll <- read.csv("G:/Boreal/InterannualVariability/BreedingLanduseDat20012014_drop.csv")	
ll <- ll[ll$YEAR > 2000,]
names(ll) <- c("zone","forestchg","YEAR")
yy <- read.csv("G:/Boreal/InterannualVariability/WinterLanduseDat20012014_CEC.csv")

ecolu <- read.csv("G:/Boreal/InterannualVariability/ecoregion_lu.csv") 
ecolu$BorealLevel3 <- as.factor(as.character(ecolu$BorealLevel3))

trend <- data.frame(SPECIES="",ecodrop=0,betaw=0, beta=0, intw=0, int=0, blcw=0, blc=0)
trend <- trend[trend$ecodrop != 0,]
	for (j in 1:length(LDspec)) {
		for (k in ecolu$BorealLevel3) {	
			 pred <- read.csv(paste("G:/Boreal/InterannualVariability/ecopred_2000-2013",LDspec[j],"_update.csv",sep=""))
			 pred <- pred[pred$ecodrop==k,]
				pred <- pred[pred$YEAR > 2000,]
				x1 <- try(trend.lm <- lm(pred$pred ~ pred$YEAR + pred$nalc, weights=pred$x))
				x2 <- try(trend.lm2 <- lm(pred$pred ~ pred$YEAR + pred$nalc))
				if (class(x1) != "try-error") {
				trend1 <- data.frame(SPECIES=as.character(LDspec[j]),ecodrop=as.numeric(as.character(k)),betaw=trend.lm$coefficients[2], beta=trend.lm2$coefficients[2], intw=trend.lm$coefficients[1], int=trend.lm2$coefficients[1], blcw=trend.lm$coefficients[3], blc=trend.lm2$coefficients[3])
				trend <- rbind(trend,trend1)
			}
		}
	}	
write.csv(trend, file="G:/Boreal/InterannualVariability/trends_weighted_2000_update.csv", row.names=FALSE)


for (j in 1:length(LDspec)) {
	spectrend <- trend[trend$SPECIES == LDspec[j],]
		rc <- ratify(clust)
		ratc <- levels(rc)[[1]]
		ratc <- merge(ratc,spectrend[,c(2,3)],by.x="ID",by.y="ecodrop")
		ratc$betaw <- round(ratc$betaw, 3)
		levels(rc) <- ratc
		rc1 <- deratify(rc,att='betaw')
		plot1 <- levelplot(rc1, att='betaw', main = paste(LDspec[j], sep=" "), margin=FALSE) + layer(sp.polygons(prov))
		#grid.arrange(diverge0(plot1,'RdBu'), ncol=1, nrow=1)
	}	

jultmin <- data.frame(jultmin="",zone=0,beta=0)
jultmin <- jultmin[jultmin$zone != 0,]
	for (k in ecolu$BorealLevel3) {	
		pred <- zz[zz$zone==k,]
		pred <- pred[pred$YEAR > 2000,]
		pred <- zz[zz$zone==k,]
		pred <- pred[pred$YEAR > 2000,]
		x1 <- try(jultmin.lm <- lm(pred$jultmin0 ~ pred$YEAR))
		if (class(x1) != "try-error") {
		jultmin1 <- data.frame(zone=k,beta=jultmin.lm$coefficients[2])
		jultmin <- rbind(jultmin,jultmin1)
		}
	}
	
jultmax <- data.frame(jultmax="",zone=0,beta=0)
jultmax <- jultmax[jultmax$zone != 0,]
	for (k in ecolu$BorealLevel3) {	
		pred <- zz[zz$zone==k,]
		pred <- pred[pred$YEAR > 2000,]
		pred <- zz[zz$zone==k,]
		pred <- pred[pred$YEAR > 2000,]
		x1 <- try(jultmax.lm <- lm(pred$jultmax0 ~ pred$YEAR))
		if (class(x1) != "try-error") {
		jultmax1 <- data.frame(zone=k,beta=jultmax.lm$coefficients[2])
		jultmax <- rbind(jultmax,jultmax1)
		}
	}
	
juntmin <- data.frame(juntmin="",zone=0,beta=0)
juntmin <- juntmin[juntmin$zone != 0,]
	for (k in ecolu$BorealLevel3) {	
		pred <- zz[zz$zone==k,]
		pred <- pred[pred$YEAR > 2000,]
		pred <- zz[zz$zone==k,]
		pred <- pred[pred$YEAR > 2000,]
		x1 <- try(juntmin.lm <- lm(pred$juntmin0 ~ pred$YEAR))
		if (class(x1) != "try-error") {
		juntmin1 <- data.frame(zone=k,beta=juntmin.lm$coefficients[2])
		juntmin <- rbind(juntmin,juntmin1)
		}
	}
	
juntmax <- data.frame(juntmax="",zone=0,beta=0)
juntmax <- juntmax[juntmax$zone != 0,]
	for (k in ecolu$BorealLevel3) {	
		pred <- zz[zz$zone==k,]
		pred <- pred[pred$YEAR > 2000,]
		pred <- zz[zz$zone==k,]
		pred <- pred[pred$YEAR > 2000,]
		x1 <- try(juntmax.lm <- lm(pred$juntmax0 ~ pred$YEAR))
		if (class(x1) != "try-error") {
		juntmax1 <- data.frame(zone=k,beta=juntmax.lm$coefficients[2])
		juntmax <- rbind(juntmax,juntmax1)
		}
	}

sample <- data.frame(ecodrop=0,count=0)
sample <- sample[sample$ecodrop !=0,]
for (k in ecolu$BorealLevel3) {	
	pred <- read.csv(paste("G:/Boreal/InterannualVariability/ecopred_2000-2013",LDspec[1],"_update.csv",sep=""))
	pred <- pred[pred$ecodrop==k,]
	pred <- pred[pred$YEAR>2000,]
	sample1 <- aggregate(pred$x, by=list(pred$ecodrop), FUN="sum")
	sample1$logcount <- log(sample1$x + 1)
	names(sample1) <- c("ecodrop","count","logcount")
	sample <- rbind(sample,sample1)
	}
sample$ecodrop <- as.numeric(sample$ecodrop)
rs <- ratify(clust)
rats <- levels(rs)[[1]]	
rats <- merge(rats,sample,by.x="ID",by.y="ecodrop")
levels(rs) <- rats
rs1 <- deratify(rs,att='logcount')	
plots <- levelplot(rs1,att='count',main="log(sample size)", margin=FALSE) + layer(sp.polygons(prov))
pdf(file=paste("G:/Boreal/InterannualVariability/samplesize_update.pdf",sep=""), height=7, width=10)
plot(plots)
dev.off()
write.csv(sample, file="G:/Boreal/InterannualVariability/sample_2000_update.csv")


plote <- levelplot(rs,att='ID',main="ecoregions", margin=FALSE) + layer(sp.polygons(prov))

forest <- aggregate(ll$forestchg, by=list(ll$zone), FUN="sum")
names(forest) <- c("ID","forestchg")
forest$ID <- as.numeric(forest$ID)
fc <- merge(ratc,forest,by="ID")
fc$forestchg <- round(fc$forestchg, 5)
rcf <- rc
levels(rcf) <- fc
fc1 <- deratify(rcf,att='forestchg')
plotf <- levelplot(-1*fc1, att='forestchg', main = "forest change", margin=FALSE) + layer(sp.polygons(prov))

wwforest <- aggregate(yy$mean, by=list(yy$zone), FUN="sum")
names(wwforest) <- c("ID","forestchg")
rcw <- ratify(wintid)
ratcw <- levels(rcw)[[1]]
fcw <- merge(ratcw,wwforest,by="ID")
fcw$forestchg <- round(fcw$forestchg, 3)
rcfw <- rcw
levels(rcfw) <- fcw
fcw1 <- deratify(rcfw,att='forestchg')
fcw1 <- trim(fcw1)
plotfw <- levelplot(-1*fcw1, att='forestchg', main = "forest change", margin=FALSE) 
pdf(file=paste("G:/Boreal/InterannualVariability/winter_forestloss_2000.pdf",sep=""), height=7, width=10)
plot(plotfw)
dev.off()

clim <- cbind(juntmin,"juntmax"=juntmax[,2],"jultmin"=jultmin[,2],"jultmax"=jultmax[,2])
names(clim)[2] <- "juntmin"
names(clim)[1] <- "ID"
cc <- merge(ratc,clim,by="ID")
cc <- round(cc,3)
rcc <- rc
levels(rcc) <- cc
ccjuntmin <- deratify(rcc,att='juntmin')
plotjuntmin <- levelplot(ccjuntmin, att='juntmin', main = "juntmin change", margin=FALSE) + layer(sp.polygons(prov))
ccjuntmax <- deratify(rcc,att='juntmax')
plotjuntmax <- levelplot(ccjuntmax, att='juntmax', main = "juntmax change", margin=FALSE) + layer(sp.polygons(prov))
ccjultmin <- deratify(rcc,att='jultmin')
plotjultmin <- levelplot(ccjultmin, att='jultmin', main = "jultmin change", margin=FALSE) + layer(sp.polygons(prov))
ccjultmax <- deratify(rcc,att='jultmax')
plotjultmax <- levelplot(ccjultmax, att='jultmax', main = "jultmax change", margin=FALSE) + layer(sp.polygons(prov))


pdf(file=paste("G:/Boreal/InterannualVariability/_trends_continuous_weighted_2000_update.pdf",sep=""), height=7, width=10)
for (j in 1:length(LDspec)) {
	spectrend <- trend[trend$SPECIES == LDspec[j],]
	rc <- ratify(clust)
	ratc <- levels(rc)[[1]]
	ratc <- merge(ratc,spectrend[,c(2,3)],by.x="ID",by.y="ecodrop")
	ratc$betaw <- round(ratc$betaw, 3)
	levels(rc) <- ratc
	rc1 <- deratify(rc,att='betaw')
	plot1 <- levelplot(rc1, att='betaw', main = paste(LDspec[j],"trend", sep=" "), margin=FALSE) + layer(sp.polygons(prov))
	grid.arrange(diverge0(plot1,'RdBu'), ncol=1, nrow=1)
	}	
	dev.off()
	
pdf(file=paste("G:/Boreal/InterannualVariability/trends_forestchg_2000_update.pdf",sep=""), height=7, width=10)
par(mfcol=c(3,3), oma=c(0,0,0,0))
for (j in 1:length(LDspec)) {
	spectrend <- trend[trend$SPECIES == LDspec[j],]
	tf <- merge(spectrend, forest, by.x="ecodrop",by.y="ID")
	plot(tf$forestchg, tf$betaw, 	xlab="Forest loss proportion ", ylab="2001-2013 Trend" , main=as.character(LDspec[j]))
	lines(lowess(tf$forestchg,tf$betaw), col="blue") # lowess line (x,y)
	abline(lm(tf$betaw~tf$forestchg), col="red") # regression line (y~x) 
}
dev.off()

pdf(file=paste("G:/Boreal/InterannualVariability/trends_forestchgcombo_2000_update.pdf",sep=""), height=7, width=10)
	tf <- merge(trend, forest, by.x="ecodrop",by.y="ID")
	plot(tf$forestchg, tf$betaw, 	xlab="Forest loss proportion ", ylab="2001-2013 Trend")
	lines(lowess(tf$forestchg,tf$betaw), col="blue") # lowess line (x,y)
	abline(lm(tf$betaw~tf$forestchg), col="red") # regression line (y~x)
dev.off()

pdf(file=paste("G:/Boreal/InterannualVariability/trends_julmaxt_2000_update.pdf",sep=""), height=7, width=10)
par(mfcol=c(3,3), oma=c(0,0,0,0))
for (j in 1:length(LDspec)) {
	spectrend <- trend[trend$SPECIES == LDspec[j],]
	tf <- merge(spectrend, clim, by.x="ecodrop",by.y="ID")
	plot(tf$jultmax, tf$betaw, 	xlab="July max temp change", ylab="2001-2013 Trend" , main=as.character(LDspec[j]))
	lines(lowess(tf$jultmax,tf$betaw), col="blue") # lowess line (x,y)
	abline(lm(tf$betaw~tf$jultmax), col="red") # regression line (y~x) 
}
dev.off()

pdf(file=paste("G:/Boreal/InterannualVariability/trends_junmint_2000_update.pdf",sep=""), height=7, width=10)
par(mfcol=c(3,3), oma=c(0,0,0,0))
for (j in 1:length(LDspec)) {
	spectrend <- trend[trend$SPECIES == LDspec[j],]
	tf <- merge(spectrend, clim, by.x="ecodrop",by.y="ID")
	plot(tf$juntmin, tf$betaw, 	xlab="June min temp change", ylab="2001-2013 Trend" , main=as.character(LDspec[j]))
	lines(lowess(tf$juntmin,tf$betaw), col="blue") # lowess line (x,y)
	abline(lm(tf$betaw~tf$juntmin), col="red") # regression line (y~x) 
}
dev.off()

pdf(file=paste("G:/Boreal/InterannualVariability/trends_junmintcombo_2000_update.pdf",sep=""), height=7, width=10)
	tf <- merge(trend, clim, by.x="ecodrop",by.y="ID")
	plot(tf$juntmin, tf$betaw, 	xlab="June min temp change", ylab="2001-2013 Trend")
	lines(lowess(tf$juntmin,tf$betaw), col="blue") # lowess line (x,y)
	abline(lm(tf$betaw~tf$juntmin), col="red") # regression line (y~x) 
dev.off()

pdf(file=paste("G:/Boreal/InterannualVariability/trends_julmaxtcombo_2000_update.pdf",sep=""), height=7, width=10)
	tf <- merge(trend, clim, by.x="ecodrop",by.y="ID")
	plot(tf$jultmax, tf$betaw, 	xlab="June min temp change", ylab="2001-2013 Trend")
	lines(lowess(tf$jultmax,tf$betaw), col="blue") # lowess line (x,y)
	abline(lm(tf$betaw~tf$jultmax), col="red") # regression line (y~x) 
dev.off()