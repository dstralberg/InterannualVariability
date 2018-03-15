library(data.table)
library(reshape)
library(stringr)
library(rasterVis)
require(gridExtra) # also loads grid
library(rgdal)
library(cluster)
library(gbm)
library(dismo)

clust <- raster("L:/Boreal/InterannualVariability/boreal_ecoregions_drop.asc")
d <- as.data.frame(rasterToPoints(clust))
names(d)[3] <-"eco"
centroid <- aggregate(d, by=list(d$eco),FUN="mean")
prov <- rgdal::readOGR("I:/GIS/basemaps/province_state.shp")
boreal <- rgdal::readOGR("L:/Boreal/InterannualVariability/boreal_ecoregions.shp")
wintid <- raster("L:/Boreal/InterannualVariability/ceclevel2nsa1.tif")

is_try_error <- function(x) inherits(x, "try-error")
intersect <- function(x, y) y[match(x, y, nomatch = 0)]
rainbow15 <- c("grey","black","brown","maroon","red","orange","yellow","lightgreen","green","darkgreen","turquoise","blue","purple","violet","pink")
indices <- read.csv("L:/Boreal/InterannualVariability/nao_pdo_soi_amo_annual.csv")
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
  zlim <- switch(which.min(abs(rng)), `1`=i:(1000+1), `2`=1:(i+1))
  p$legend[[1]]$args$key$at <- s[zlim]
  p$par.settings$regions$col <- ramp(1000)[zlim[-length(zlim)]]
  p
}

split.fun <- function(x, labs, digits, varlen, faclen)
{
    # replace commas with spaces (needed for strwrap)
    labs <- gsub(",", " ", labs)
    for(i in 1:length(labs)) {
        # split labs[i] into multiple lines
        labs[i] <- paste(strwrap(labs[i], width=25), collapse="\n")
    }
    labs
}

speclist <- read.csv("I:/BAM/BAMData/SpeciesClassesModv5.csv")
spec <- read.csv("I:/BAM/BAMData/species.csv")
speclist <- merge(speclist,spec[,c(1,4)], by.x="spp", by.y="SPECIES")
speclist$spp <- gsub("YWAR","YEWA",speclist$spp)
speclist <- speclist[speclist$spp != "PIWA",]
LDspec <- as.factor(as.character(speclist[speclist$DATABASE_MIG_TYPE=="LD",1]))

ecolu <- read.csv("L:/Boreal/InterannualVariability/ecoregion_lu.csv") 
ecolu$BorealLevel3 <- as.factor(as.character(ecolu$BorealLevel3))
trend <- read.csv("L:/Boreal/InterannualVariability/trends_weighted_2000.csv") 
rc <- ratify(clust)
ratc <- levels(rc)[[1]]

ts <- reshape(trend[,1:3], direction="wide", idvar="SPECIES", timevar="ecodrop", v.names="betaw")
row.names(ts) <- ts$SPECIES
s <- hclust(daisy(ts[,2:29]), method="ward.D")
pdf(file=paste("L:/Boreal/InterannualVariability/_speciesclusters.pdf",sep=""))
plot(s)
dev.off()


pdf(file=paste("L:/Boreal/InterannualVariability/_trendclusters.pdf",sep=""), height=3, width=4)
for (j in 1:length(LDspec)) {
	ts <- trend[trend$SPECIES == LDspec[j],]
	ts <- merge(ts,centroid,by.x="ecodrop",by.y="eco")
	c <- hclust(daisy(ts[,c(3,8,9)],metric="gower"))
	cc <- cutree(c, 4)
	tc <- as.data.frame((table(cc, ts$ecodrop)))
	tc$class <- as.numeric(tc$Freq) * as.numeric(tc$cc)
	tc <- tc[tc$class > 0,]
	tc <- tc[,c(2,4)]
	names(tc) <- c("eco","class")
	ratc <- merge(ratc,tc,by.x="ID",by.y="eco")
	levels(rc) <- ratc
	plot1 <- levelplot(rc, main = paste(LDspec[j], sep=" "), margin=FALSE, colorkey=NULL) + layer(sp.polygons(prov))
	grid.arrange(plot1,ncol=1,nrow=1)
	rc <- ratify(clust)
	ratc <- levels(rc)[[1]]
	}	
	dev.off()


pdf(file=paste("L:/Boreal/InterannualVariability/_abundclusters.pdf",sep=""), height=3, width=4)	
for (j in 1:length(LDspec)) {
	dat <- read.csv(paste("L:/Boreal/InterannualVariability/predclim19932013",LDspec[j],"2.csv",sep=""))
	dat <- merge(dat,ind,by.x="YEAR",by.y="year")
	dat$nalc <- as.factor(as.character(dat$nalc))
	dat$YEAR <- as.factor(as.character(dat$YEAR))
	dat$ecodrop <- as.factor(as.character(dat$ecodrop))	
	specmean <- aggregate(dat$pred,by=list(dat$ecodrop,dat$nalc),FUN=mean)
	specsd <- aggregate(dat$pred,by=list(dat$ecodrop,dat$nalc),FUN=sd)
	names(specmean) <- c("ecodrop","nalc","mean")
	names(specsd) <- c("ecodrop","nalc","sd")
	dat <- merge(dat,specmean,by=c("ecodrop","nalc"))
	dat <- merge(dat,specsd,by=c("ecodrop","nalc"))
	dat <- merge(dat,centroid,by.x="ecodrop",by.y="eco")
	c <- hclust(daisy(dat[,c(5,ncol(dat),ncol(dat)-1)],metric="gower"))
	cc <- cutree(c, 4)
	tc <- as.data.frame((table(cc, dat$ecodrop)))
	tc$Group <- ifelse(tc$Freq > 0, 1, 0)
	tc$class <- as.numeric(tc$Group) * as.numeric(tc$cc)
	tc <- tc[tc$class > 0,]
	tc <- tc[,c(2,5)]
	tc <- tc[!duplicated(tc$Var2),]
	names(tc) <- c("eco","class")
	ratc <- merge(ratc,tc,by.x="ID",by.y="eco")
	levels(rc) <- ratc
	plot1 <- levelplot(rc, main = paste(LDspec[j], sep=" "), margin=FALSE, colorkey=NULL) + layer(sp.polygons(prov))
	grid.arrange(plot1,ncol=1,nrow=1)
	rc <- ratify(clust)
	ratc <- levels(rc)[[1]]
	}
	dev.off()

rc <- ratify(clust)
ratc <- levels(rc)[[1]]	
pdf(file=paste("L:/Boreal/InterannualVariability/_abundyearclusters.pdf",sep=""))	
for (j in 1:length(LDspec)) {
	dat <- read.csv(paste("L:/Boreal/InterannualVariability/predclim19932013",LDspec[j],"2.csv",sep=""))
	dat <- merge(dat,ind,by.x="YEAR",by.y="year")
	dat$nalc <- as.factor(as.character(dat$nalc))
	dat$YEAR <- as.factor(as.character(dat$YEAR))
	dat$ecodrop <- as.factor(as.character(dat$ecodrop))	
	specmean <- aggregate(dat$pred,by=list(dat$ecodrop,dat$nalc),FUN=mean)
	specsd <- aggregate(dat$pred,by=list(dat$ecodrop,dat$nalc),FUN=sd)
	names(specmean) <- c("ecodrop","nalc","mean")
	names(specsd) <- c("ecodrop","nalc","sd")
	dat <- merge(dat,specmean,by=c("ecodrop","nalc"))
	dat <- merge(dat,specsd,by=c("ecodrop","nalc"))
	dat <- merge(dat,centroid,by.x="ecodrop",by.y="eco")
	dat$YEAR <- as.numeric(as.character(dat$YEAR))
	
	#dat1 <- reshape(dat[,c(1:5,ncol(dat)-1,ncol(dat))], direction="wide", idvar=c("ecodrop","SPECIES","nalc","x.y","y"), timevar="YEAR", v.names="pred")
	#c <- hclust(daisy(dat1[,5:ncol(dat1)],metric="gower"), method="ward.D")
	#cc <- cutree(c, 4)
	#tc <- as.data.frame((table(cc, dat1$ecodrop)))
	#tc$Group <- ifelse(tc$Freq > 0, 1, 0)
	#tc$region <- as.numeric(tc$Group) * as.numeric(tc$cc)
	#tc <- tc[tc$region > 0,]
	#tc <- tc[,c(2,5)]
	#tc <- tc[!duplicated(tc$Var2),]	
	#names(tc) <- c("eco","region")
	#write.csv(tc,file=paste("L:/Boreal/InterannualVariability/",LDspec[j],"abundyearclust.csv",sep=""), row.names=FALSE)
	#ratc <- merge(ratc,tc,by.x="ID",by.y="eco")
	#levels(rc) <- ratc
	#plot1 <- levelplot(rc, main = paste(LDspec[j], sep=" "), margin=FALSE) + layer(sp.polygons(prov))
	#rc1 <- deratify(rc,att="region")
	#grid.arrange(plot1,ncol=1,nrow=1)	
	
	c <- hclust(daisy(dat[,c(3,5,ncol(dat),ncol(dat)-1)],metric="gower"), method="ward.D")
	cc <- cutree(c, 4)
	tc <- as.data.frame((table(cc, dat$ecodrop)))
	tc$Group <- ifelse(tc$Freq > 0, 1, 0)
	tc$region <- as.numeric(tc$Group) * as.numeric(tc$cc)
	tc <- tc[tc$region > 0,]
	tc <- tc[,c(2,5)]
	tc <- tc[!duplicated(tc$Var2),]
	
	names(tc) <- c("eco","region")
	write.csv(tc,file=paste("L:/Boreal/InterannualVariability/",LDspec[j],"abundyearclust.csv",sep=""), row.names=FALSE)
	ratc <- merge(ratc,tc,by.x="ID",by.y="eco")
	levels(rc) <- ratc
	plot1 <- levelplot(rc, main = paste(LDspec[j], sep=" "), margin=FALSE) + layer(sp.polygons(prov))
	rc1 <- deratify(rc,att="region")
	grid.arrange(plot1,ncol=1,nrow=1)
	writeRaster(rc1, file=paste("L:/Boreal/InterannualVariability/",LDspec[j],"abundyearclust",sep=""), format="GTiff", options=c("COMPRESS=NONE", "TFW=YES"),overwrite=TRUE)
	rc <- ratify(clust)
	ratc <- levels(rc)[[1]]
	}
	dev.off()
		

pdf(file=paste("L:/Boreal/InterannualVariability/_lmertrees_pred_anom_cluster.pdf",sep=""))			
for (j in 1:length(LDspec)) {
	dat <- read.csv(paste("L:/Boreal/InterannualVariability/predclim19932013",LDspec[j],"2.csv",sep=""))
	names(dat)[2] <- "eco"
	dat <- merge(dat,ind,by.x="YEAR",by.y="year")
	dat$nalc <- as.factor(as.character(dat$nalc))
	dat$YEAR <- as.factor(as.character(dat$YEAR))
	dat$eco <- as.factor(as.character(dat$eco))	
	specmean <- aggregate(dat$pred,by=list(dat$eco,dat$nalc),FUN=mean)
	specsd <- aggregate(dat$pred,by=list(dat$eco,dat$nalc),FUN=sd)
	names(specmean) <- c("eco","nalc","mean")
	names(specsd) <- c("eco","nalc","sd")
	dat <- merge(dat,specmean,by=c("eco","nalc"))
	dat <- merge(dat,specsd,by=c("eco","nalc"))
	dat <- merge(dat,centroid,by="eco")
	dat$YEAR <- as.numeric(as.character(dat$YEAR))
	dat$anom <- (dat$pred - dat$mean)/(dat$sd)
	dat <- dat[dat$mean > 0.001,]
	tc <- read.csv(paste("L:/Boreal/InterannualVariability/",LDspec[j],"abundyearclust.csv",sep=""))	
	dat <- merge(dat,tc, by="eco")
	dat$region <- as.factor(as.character(dat$region))
	indep <- names(dat)[7]
	for (i in 8:(ncol(dat)-7)) { 
		indep <- paste(indep, names(dat)[i], sep=" + ")
		}
	indep <- paste(indep, "region", sep=" + ")	
	form1 <- as.formula(paste("pred ~ nalc | eco | ", indep,sep=""))	
	x1 <- try(lt1 <- lmertree(form1, data = dat, joint=FALSE))
	if (class(x1) != "try-error") {
		plot(lt1, main=paste(as.character(LDspec[j]), "pred", sep=" "), which="tree")}
	#form2 <- as.formula(paste("anom ~ nalc | eco | ", indep,sep=""))	
	#x2 <- try(lt2 <- lmertree(form2, data = dat, joint=FALSE))
	#if (class(x2) != "try-error") {
	#plot(lt2, main=paste(as.character(LDspec[j]), "anom", sep=" "), which="tree")}
	}	
dev.off()

# annual abundance predictions combined
for (j in 1:length(LDspec)) {
	dat <- read.csv(paste("L:/Boreal/InterannualVariability/predclim19932013",LDspec[j],"2.csv",sep=""))
	names(dat)[2] <- "eco"
	dat <- merge(dat,ind,by.x="YEAR",by.y="year")
	dat$nalc <- as.factor(as.character(dat$nalc))
	dat$YEAR <- as.factor(as.character(dat$YEAR))
	dat$eco <- as.factor(as.character(dat$eco))	
	specmean <- aggregate(dat$pred,by=list(dat$eco,dat$nalc),FUN=mean)
	specsd <- aggregate(dat$pred,by=list(dat$eco,dat$nalc),FUN=sd)
	names(specmean) <- c("eco","nalc","mean")
	names(specsd) <- c("eco","nalc","sd")
	dat <- merge(dat,specmean,by=c("eco","nalc"))
	dat <- merge(dat,specsd,by=c("eco","nalc"))
	dat <- merge(dat,centroid,by="eco")
	dat$YEAR <- as.numeric(as.character(dat$YEAR))
	tc <- read.csv(paste("L:/Boreal/InterannualVariability/",LDspec[j],"abundyearclust.csv",sep=""))	
	dat$anom <- (dat$pred - dat$mean)/(dat$sd)
	dat <- dat[dat$mean > 0.001,]
	dat <- merge(dat,tc, by="eco")
	names(dat)[ncol(dat)] <- "region"
	dat$region <- as.factor(as.character(dat$region))
	dat1 <- cbind("pred"=dat[,5],dat[,c(2,7:(ncol(dat)-7))],"region"=dat[,ncol(dat)])
	brt1 <- gbm.step(dat1, gbm.y = 1, gbm.x = c(3:ncol(dat1)), family = "gaussian", tree.complexity = 3, learning.rate = 0.001, bag.fraction = 0.5)
	save(brt1,file=paste("L:/Boreal/InterannualVariability/",LDspec[j],"brt_2001-2013_pred_cluster.RData",sep=""))
	varimp <- as.data.frame(brt1$contributions[1:100,])
	write.csv(varimp,file=paste("L:/Boreal/InterannualVariability/",LDspec[j],"brt_2001-2013varimp_pred_cluster.csv",sep=""))
	cvstats <- t(as.data.frame(brt1$cv.statistics))
	write.csv(cvstats,file=paste("L:/Boreal/InterannualVariability/",LDspec[j],"brt_2001-2013cvstats_pred_cluster.csv",sep=""))
	}	

# annual abundance predictions by region
for (j in 1:length(LDspec)) {
	dat <- read.csv(paste("L:/Boreal/InterannualVariability/predclim19932013",LDspec[j],"2.csv",sep=""))
	names(dat)[2] <- "eco"
	dat <- merge(dat,ind,by.x="YEAR",by.y="year")
	dat$nalc <- as.factor(as.character(dat$nalc))
	dat$YEAR <- as.factor(as.character(dat$YEAR))
	dat$eco <- as.factor(as.character(dat$eco))	
	specmean <- aggregate(dat$pred,by=list(dat$eco,dat$nalc),FUN=mean)
	specsd <- aggregate(dat$pred,by=list(dat$eco,dat$nalc),FUN=sd)
	names(specmean) <- c("eco","nalc","mean")
	names(specsd) <- c("eco","nalc","sd")
	dat <- merge(dat,specmean,by=c("eco","nalc"))
	dat <- merge(dat,specsd,by=c("eco","nalc"))
	dat <- merge(dat,centroid,by="eco")
	dat$YEAR <- as.numeric(as.character(dat$YEAR))
	datt <- dat
	datt$YEAR <- dat$YEAR -1
	x1 <- try(trend.lm <- lm(pred ~ eco:YEAR + nalc, data=dat, weights=x.x))
	dat$pt <- predict(trend.lm, newdata=dat, type="response")
	dat$pt1 <- predict(trend.lm, newdata=datt, type="response")
	dat$chg <- log(dat$pred - dat$pt1 + 1)
	dat$YEAR <- as.factor(as.character(dat$YEAR))
	dat$tanom <- log(dat$pred - dat$pt + 1)
	dat$YEAR <- as.factor(as.character(dat$YEAR))	
	dat$anom <- (dat$pred - dat$mean)/(dat$sd)
	dat <- dat[dat$mean > 0.001,]
	tc <- read.csv(paste("L:/Boreal/InterannualVariability/",LDspec[j],"abundyearclust.csv",sep=""))
	dat <- merge(dat,tc, by="eco")
	names(dat)[ncol(dat)] <- "region"
	dat$region <- as.factor(as.character(dat$region))
	tc$region <- as.factor(tc$region)
	for (i in levels(tc$region)) {
		dat1 <- cbind("pred"=dat[,5],dat[,c(2,7:(ncol(dat)-11))],"region"=dat[,ncol(dat)])
		dat1 <- dat1[dat1$region == i,]
		x1 <- try(brt1 <- gbm.step(dat1, gbm.y = 1, gbm.x = c(2:ncol(dat1)), family = "gaussian", tree.complexity = 3, learning.rate = 0.001, bag.fraction = 0.5))
		if (class(x1) != "try-error") {
			save(brt1,file=paste("L:/Boreal/InterannualVariability/",LDspec[j],i,"brt_2001-2013_pred_cluster.RData",sep=""))
			varimp <- as.data.frame(brt1$contributions[1:100,])
			write.csv(varimp,file=paste("L:/Boreal/InterannualVariability/",LDspec[j],i,"brt_2001-2013varimp_pred_cluster.csv",sep=""))
			cvstats <- t(as.data.frame(brt1$cv.statistics))
			write.csv(cvstats,file=paste("L:/Boreal/InterannualVariability/",LDspec[j],i,"brt_2001-2013cvstats_pred_cluster.csv",sep=""))
			}
		}
	}	

#mean anomalies by region
for (j in 1:length(LDspec)) {
	dat <- read.csv(paste("L:/Boreal/InterannualVariability/predclim19932013",LDspec[j],"2.csv",sep=""))
	names(dat)[2] <- "eco"
	dat <- merge(dat,ind,by.x="YEAR",by.y="year")
	dat$nalc <- as.factor(as.character(dat$nalc))
	dat$YEAR <- as.factor(as.character(dat$YEAR))
	dat$eco <- as.factor(as.character(dat$eco))	
	specmean <- aggregate(dat$pred,by=list(dat$eco,dat$nalc),FUN=mean)
	specsd <- aggregate(dat$pred,by=list(dat$eco,dat$nalc),FUN=sd)
	names(specmean) <- c("eco","nalc","mean")
	names(specsd) <- c("eco","nalc","sd")
	dat <- merge(dat,specmean,by=c("eco","nalc"))
	dat <- merge(dat,specsd,by=c("eco","nalc"))
	dat <- merge(dat,centroid,by="eco")
	dat$YEAR <- as.numeric(as.character(dat$YEAR))
	datt <- dat
	datt$YEAR <- dat$YEAR -1
	x1 <- try(trend.lm <- lm(pred ~ eco:YEAR + nalc, data=dat, weights=x.x))
	dat$pt <- predict(trend.lm, newdata=dat, type="response")
	dat$pt1 <- predict(trend.lm, newdata=datt, type="response")
	dat$chg <- log(dat$pred - dat$pt1 + 1)
	dat$YEAR <- as.factor(as.character(dat$YEAR))
	dat$tanom <- log(dat$pred - dat$pt + 1)
	dat$YEAR <- as.factor(as.character(dat$YEAR))	
	dat$anom <- (dat$pred - dat$mean)/(dat$sd)
	dat <- dat[dat$mean > 0.001,]
	tc <- read.csv(paste("L:/Boreal/InterannualVariability/",LDspec[j],"abundyearclust.csv",sep=""))
	dat <- merge(dat,tc, by="eco")
	names(dat)[ncol(dat)] <- "region"
	dat$region <- as.factor(as.character(dat$region))
	tc$region <- as.factor(tc$region)
	for (i in levels(tc$region)) {
		dat1 <- cbind("anom"=dat[,ncol(dat)-1],dat[,c(2,7:(ncol(dat)-11))],"region"=dat[,ncol(dat)])
		dat1 <- dat1[dat1$region == i,]
		x1 <- try(brt1 <- gbm.step(dat1, gbm.y = 1, gbm.x = c(3:ncol(dat1)), family = "gaussian", tree.complexity = 3, learning.rate = 0.001, bag.fraction = 0.5))
		if (class(x1) != "try-error") {
			save(brt1,file=paste("L:/Boreal/InterannualVariability/",LDspec[j],i,"brt_2001-2013_anom_cluster.RData",sep=""))
			varimp <- as.data.frame(brt1$contributions[1:100,])
			write.csv(varimp,file=paste("L:/Boreal/InterannualVariability/",LDspec[j],i,"brt_2001-2013varimp_anom_cluster.csv",sep=""))
			cvstats <- t(as.data.frame(brt1$cv.statistics))
			write.csv(cvstats,file=paste("L:/Boreal/InterannualVariability/",LDspec[j],i,"brt_2001-2013cvstats_anom_cluster.csv",sep=""))
			}
		}
	}	


#annual change by region
for (j in 1:length(LDspec)) {
	dat <- read.csv(paste("L:/Boreal/InterannualVariability/predclim19932013",LDspec[j],"2.csv",sep=""))
	names(dat)[2] <- "eco"
	dat <- merge(dat,ind,by.x="YEAR",by.y="year")
	dat$nalc <- as.factor(as.character(dat$nalc))
	dat$YEAR <- as.factor(as.character(dat$YEAR))
	dat$eco <- as.factor(as.character(dat$eco))	
	specmean <- aggregate(dat$pred,by=list(dat$eco,dat$nalc),FUN=mean)
	specsd <- aggregate(dat$pred,by=list(dat$eco,dat$nalc),FUN=sd)
	names(specmean) <- c("eco","nalc","mean")
	names(specsd) <- c("eco","nalc","sd")
	dat <- merge(dat,specmean,by=c("eco","nalc"))
	dat <- merge(dat,specsd,by=c("eco","nalc"))
	dat <- merge(dat,centroid,by="eco")
	dat$YEAR <- as.numeric(as.character(dat$YEAR))
	datt <- dat
	datt$YEAR <- dat$YEAR -1
	x1 <- try(trend.lm <- lm(pred ~ eco:YEAR + nalc, data=dat, weights=x.x))
	dat$pt <- predict(trend.lm, newdata=dat, type="response")
	dat$pt1 <- predict(trend.lm, newdata=datt, type="response")
	dat$chg <- log(dat$pred - dat$pt1 + 1)
	dat$YEAR <- as.factor(as.character(dat$YEAR))
	dat$tanom <- log(dat$pred - dat$pt + 1)
	dat$YEAR <- as.factor(as.character(dat$YEAR))	
	dat$anom <- (dat$pred - dat$mean)/(dat$sd)
	dat <- dat[dat$mean > 0.001,]
	tc <- read.csv(paste("L:/Boreal/InterannualVariability/",LDspec[j],"abundyearclust.csv",sep=""))
	dat <- merge(dat,tc, by="eco")
	names(dat)[ncol(dat)] <- "region"
	dat$region <- as.factor(as.character(dat$region))
	tc$region <- as.factor(tc$region)
	for (i in levels(tc$region)) {
		dat1 <- cbind("chg"=dat[,ncol(dat)-3],dat[,c(2,7:(ncol(dat)-11))],"region"=dat[,ncol(dat)])
		dat1 <- dat1[dat1$region == i,]
		x1 <- try(brt1 <- gbm.step(dat1, gbm.y = 1, gbm.x = c(3:ncol(dat1)), family = "gaussian", tree.complexity = 3, learning.rate = 0.001, bag.fraction = 0.5))
		if (class(x1) != "try-error") {
			save(brt1,file=paste("L:/Boreal/InterannualVariability/",LDspec[j],i,"brt_2001-2013_chg_cluster.RData",sep=""))
			varimp <- as.data.frame(brt1$contributions[1:100,])
			write.csv(varimp,file=paste("L:/Boreal/InterannualVariability/",LDspec[j],i,"brt_2001-2013varimp_chg_cluster.csv",sep=""))
			cvstats <- t(as.data.frame(brt1$cv.statistics))
			write.csv(cvstats,file=paste("L:/Boreal/InterannualVariability/",LDspec[j],i,"brt_2001-2013cvstats_chg_cluster.csv",sep=""))
			}
		}
	}			

#trend anomalies by region
for (j in 1:length(LDspec)) {
	dat <- read.csv(paste("L:/Boreal/InterannualVariability/predclim19932013",LDspec[j],"2.csv",sep=""))
	names(dat)[2] <- "eco"
	dat <- merge(dat,ind,by.x="YEAR",by.y="year")
	dat$nalc <- as.factor(as.character(dat$nalc))
	dat$YEAR <- as.factor(as.character(dat$YEAR))
	dat$eco <- as.factor(as.character(dat$eco))	
	specmean <- aggregate(dat$pred,by=list(dat$eco,dat$nalc),FUN=mean)
	specsd <- aggregate(dat$pred,by=list(dat$eco,dat$nalc),FUN=sd)
	names(specmean) <- c("eco","nalc","mean")
	names(specsd) <- c("eco","nalc","sd")
	dat <- merge(dat,specmean,by=c("eco","nalc"))
	dat <- merge(dat,specsd,by=c("eco","nalc"))
	dat <- merge(dat,centroid,by="eco")
	dat$YEAR <- as.numeric(as.character(dat$YEAR))
	datt <- dat
	datt$YEAR <- dat$YEAR -1
	x1 <- try(trend.lm <- lm(pred ~ eco:YEAR + nalc, data=dat, weights=x.x))
	dat$pt <- predict(trend.lm, newdata=dat, type="response")
	dat$pt1 <- predict(trend.lm, newdata=datt, type="response")
	dat$chg <- log(dat$pred - dat$pt1 + 1)
	dat$YEAR <- as.factor(as.character(dat$YEAR))
	dat$tanom <- log(dat$pred - dat$pt + 1)
	dat$YEAR <- as.factor(as.character(dat$YEAR))	
	dat$anom <- (dat$pred - dat$mean)/(dat$sd)
	dat <- dat[dat$mean > 0.001,]
	tc <- read.csv(paste("L:/Boreal/InterannualVariability/",LDspec[j],"abundyearclust.csv",sep=""))
	dat <- merge(dat,tc, by="eco")
	names(dat)[ncol(dat)] <- "region"
	dat$region <- as.factor(as.character(dat$region))
	tc$region <- as.factor(tc$region)
	for (i in levels(tc$region)) {
		dat1 <- cbind("tanom"=dat[,ncol(dat)-2],dat[,c(2,7:(ncol(dat)-11))],"region"=dat[,ncol(dat)])
		dat1 <- dat1[dat1$region == i,]
		x1 <- try(brt1 <- gbm.step(dat1, gbm.y = 1, gbm.x = c(3:ncol(dat1)), family = "gaussian", tree.complexity = 3, learning.rate = 0.001, bag.fraction = 0.5))
		if (class(x1) != "try-error") {
			save(brt1,file=paste("L:/Boreal/InterannualVariability/",LDspec[j],i,"brt_2001-2013_tanom_cluster.RData",sep=""))
			varimp <- as.data.frame(brt1$contributions[1:100,])
			write.csv(varimp,file=paste("L:/Boreal/InterannualVariability/",LDspec[j],i,"brt_2001-2013varimp_tanom_cluster.csv",sep=""))
			cvstats <- t(as.data.frame(brt1$cv.statistics))
			write.csv(cvstats,file=paste("L:/Boreal/InterannualVariability/",LDspec[j],i,"brt_2001-2013cvstats_tanom_cluster.csv",sep=""))
			}
		}
	}			
	

#summarize explanatory power and top variables
cl <- list.files("L:/Boreal/InterannualVariability/", pattern="cluster.csv")
setwd("L:/Boreal/InterannualVariability/")
cv <- grep(cl, pattern="cvstats", value=TRUE)
vi <- grep(cl, pattern="varimp", value=TRUE)
cv <- grep(cv, pattern="chg", value=TRUE) #92 region / species
vi <- grep(vi, pattern="chg", value=TRUE)
combo <- data.frame("ID" =1:length(cv), "species" = 0, "region"=0, "metric"= 0, "R2" = 0, "vars" = "none")
combo$vars <- as.character(combo$vars)
for (i in 1:length(cv)) {
	combo$species[i] <- substr(cv[i],1,4)
	combo$region[i] <- substr(cv[i],5,5)
	combo$metric[i] <- substr(cv[i],27,30)
	x1 <- try(cvstats <- read.csv(cv[i]))
	if (class(x1) != "try-error") {
		combo$R2[i] <- cvstats[3,2]
		varimp <- read.csv(vi[i])
		vars <- as.character(varimp[1,1])
		for (k in 2:20) {
			var <- as.character(varimp[k,1])
			vars <- paste(vars,var,sep=", ")
			}
		combo$vars[i] <- vars
		}
	}
write.csv(combo, file="L:/Boreal/InterannualVariability/_BRTResultsByRegion.csv")


#variable importance (change)
cv <- grep(cv, pattern="chg", value=TRUE) #92 region / species
vi <- grep(vi, pattern="chg", value=TRUE)
cvstats <- read.csv(cv[1])
names(cvstats) <- c("var",substr(cv[1],1,5))
varimp <- read.csv(vi[1])
varimp$spec <- substr(vi[1],1,5)
varimp$R2 <- cvstats[3,2]
for (j in 2:length(cv)) { 
	t1 <- try(varimp1 <- read.csv(vi[j]), silent=TRUE)
	t <- try(cvstats1 <- read.csv(cv[j]), silent=TRUE)
	if(class(t1) == "try-error") {} else {
		varimp1$spec <- substr(vi[j],1,5)
		varimp1$R2 <- cvstats1[3,2]
		varimp <- rbind(varimp,varimp1)}
	if(class(t) == "try-error") {} else {
		names(cvstats1) <- c("var",substr(cv[j],1,5))
		cvstats <- merge(cvstats,cvstats1, by="var")
		}
	}	
write.csv(cvstats, file="L:/Boreal/InterannualVariability/_cvstats_chg_byregion.csv", row.names=FALSE)

varimplookup <- read.csv("L:/Boreal/InterannualVariability/_varimp_lookup.csv")
varimp <- merge(varimp[,2:5], varimplookup, by.x = "var", by.y ="Group.1")
varimp$abs.inf <- (varimp$rel.inf * varimp$R2)
varimp$region <- substr(varimp$spec,5,5)
varimp$species <- substr(varimp$spec,1,4)
write.csv(varimp, file="L:/Boreal/InterannualVariability/_varimp_chg_byregion.csv", row.names=FALSE)

topvars <- aggregate(varimp[,c(2,9)], by=list("var"=varimp$var), FUN="sum")
write.csv(topvars, file="L:/Boreal/InterannualVariability/_topvars_chg_byregion.csv", row.names=FALSE)

R2max <- aggregate(varimp$R2,by=list("type"=varimp$type, "species"=varimp$species), FUN="max")
names(R2max)[3] <- "R2"
varimpmax <- merge(varimp, R2max, by=c("R2","species","type"),all.y=TRUE)

s <- aggregate(varimp$R2, by=list(varimp$species),FUN="max") #41 species

varimpsum <- aggregate(varimp[,c(2,9)],by=list("type"=varimp$type), FUN="sum")	
write.csv(varimpsum, file="L:/Boreal/InterannualVariability/_varimpsum_chg_byregion.csv", row.names=FALSE)

varimpsum2 <- aggregate(varimp[,c(2,9)],by=list("type2"=varimp$level3), FUN="sum")	
write.csv(varimpsum2, file="L:/Boreal/InterannualVariability/_varimpsum_chg_byregion2.csv", row.names=FALSE)

varimpsummax <- aggregate(varimpmax[,c(5,10)],by=list("type"=varimpmax$type), FUN="sum")	
write.csv(varimpsummax2, file="L:/Boreal/InterannualVariability/_varimpsum_chg_bymaxregion.csv", row.names=FALSE)

varimpsummax2 <- aggregate(varimpmax[,c(5,10)],by=list("type2"=varimpmax$level3), FUN="sum")	
write.csv(varimpsummax, file="L:/Boreal/InterannualVariability/_varimpsum_chg_bymaxregion2.csv", row.names=FALSE)

varimpmean <- aggregate(varimp[,c(2,9)],by=list(varimp$type), FUN="mean")	
write.csv(varimpmean, file="L:/Boreal/InterannualVariability/_varimpmean_chg_byregion.csv", row.names=FALSE)

varimpmean2 <- aggregate(varimp[,c(2,9)],by=list(varimp$level3), FUN="mean")	
write.csv(varimpmean2, file="L:/Boreal/InterannualVariability/_varimpmean_chg_byregion2.csv", row.names=FALSE)