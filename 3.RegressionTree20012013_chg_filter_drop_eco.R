library(data.table)
library(reshape)
library(stringr)
library(rasterVis)
require(gridExtra) # also loads grid
library(rgdal)
library(cluster)
library(gbm)
library(dismo)

clust <- raster("G:/Boreal/InterannualVariability/boreal_ecoregions_drop.asc")
d <- as.data.frame(rasterToPoints(clust))
names(d)[3] <-"eco"
centroid <- aggregate(d, by=list(d$eco),FUN="mean")
prov <- rgdal::readOGR("E:/GIS/basemaps/province_state.shp")
boreal <- rgdal::readOGR("G:/Boreal/InterannualVariability/boreal_ecoregions.shp")
wintid <- raster("G:/Boreal/InterannualVariability/ceclevel2nsa1.tif")

is_try_error <- function(x) inherits(x, "try-error")
intersect <- function(x, y) y[match(x, y, nomatch = 0)]
rainbow15 <- c("grey","black","brown","maroon","red","orange","yellow","lightgreen","green","darkgreen","turquoise","blue","purple","violet","pink")
indices <- read.csv("G:/Boreal/InterannualVariability/nao_pdo_soi_amo_annual.csv")
ind <- indices[,c(1:25,32:35,44:47)]

geomean <- function(x) {
  exp(mean(log(x)))
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

speclist <- read.csv("F:/BAM/BAMData/SpeciesClassesModv5.csv")
spec <- read.csv("F:/BAM/BAMData/species.csv")
speclist <- merge(speclist,spec[,c(1,4)], by.x="spp", by.y="SPECIES")
speclist$spp <- gsub("YWAR","YEWA",speclist$spp)
speclist <- speclist[speclist$spp != "PIWA",]
speclist <- speclist[speclist$spp != "YTVI",]
speclist <- speclist[speclist$spp != "BAOR",]
LDspec <- as.factor(as.character(speclist[speclist$DATABASE_MIG_TYPE=="LD",1]))

ecolu <- read.csv("G:/Boreal/InterannualVariability/ecoregion_lu.csv") 
ecolu$BorealLevel3 <- as.factor(as.character(ecolu$BorealLevel3))

trend <- read.csv("G:/Boreal/InterannualVariability/_trend20002013_update.csv") 
rc <- ratify(clust)
ratc <- levels(rc)[[1]]

#annual change combined with eco
for (j in 1:length(LDspec)) {
	dat <- read.csv(paste("G:/Boreal/InterannualVariability/predclim20002013",LDspec[j],"_update.csv",sep=""))
	names(dat)[2] <- "eco"
	dat <- merge(dat,ind,by.x="YEAR",by.y="year")
	dat$nalc <- as.factor(as.character(dat$nalc))
	dat$eco <- as.factor(as.character(dat$eco))	
	specmean <- aggregate(dat$pred,by=list(dat$eco,dat$nalc),FUN=mean)
	specsd <- aggregate(dat$pred,by=list(dat$eco,dat$nalc),FUN=sd)
	names(specmean) <- c("eco","nalc","mean")
	names(specsd) <- c("eco","nalc","sd")
	dat <- merge(dat,specmean,by=c("eco","nalc"))
	dat <- merge(dat,specsd,by=c("eco","nalc"))
	dat <- dat[dat$mean > 0.001,]
	dat1 <- dat[,c(9,1:2,11:(ncol(dat)-2))]
	dat1 <- na.omit(dat1)
	try(brt1 <- gbm.step(dat1, gbm.y = 1, gbm.x = c(2:ncol(dat1)), family = "gaussian", tree.complexity = 3, learning.rate = 0.001, bag.fraction = 0.5))
	if(is.null(brt1)) {try(brt.chg <- gbm.step(dat1, gbm.y = 1, gbm.x = c(2:(ncol(dat1))), family = "gaussian", tree.complexity = 3, learning.rate = 0.0001, bag.fraction = 0.5))}
	try(save(brt1,file=paste("G:/Boreal/InterannualVariability/",LDspec[j],"brt_2001-2013_chg_eco_update.RData",sep="")))	
	varimp <- as.data.frame(brt1$contributions)
	write.csv(varimp,file=paste("G:/Boreal/InterannualVariability/",LDspec[j],"brt_2001-2013varimp_chg_eco_update.csv",sep=""))
	cvstats <- as.data.frame(brt1$cv.statistics[c(1,3)])
	names(cvstats) <- c("deviance.cv","correlation.cv")
	cvstats$deviance.null <- brt1$self.statistics$mean.null
	cvstats$pseudo.R2 <- (brt1$self.statistics$mean.null-brt1$self.statistics$mean.resid)/brt1$self.statistics$mean.null
	cvstats$correlation <- brt1$self.statistics$correlation
	cvstats$pseudo.R2.cv <- (cvstats$deviance.null-cvstats$deviance.cv)/cvstats$deviance.null
	write.csv(cvstats,file=paste("G:/Boreal/InterannualVariability/",LDspec[j],"brt_2001-2013cvstats_chg_eco_update.csv",sep=""))
}	

# annual pred combined with eco
for (j in 1:length(LDspec)) {
  dat <- read.csv(paste("G:/Boreal/InterannualVariability/predclim20002013",LDspec[j],"_update.csv",sep=""))
  names(dat)[2] <- "eco"
  dat <- merge(dat,ind,by.x="YEAR",by.y="year")
  dat$nalc <- as.factor(as.character(dat$nalc))
  dat$eco <- as.factor(as.character(dat$eco))	
  specmean <- aggregate(dat$pred,by=list(dat$eco,dat$nalc),FUN=mean)
  specsd <- aggregate(dat$pred,by=list(dat$eco,dat$nalc),FUN=sd)
  names(specmean) <- c("eco","nalc","mean")
  names(specsd) <- c("eco","nalc","sd")
  dat <- merge(dat,specmean,by=c("eco","nalc"))
  dat <- merge(dat,specsd,by=c("eco","nalc"))
  dat <- dat[dat$mean > 0.001,]
  dat1 <- dat[,c(5,1:2,11:(ncol(dat)-2))]
  dat1 <- na.omit(dat1)
  try(brt1 <- gbm.step(dat1, gbm.y = 1, gbm.x = c(2:ncol(dat1)), family = "gaussian", tree.complexity = 3, learning.rate = 0.001, bag.fraction = 0.5))
  if(is.null(brt1)) {try(brt.chg <- gbm.step(dat1, gbm.y = ncol(dat1), gbm.x = c(1:(ncol(dat1)-1)), family = "gaussian", tree.complexity = 3, learning.rate = 0.0001, bag.fraction = 0.5))}
  try(save(brt1,file=paste("G:/Boreal/InterannualVariability/",LDspec[j],"brt_2001-2013_pred_eco_update.RData",sep="")))	
  varimp <- as.data.frame(brt1$contributions)
  write.csv(varimp,file=paste("G:/Boreal/InterannualVariability/",LDspec[j],"brt_2001-2013varimp_pred_eco_update.csv",sep=""))
  cvstats <- as.data.frame(brt1$cv.statistics[c(1,3)])
  names(cvstats) <- c("deviance.cv","correlation.cv")
  cvstats$deviance.null <- brt1$self.statistics$mean.null
  cvstats$pseudo.R2 <- (brt1$self.statistics$mean.null-brt1$self.statistics$mean.resid)/brt1$self.statistics$mean.null
  cvstats$correlation <- brt1$self.statistics$correlation
  cvstats$pseudo.R2.cv <- (cvstats$deviance.null-cvstats$deviance.cv)/cvstats$deviance.null
  write.csv(cvstats,file=paste("G:/Boreal/InterannualVariability/",LDspec[j],"brt_2001-2013cvstats_pred_eco_update.csv",sep=""))
}	

# trend anomalies combined with eco
for (j in 1:length(LDspec)) {
  dat <- read.csv(paste("G:/Boreal/InterannualVariability/predclim20002013",LDspec[j],"_update.csv",sep=""))
  names(dat)[2] <- "eco"
  dat <- merge(dat,ind,by.x="YEAR",by.y="year")
  dat$nalc <- as.factor(as.character(dat$nalc))
  dat$eco <- as.factor(as.character(dat$eco))	
  specmean <- aggregate(dat$pred,by=list(dat$eco,dat$nalc),FUN=mean)
  specsd <- aggregate(dat$pred,by=list(dat$eco,dat$nalc),FUN=sd)
  names(specmean) <- c("eco","nalc","mean")
  names(specsd) <- c("eco","nalc","sd")
  dat <- merge(dat,specmean,by=c("eco","nalc"))
  dat <- merge(dat,specsd,by=c("eco","nalc"))
  dat <- dat[dat$mean > 0.001,]
  dat1 <- dat[,c(5,1:2,11:(ncol(dat)-2))]
  dat1 <- na.omit(dat1)
  try(brt1 <- gbm.step(dat1, gbm.y = 1, gbm.x = c(2:ncol(dat1)), family = "gaussian", tree.complexity = 3, learning.rate = 0.001, bag.fraction = 0.5))
  if(is.null(brt1)) {try(brt.chg <- gbm.step(dat1, gbm.y = ncol(dat1), gbm.x = c(1:(ncol(dat1)-1)), family = "gaussian", tree.complexity = 3, learning.rate = 0.0001, bag.fraction = 0.5))}
  try(save(brt1,file=paste("G:/Boreal/InterannualVariability/",LDspec[j],"brt_2001-2013_tanom_eco_update.RData",sep="")))	
  varimp <- as.data.frame(brt1$contributions)
  write.csv(varimp,file=paste("G:/Boreal/InterannualVariability/",LDspec[j],"brt_2001-2013varimp_tanom_eco_update.csv",sep=""))
  cvstats <- as.data.frame(brt1$cv.statistics[c(1,3)])
  names(cvstats) <- c("deviance.cv","correlation.cv")
  cvstats$deviance.null <- brt1$self.statistics$mean.null
  cvstats$pseudo.R2 <- (brt1$self.statistics$mean.null-brt1$self.statistics$mean.resid)/brt1$self.statistics$mean.null
  cvstats$correlation <- brt1$self.statistics$correlation
  cvstats$pseudo.R2.cv <- (cvstats$deviance.null-cvstats$deviance.cv)/cvstats$deviance.null
  write.csv(cvstats,file=paste("G:/Boreal/InterannualVariability/",LDspec[j],"brt_2001-2013cvstats_tanom_eco_update.csv",sep=""))
}	

varimplookup <- read.csv("G:/Boreal/InterannualVariability/_varimp_lookup.csv")


#summarize explanatory power and top variables
cl <- list.files("G:/Boreal/InterannualVariability/", pattern="eco_update.csv")
setwd("G:/Boreal/InterannualVariability/")
cv <- grep(cl, pattern="cvstats", value=TRUE)
vi <- grep(cl, pattern="varimp", value=TRUE)
cv <- grep(cv, pattern="chg", value=TRUE) 
#cv <- cv[2:46]
vi <- grep(vi, pattern="chg", value=TRUE)
#vi <- vi[4:49]
combo <- data.frame("ID" =1:length(cv), "species" = 0, "R2" = 0, "vars" = "none")
combo$vars <- as.character(combo$vars)
for (i in 1:length(cv)) {
	combo$species[i] <- substr(cv[i],1,4)
	x1 <- try(cvstats <- read.csv(cv[i]))
	if (class(x1) != "try-error") {
		combo$R2[i] <- cvstats[1,5]
		varimp <- read.csv(vi[i])
		vars <- as.character(varimp[1,1])
		for (k in 2:20) {
			var <- as.character(varimp[k,1])
			vars <- paste(vars,var,sep=", ")
			}
		combo$vars[i] <- vars
		}
	}
write.csv(combo, file="G:/Boreal/InterannualVariability/_BRTResults_chg_eco_update.csv")


#summarize explanatory power and top variables
cvstats <- read.csv(cv[1])
cvstats$spec <- substr(cv[1],1,4)
varimp <- read.csv(vi[1])
varimp$spec <- substr(vi[1],1,4)
varimp$R2 <- cvstats[1,5]
for (j in 2:length(cv)) { 
	t1 <- try(varimp1 <- read.csv(vi[j]), silent=TRUE)
	t <- try(cvstats1 <- read.csv(cv[j]), silent=TRUE)
	if(class(t1) == "try-error") {} else {
		varimp1$spec <- substr(vi[j],1,4)
		varimp1$R2 <- cvstats1[1,5]
		varimp <- rbind(varimp,varimp1)}
	if(class(t) == "try-error") {} else {
		cvstats$spec <- substr(cv[j],1,4)
		cvstats <- rbind(cvstats,cvstats1)
		}
	}	
write.csv(cvstats, file="G:/Boreal/InterannualVariability/_cvstats_chg_eco_update.csv", row.names=FALSE)

varimplookup <- read.csv("G:/Boreal/InterannualVariability/_varimp_lookup_update.csv")
varimp <- merge(varimp[,2:5], varimplookup, by.x = "var", by.y ="Group.1")
varimp$abs.inf <- (varimp$rel.inf * varimp$R2)
varimp$region <- substr(varimp$spec,5,5)
varimp$species <- substr(varimp$spec,1,4)
write.csv(varimp, file="G:/Boreal/InterannualVariability/_varimp_chg_eco_update.csv", row.names=FALSE)

topvars <- aggregate(varimp[,c(2,9)], by=list("var"=varimp$var), FUN="sum")
write.csv(topvars, file="G:/Boreal/InterannualVariability/_topvars_chg_eco_update.csv", row.names=FALSE)

R2max <- aggregate(varimp$R2,by=list("type"=varimp$type, "species"=varimp$species), FUN="max")
names(R2max)[3] <- "R2"
varimpmax <- merge(varimp, R2max, by=c("R2","species","type"),all.y=TRUE)

s <- aggregate(varimp$R2, by=list(varimp$species),FUN="max") #41 species
write.csv(s, file="G:/Boreal/InterannualVariability/_speciesR2_eco_update.csv")

varimpsum <- aggregate(varimp[,c(2,9)],by=list("type"=varimp$type), FUN="sum")	
write.csv(varimpsum, file="G:/Boreal/InterannualVariability/_varimpsum_chg_eco_update.csv", row.names=FALSE)

varimpsum2 <- aggregate(varimp[,c(2,9)],by=list("type2"=varimp$level3), FUN="sum")	
write.csv(varimpsum2, file="G:/Boreal/InterannualVariability/_varimpsum_chg_eco2_update.csv", row.names=FALSE)

varimpmean <- aggregate(varimp[,c(2,9)],by=list(varimp$type), FUN="mean")	
write.csv(varimpmean, file="G:/Boreal/InterannualVariability/_varimpmean_chg_eco_update.csv", row.names=FALSE)

varimpmean2 <- aggregate(varimp[,c(2,9)],by=list(varimp$level3), FUN="mean")	
write.csv(varimpmean2, file="G:/Boreal/InterannualVariability/_varimpmean_chg_eco2_update.csv", row.names=FALSE)

