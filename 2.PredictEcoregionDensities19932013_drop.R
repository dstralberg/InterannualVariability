library(raster)
library(data.table)

load("L:/Boreal/InterannualVariability/Summary19912013.RData")
load("I:/BAM/BAMData/offsets-v3_2016-04-18.Rdata")
indices <- read.csv("L:/Boreal/InterannualVariability/nao_pdo_soi_amo_annual.csv")
library(reshape)
library(stringr)
is_try_error <- function(x) inherits(x, "try-error")
intersect <- function(x, y) y[match(x, y, nomatch = 0)]

surveydate <- data.table(unique(PKEY[
,c(1,2,8)]))
surveydate$YEAR <- as.factor(surveydate$YEAR)
combo$ecodrop <- as.character(combo$ecodrop)
combo1 <- combo[combo$nalc %in% c(1,2,5,6),]
combo1 <- na.omit(combo1)
combo1$nalc <- as.factor(as.character(combo1$nalc))
combo1$ecodrop <- as.factor(combo1$ecodrop)
zz <- read.csv("L:/Boreal/InterannualVariability/ClusterClimDat19912013_drop.csv")
z <- read.csv("L:/Boreal/InterannualVariability/BreedingLanduseDat20012014_drop.csv")
names(z) <- c("ecodrop","forestchg","YEAR")
z$ecodrop <- as.character(z$ecodrop)
names(zz)[1] <- "ecodrop"
zz$ecodrop <- as.character(zz$ecodrop)

XY <- base::unique(combo1[,c(3,5:6,10:53,58,60)])
XY1 <- XY[!XY$ecodrop %in% c(7,10,11,45,46),]
XY1 <- data.table(XY1)
XY1$ecodrop <- as.factor(as.character(XY1$ecodrop))
XY1$YEAR <- as.factor(as.character(XY1$YEAR))
clust <- raster("L:/Boreal/InterannualVariability/boreal_ecoregions_drop.asc")
offl <- data.table(melt(OFF))
names(offl) <- c("PKEY","SPECIES","logoffset")
offl$SPECIES <- as.character(offl$SPECIES)

speclist <- read.csv("I:/BAM/BAMData/SpeciesClassesModv5.csv")
spec <- read.csv("I:/BAM/BAMData/species.csv")
speclist <- merge(speclist,spec[,c(1,4)], by.x="spp", by.y="SPECIES")
speclist$spp <- gsub("YWAR","YEWA",speclist$spp)
LDspec <- as.factor(as.character(speclist[speclist$DATABASE_MIG_TYPE=="LD",1]))

for (j in 1:length(LDspec)) {
	specdat <- data.table(PCTBL[PCTBL$SPECIES == as.character(LDspec[j]),2:5])
	specdat$SS <- as.factor(specdat$SS)
	specdat <- merge(surveydate, specdat, by = c("SS", "PKEY"), all.x=TRUE)
	specdat <- merge(specdat, XY1, by = c("SS","YEAR"))
	specdat$SPECIES <- as.character(LDspec[j])
	specdat$ABUND <- as.integer(ifelse(is.na(specdat$ABUND),0,specdat$ABUND))
	specdat <- merge(specdat,offl, by=c("SPECIES","PKEY"))
	specdat$YEAR <- as.integer(as.character(specdat$YEAR))
	specdat <- na.omit(specdat)
	specdat$nalc <- as.factor(as.character(specdat$nalc))
	freq <- melt(table(specdat$ecodrop,specdat$nalc))
	names(freq) <- c("ecodrop","nalc","freq")
	freq <- freq[freq$freq >= 25,]
	freq$ecodrop <- as.factor(as.character(freq$ecodrop))
	freq$nalc <- as.factor(as.character(freq$nalc))
	specdat <- merge(specdat,freq,by=c("ecodrop","nalc"))
	pred <- data.table(YEAR = character(),ecodrop = character(), nalc = character(), SPECIES = character(), pred=double())
	for (i in 1993:2013) {
		yeardat <- specdat[specdat$YEAR == i,]
		yeardat$ecocov <- interaction(yeardat$nalc, yeardat$ecodrop, drop=TRUE, sep="_")
		n <- length(levels(as.factor(as.character(yeardat$ecodrop))))
		if (n>1) {
			x <- interaction(yeardat$nalc, yeardat$ecodrop, drop=TRUE, sep="_")
			cluster <- data.table(levels(x))
			names(cluster)[1] <- "ecocov"
			cluster$ecocov <- as.factor(as.character(cluster$ecocov))
			l <- str_split(cluster$ecocov,"_")
			d <- data.table(matrix(unlist(l), nrow=length(l), ncol=2, byrow=T))
			cluster$YEAR <- as.character(i)
			cluster$ecodrop <- as.character(d[[2]])
			cluster$nalc <- as.character(d[[1]])
			cluster$logoffset <- 0
			cluster$SPECIES <- as.character(LDspec[j])
			a <- try(gridabund <- glm(data=yeardat, ABUND ~ ecocov, offset = logoffset, family="poisson"))
			if (is_try_error(a) == FALSE) {cluster$pred <- predict.glm(gridabund, newdata=cluster, type="response")
			pred <- rbind(pred,cluster[,c(2:4,6:7),with=FALSE])}
		} else { 	
			if (n>0) {
			cluster <- data.table(levels(yeardat$nalc))
			names(cluster)[1] <- "nalc"
			cluster$YEAR <- i
			cluster$ecodrop <- base::unique(yeardat$ecodrop)
			cluster$logoffset <- 0
			cluster$SPECIES <- LDspec[j]
			a <- try(gridabund <- glm(data=yeardat, ABUND ~ nalc, offset = logoffset, family="poisson"))
			if (is_try_error(a) == FALSE) {cluster$pred <- predict(gridabund, newdata=cluster, type="response")
			pred <- rbind(pred,cluster[,c(2:4,6:7),with=FALSE])}
			}
		}	
	}
	pred$YEAR <- as.integer(as.character(pred$YEAR))
	write.csv(pred,file=paste("K:/Boreal/InterannualVariability/ecopred_1993-2013_",LDspec[j],"drop.csv",sep=""),row.names=FALSE)
	#dat <- merge(pred,z,by=c("YEAR","ecodrop"))
	#dat$ecodrop <- as.integer(as.character(dat$ecodrop))
	dat <- merge(pred,zz[,c(1,3:ncol(zz))],by=c("YEAR","ecodrop"))
	dat$ecodrop <- as.factor(dat$ecodrop)
	dat$nalc <- as.factor(dat$nalc)
	write.csv(dat, file=paste("K:/Boreal/InterannualVariability/predclim19932013_",LDspec[j],"drop.csv",sep=""), row.names=FALSE)
}

	  