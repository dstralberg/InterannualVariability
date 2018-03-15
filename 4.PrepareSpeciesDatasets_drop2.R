library(raster)
library(data.table)
load("L:/Boreal/InterannualVariability/Summary19912013.RData")
load("I:/BAM/BAMData/offsets-v3_2016-04-18.Rdata")
indices <- read.csv("L:/Boreal/InterannualVariability/nao_pdo_soi_amo_annual.csv")
library(reshape)
library(stringr)
require(maptree)
is_try_error <- function(x) inherits(x, "try-error")
intersect <- function(x, y) y[match(x, y, nomatch = 0)]
rainbow15 <- c("grey","black","brown","maroon","red","orange","yellow","lightgreen","green","darkgreen","turquoise","blue","purple","violet","pink")

surveydate <- data.table(unique(PKEY[,c(1,2,8)]))
surveydate$YEAR <- as.factor(surveydate$YEAR)
combo$ecodrop <- as.character(combo$ecodrop)
combo1 <- combo[combo$nalc %in% c(1,2,5,6),]
combo1 <- na.omit(combo1)
combo1$nalc <- ifelse(combo1$nalc == 2, 1, combo1$nalc)
combo1$nalc <- as.factor(as.character(combo1$nalc))
combo1$ecodrop <- as.factor(combo1$ecodrop)

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

filter <- read.csv("L:/Boreal/InterannualVariability/variablefilters.csv")
filter$cluster <- as.character(filter$cluster)
speclist <- read.csv("I:/BAM/BAMData/SpeciesClassesModv5.csv")
spec <- read.csv("I:/BAM/BAMData/species.csv")
speclist <- merge(speclist,spec[,c(1,4)], by.x="spp", by.y="SPECIES")
speclist$spp <- gsub("YWAR","YEWA",speclist$spp)
speclist <- speclist[speclist$spp != "PIWA",]
LDspec <- as.factor(as.character(speclist[speclist$DATABASE_MIG_TYPE=="LD",1]))

zz <- read.csv("L:/Boreal/InterannualVariability/ClusterClimDat19912013_drop.csv")
zz$zone <- as.character(zz$zone)
names(zz)[1] <- "ecodrop"
zz$YEAR <- as.integer(as.character(zz$YEAR))
zz <- zz[zz$ecodrop !=1,]
zz$sepoctpre0 <- zz$seppre0 + zz$octpre0
zz$novmarpre <- zz$novpre0 + zz$decpre0 + zz$janpre1 + zz$febpre1 + zz$marpre1
zz$sepocttmin0 <- (zz$septmin0 + zz$octtmin0)/2
zz$novmartmin <- (zz$novtmin0 + zz$dectmin0 + zz$jantmin1 + zz$febtmin1 + zz$martmin1)/5
zz <- zz[,c(1:14,36,38:39,41:42,44:49)]

ww <- read.csv("L:/Boreal/InterannualVariability/annclimDat19912013_CEC.csv")
ww$zone <- as.character(ww$zone)
names(ww) <- paste(names(ww),"w",sep="")
ww <- ww[,c(1,11,13,14,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,44)]

yy <- read.csv("L:/Boreal/InterannualVariability/WinterLanduseDat20012014_CEC.csv")

ll <- read.csv("L:/Boreal/InterannualVariability/BreedingLanduseDat20012014_drop.csv")
ll$zone <- as.character(ll$zone)
names(ll)[1] <- "ecodrop"
ll$YEAR <- as.integer(as.character(ll$YEAR))
ll <- ll[ll$ecodrop !=1,]
names(ll) <- c("ecodrop","forestchg","YEAR")

sample <- merge(surveydate, XY1, by = c("SS","YEAR"))
freqall <- melt(table(sample$ecodrop,sample$nalc))
names(freqall) <- c("ecodrop","nalc","freq")

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
	specdat$count <- 1
	sample <- aggregate(specdat$count,by=list("ecodrop"=specdat$ecodrop,"nalc"=specdat$nalc,"YEAR"=specdat$YEAR),FUN=sum)
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
	pred <- merge(pred,sample,by=c("ecodrop","nalc","YEAR"))
	write.csv(pred,file=paste("L:/Boreal/InterannualVariability/ecopred_1993-2013",LDspec[j],"2.csv",sep=""),row.names=FALSE)
	
	f <- filter[filter$Species == LDspec[j],]
	w <- merge(ww[,c(1,6:19,22)], f[,4:6], by.x = "zonew", by.y = "cluster")
	w <- w[w$wintering > 0,]
	w <- as.data.frame(w)
	w <- w[,1:16]
	w1 <- reshape(w, direction="wide", timevar="zonew", idvar="YEARw")
	w1$YEARw <- as.integer(as.character(w1$YEARw))
	
	f <- filter[filter$Species == LDspec[j],]
	m <- merge(ww[,c(1:5,20:22)], f[,4:6], by.x = "zonew", by.y = "cluster")
    m <- m[m$migration > 0,]
	m <- as.data.frame(m)
	m <- m[,1:8]
	m1 <- reshape(m, direction="wide", timevar="zonew", idvar="YEARw")
	m1$YEARw <- as.integer(as.character(m1$YEARw))

	y <- merge(yy, f[,4:6], by.x="zone", by.y="cluster")
	y$use <- y$wintering + y$migration
	y <- y[y$use > 0,]
	y <- y[,1:3]
	names(y) <- c("zone","forestchg","YEAR")
	y1 <- reshape(y, direction="wide", timevar="zone", idvar="YEAR")
	
	dat <- merge(pred,w1,by.x="YEAR",by.y="YEARw")
	dat <- merge(dat,m1,by.x="YEAR",by.y="YEARw")
	#dat$ecoreg <- as.integer(as.character(dat$ecodrop))
	dat <- merge(dat,zz[,c(1,3:ncol(zz))],by=c("YEAR","ecodrop"))
	dat$ecodrop <- as.factor(dat$ecodrop)
	dat$nalc <- as.factor(dat$nalc)
	dat <- merge(dat,y1,by="YEAR")
	dat <- merge(dat,ll,by=c("YEAR","ecodrop"))
	write.csv(dat, file=paste("L:/Boreal/InterannualVariability/predclim19932013",LDspec[j],"2.csv",sep=""), row.names=FALSE)
}

