library(raster)
library(reshape)
library(stringr)
library(rasterVis)
require(gridExtra) # also loads grid
require(lattice)
gpclibPermit()
library(maptools)
library(rgdal)
library(rgeos)
library(reshape2)
#library(stringr)

eco2 <- rgdal::readOGR("I:/GIS/ecoregions/CEC/WesternHemisphere_CEC_Eco_Level2_minusboreal_dissolve.shp")
eco2r <- raster("I:/GIS/ecoregions/CEC/westhem2.asc")
ecob <- rgdal::readOGR("I:/GIS/ecoregions/CEC/boreal_ecoregions_DD.shp")
countries <- rgdal::readOGR("I:/GIS/basemaps/world_countries_boundary_file_world_2002.shp")

speclist <- read.csv("I:/BAM/BAMData/SpeciesClassesModv5.csv")
spec <- read.csv("I:/BAM/BAMData/species.csv")
speclist <- merge(speclist,spec[,c(1,4)], by.x="spp", by.y="SPECIES")
speclist$spp <- gsub("YWAR","YEWA",speclist$spp)
speclist <- speclist[speclist$spp != "PIWA",]
speclist <- speclist[speclist$spp != "SEWR",]
LDspec <- as.factor(as.character(speclist[speclist$DATABASE_MIG_TYPE=="LD",1]))

varimp <- read.csv("L:/Boreal/InterannualVariability/_varimp_chg_byregion.csv")
#varimp$wint <- str_extract_all(varimp$var,"\\(?[50-300]+\\)?")
varimp$var <- gsub("\\Q.\\E","-",varimp$var)
varimp$wint <- str_split(varimp$var,"-",n=2)
for (k in 1:nrow(varimp)) {
	varimp$wint1[k] <- unlist(varimp$wint[k])[2]
	}
varimp$wint1 <- as.integer(as.character(varimp$wint1))
varimp <- varimp[varimp$wint1 > 0,]
varimp <- na.omit(varimp)

is_try_error <- function(x) inherits(x, "try-error")

#color <- c("orange","yellow","white")
#color_transparent <- adjustcolor(color, alpha.f = 0.3) 
blues <- c("#99AF29", "#416233", "#19413C", "#0058A6")
bluest <- adjustcolor(blues, alpha.f=0.3)

pdf("L:/Boreal/InterannualVariability/_migconnmaps2.pdf")
for (i in 1:length(LDspec)) {
	vv <- varimp[varimp$species== as.character(LDspec[i]),]
	v1 <- vv[(vv$abs.inf >= 0.5),]
	v2 <- vv[(vv$rel.inf >= 2),]
	v <- unique(rbind(v1,v2))
	v <- v[v$wint1 != 61,]
	v <- v[v$wint1 != 52,]
	if(nrow(v)>0) {
	range <- rgdal::readOGR(paste("I:/GIS/NatureServe/CODES/",LDspec[i],".shp",sep=""))
	range$ORIGIN<-as.factor(range$ORIGIN) 
	range@data$COLOUR <- "#FFFFFF" 
	range@data$COLOUR[range@data$ORIGIN == 4] <- adjustcolor("yellow", alpha.f=0.3) 
	range@data$COLOUR[range@data$ORIGIN == 3] <- adjustcolor("orange", alpha.f=0.3)
	range@data$COLOUR[range@data$ORIGIN == 2] <- adjustcolor("white", alpha.f=0.3)
	#range@data$COLOUR[range@data$ORIGIN == 1] <- adjustcolor("white", alpha.f=0.3)
	clust <- raster(paste("L:/Boreal/InterannualVariability/",LDspec[i],"abundyearclust.tif",sep=""))
	clust <- extend(clust, eco2r)
	rangenb <- range[range@data$ORIGIN != 2,]
	ranger <- rasterize(range,clust)
	clust <- mask(clust, ranger)
	clustp <- rasterToPolygons(clust,dissolve=TRUE)
	cc <- as.data.frame(coordinates(clustp))
	cc$id <- head(clustp)
	names(cc) <- c("X","Y","id")
	plot(countries, ylim=c(-48,71), xlim=c(-174,-32), border="light gray")
	plot(eco2, border="gray", add=TRUE)
	#plot(clust,add=TRUE, col=c("#753742", "#AA5042", "#D8BD8A", "#D8D78F"))
	plot(clust,add=TRUE, col=blues,legend=FALSE)
	plot(ecob, border="dark gray", add=TRUE)
	plot(range, col=range$COLOUR,add=TRUE)
	levels <- as.character(unlist(levels(as.factor(clust))))
	legend("topright", title = LDspec[i], legend = as.list(levels), fill = blues, cex=0.8, bg="white")
	projection(eco2) <- projection(range)
	rangernb <- rasterize(rangenb,clust)
	rangep <- rasterToPolygons(rangernb)
	eco3 <- crop(eco2,rangep)
	for (j in 1:nrow(v)) {
		if(!is.na(str_match(v$type[j],"forest loss"))==TRUE) {
			cr <- cc[cc[,3]==v$region[j],]
			breed <- cr[1,1:2]
			wint <- coordinates(eco3[eco3$GridCode==v$wint1[j],])
			if(v$abs.inf[j] > 1) {
				segments(as.numeric(breed[1]),as.numeric(breed[2]),as.numeric(wint[1]),as.numeric(wint[2]), lwd=2, lty=2, col="red")
				} else {
				segments(as.numeric(breed[1]),as.numeric(breed[2]),as.numeric(wint[1]),as.numeric(wint[2]), lwd=1, lty=2, col="red")
				}
			}
		if(!is.na(str_match(v$type[j],"weather"))==TRUE){
			cr <- cc[cc[,3]==v$region[j],]
			breed <- cr[1,1:2]
			wint <- coordinates(eco3[eco3$GridCode==v$wint1[j],])
			if(v$abs.inf[j] > 1) {
				segments(as.numeric(breed[1]),as.numeric(breed[2]),as.numeric(wint[1]),as.numeric(wint[2]), lwd=2, lty=5, col="black")
				} else {
				segments(as.numeric(breed[1]),as.numeric(breed[2]),as.numeric(wint[1]),as.numeric(wint[2]), lwd=1, lty=5, col="black")
				}
			}
		}
	pointLabel(coordinates(eco2),labels=eco2$GridCode, cex=0.6)
	}                          
	}
dev.off()
