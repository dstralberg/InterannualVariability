library(rpart)
library(rpart.plot)
library(tree)
library(gbm)
library(dismo)
library(raster)
library(pdp)
library(gridExtra)

is_try_error <- function(x) inherits(x, "try-error")

speclist <- read.csv("I:/BAM/BAMData/SpeciesClassesModv5.csv")
spec <- read.csv("I:/BAM/BAMData/species.csv")
speclist <- merge(speclist,spec[,c(1,4)], by.x="spp", by.y="SPECIES")
speclist$spp <- gsub("YWAR","YEWA",speclist$spp)
speclist <- speclist[speclist$spp != "PIWA",]
LDspec <- as.factor(as.character(speclist[speclist$DATABASE_MIG_TYPE=="LD",1]))

indices <- read.csv("L:/Boreal/InterannualVariability/nao_pdo_soi_amo_annual.csv")
ind <- indices[,c(1:25,32:35,44:47)]
ecolu <- read.csv("L:/Boreal/InterannualVariability/ecoregion_lu.csv") 
ecolu$BorealLevel3 <- as.factor(as.character(ecolu$BorealLevel3))

pdf("L:/Boreal/InterannualVariability/_partialdependence.pdf")
for (j in 1:length(LDspec)) {
	try(load(paste("L:/Boreal/InterannualVariability/",LDspec[j],"brt_2001-2013_chg_eco.RData",sep="")))
	if(is.null(brt.chg)) {} else {
	varimp <- as.data.frame(brt.chg$contributions)
	grid.arrange(
		partial(brt.chg, pred.var = as.character(varimp[1,1]), train=brt.chg$gbm.call$dataframe, n.trees=brt.chg$n.trees, ) %>%
		plotPartial(lwd = 2, ylab = "density change", smooth=TRUE),
		partial(brt.chg, pred.var = as.character(varimp[2,1]), train=brt.chg$gbm.call$dataframe, n.trees=brt.chg$n.trees, ) %>%
		plotPartial(lwd = 2, ylab = NULL, smooth=TRUE),
		partial(brt.chg, pred.var = as.character(varimp[3,1]), train=brt.chg$gbm.call$dataframe, n.trees=brt.chg$n.trees, ) %>%
		plotPartial(lwd = 2, ylab = NULL, smooth=TRUE),
		partial(brt.chg, pred.var = as.character(varimp[4,1]), train=brt.chg$gbm.call$dataframe, n.trees=brt.chg$n.trees, ) %>%
		plotPartial(lwd = 2, ylab = "density change", smooth=TRUE),
		partial(brt.chg, pred.var = as.character(varimp[5,1]), train=brt.chg$gbm.call$dataframe, n.trees=brt.chg$n.trees, ) %>%
		plotPartial(lwd = 2, ylab = NULL, smooth=TRUE),
		partial(brt.chg, pred.var = as.character(varimp[6,1]), train=brt.chg$gbm.call$dataframe, n.trees=brt.chg$n.trees, ) %>%
		plotPartial(lwd = 2, ylab = NULL, smooth=TRUE),
		partial(brt.chg, pred.var = as.character(varimp[7,1]), train=brt.chg$gbm.call$dataframe, n.trees=brt.chg$n.trees, ) %>%
		plotPartial(lwd = 2, ylab = "density change", smooth=TRUE),
		partial(brt.chg, pred.var = as.character(varimp[8,1]), train=brt.chg$gbm.call$dataframe, n.trees=brt.chg$n.trees, ) %>%
		plotPartial(lwd = 2, ylab = NULL, smooth=TRUE),
		partial(brt.chg, pred.var = as.character(varimp[9,1]), train=brt.chg$gbm.call$dataframe, n.trees=brt.chg$n.trees, ) %>%
		plotPartial(lwd = 2, ylab = NULL, smooth=TRUE),
		ncol=3,
		top= as.character(LDspec[j])
		)
		}
	}	
dev.off()

pdf("L:/Boreal/InterannualVariability/_partialdependence_clust.pdf")
clust <- list.files("L:/Boreal/InterannualVariability/", pattern="brt_2001-2013_chg_cluster.RData")
for (j in 1:length(clust)) {
	try(load(paste("L:/Boreal/InterannualVariability/",clust[j],sep="")))
	if(is.null(brt1)) {} else {
	varimp <- as.data.frame(brt1$contributions)
	grid.arrange(
		partial(brt1, pred.var = as.character(varimp[1,1]), train=brt1$gbm.call$dataframe, n.trees=brt1$n.trees, ) %>%
		plotPartial(lwd = 2, ylab = "density change", smooth=TRUE),
		partial(brt1, pred.var = as.character(varimp[2,1]), train=brt1$gbm.call$dataframe, n.trees=brt1$n.trees, ) %>%
		plotPartial(lwd = 2, ylab = NULL, smooth=TRUE),
		partial(brt1, pred.var = as.character(varimp[3,1]), train=brt1$gbm.call$dataframe, n.trees=brt1$n.trees, ) %>%
		plotPartial(lwd = 2, ylab = NULL, smooth=TRUE),
		partial(brt1, pred.var = as.character(varimp[4,1]), train=brt1$gbm.call$dataframe, n.trees=brt1$n.trees, ) %>%
		plotPartial(lwd = 2, ylab = "density change", smooth=TRUE),
		partial(brt1, pred.var = as.character(varimp[5,1]), train=brt1$gbm.call$dataframe, n.trees=brt1$n.trees, ) %>%
		plotPartial(lwd = 2, ylab = NULL, smooth=TRUE),
		partial(brt1, pred.var = as.character(varimp[6,1]), train=brt1$gbm.call$dataframe, n.trees=brt1$n.trees, ) %>%
		plotPartial(lwd = 2, ylab = NULL, smooth=TRUE),
		partial(brt1, pred.var = as.character(varimp[7,1]), train=brt1$gbm.call$dataframe, n.trees=brt1$n.trees, ) %>%
		plotPartial(lwd = 2, ylab = "density change", smooth=TRUE),
		partial(brt1, pred.var = as.character(varimp[8,1]), train=brt1$gbm.call$dataframe, n.trees=brt1$n.trees, ) %>%
		plotPartial(lwd = 2, ylab = NULL, smooth=TRUE),
		partial(brt1, pred.var = as.character(varimp[9,1]), train=brt1$gbm.call$dataframe, n.trees=brt1$n.trees, ) %>%
		plotPartial(lwd = 2, ylab = NULL, smooth=TRUE),
		ncol=3,
		top= as.character(substr(clust[j],1,5))
		)
		}
	}	
dev.off()

pdf("L:/Boreal/InterannualVariability/_partialdependence_ice.pdf")
for (j in 1:length(LDspec)) {
	try(load(paste("L:/Boreal/InterannualVariability/",LDspec[j],"brt_2001-2013_chg_eco.RData",sep="")))
	if(is.null(brt.chg)) {} else {
	varimp <- as.data.frame(brt.chg$contributions)
	grid.arrange(
		partial(brt.chg, ice = TRUE, pred.var = as.character(varimp[1,1]), train=brt.chg$gbm.call$dataframe, n.trees=brt.chg$n.trees) %>%
		plotPartial(lwd = 2, ylab = "density change", smooth=TRUE),
		partial(brt.chg, ice = TRUE, pred.var = as.character(varimp[2,1]), train=brt.chg$gbm.call$dataframe, n.trees=brt.chg$n.trees, ) %>%
		plotPartial(lwd = 2, ylab = NULL, smooth=TRUE),
		partial(brt.chg, ice = TRUE, pred.var = as.character(varimp[3,1]), train=brt.chg$gbm.call$dataframe, n.trees=brt.chg$n.trees, ) %>%
		plotPartial(lwd = 2, ice = TRUE, ylab = NULL, smooth=TRUE),
		partial(brt.chg, ice = TRUE, pred.var = as.character(varimp[4,1]), train=brt.chg$gbm.call$dataframe, n.trees=brt.chg$n.trees, ) %>%
		plotPartial(lwd = 2, ylab = "density change", smooth=TRUE),
		partial(brt.chg, ice = TRUE, pred.var = as.character(varimp[5,1]), train=brt.chg$gbm.call$dataframe, n.trees=brt.chg$n.trees, ) %>%
		plotPartial(lwd = 2, ylab = NULL, smooth=TRUE),
		partial(brt.chg, ice = TRUE, pred.var = as.character(varimp[6,1]), train=brt.chg$gbm.call$dataframe, n.trees=brt.chg$n.trees, ) %>%
		plotPartial(lwd = 2, ylab = NULL, smooth=TRUE),
		partial(brt.chg, ice = TRUE, pred.var = as.character(varimp[7,1]), train=brt.chg$gbm.call$dataframe, n.trees=brt.chg$n.trees, ) %>%
		plotPartial(lwd = 2, ylab = "density change", smooth=TRUE),
		partial(brt.chg, ice = TRUE, pred.var = as.character(varimp[8,1]), train=brt.chg$gbm.call$dataframe, n.trees=brt.chg$n.trees, ) %>%
		plotPartial(lwd = 2, ylab = NULL, smooth=TRUE),
		partial(brt.chg, ice = TRUE, pred.var = as.character(varimp[9,1]), train=brt.chg$gbm.call$dataframe, n.trees=brt.chg$n.trees, ) %>%
		plotPartial(lwd = 2, ylab = NULL, smooth=TRUE),
		ncol=3,
		top= as.character(LDspec[j])
		)
		}
	}	
dev.off()