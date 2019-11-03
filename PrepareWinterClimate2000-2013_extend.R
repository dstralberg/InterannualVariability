# Open libraries for packages used
library(raster)
library(rgdal)
library(sp)
library(maptools)

wintid <- raster("G:/Boreal/InterannualVariability/ceclevel2nsa1.tif")
wintid <- raster("G:/Boreal/InterannualVariability/wintid.asc")

cur <- "E:/CRUTS31/new_anomalies/"
clim1999 <- list.files(cur, pattern ="1999.asc$")
clim2000 <- list.files(cur, pattern ="2000.asc$")
clim2001 <- list.files(cur, pattern ="2001.asc$")
clim2002 <- list.files(cur, pattern ="2002.asc$")
clim2003 <- list.files(cur, pattern ="2003.asc$")
clim2004 <- list.files(cur, pattern ="2004.asc$")
clim2005 <- list.files(cur, pattern ="2005.asc$")
clim2006 <- list.files(cur, pattern ="2006.asc$")
clim2007 <- list.files(cur, pattern ="2007.asc$")
clim2008 <- list.files(cur, pattern ="2008.asc$")
clim2009 <- list.files(cur, pattern ="2009.asc$")
clim2010 <- list.files(cur, pattern ="2010.asc$")
clim2011 <- list.files(cur, pattern ="2011.asc$")
clim2012 <- list.files(cur, pattern ="2012.asc$")
clim2013 <- list.files(cur, pattern ="2013.asc$")

curSA <- "E:/CRUTS31/ClimateSA/"
setwd(curSA)
saclim1999 <- list.files(curSA, pattern="1999.asc$")
saclim2000 <- list.files(curSA, pattern="2000.asc$")
saclim2001 <- list.files(curSA, pattern="2001.asc$")
saclim2002 <- list.files(curSA, pattern="2002.asc$")
saclim2003 <- list.files(curSA, pattern="2003.asc$")
saclim2004 <- list.files(curSA, pattern="2004.asc$")
saclim2005 <- list.files(curSA, pattern="2005.asc$")
saclim2006 <- list.files(curSA, pattern="2006.asc$")
saclim2007 <- list.files(curSA, pattern="2007.asc$")
saclim2008 <- list.files(curSA, pattern="2008.asc$")
saclim2009 <- list.files(curSA, pattern="2009.asc$")
saclim2010 <- list.files(curSA, pattern ="2010.asc$")
saclim2011 <- list.files(curSA, pattern ="2011.asc$")
saclim2012 <- list.files(curSA, pattern ="2012.asc$")
saclim2013 <- list.files(curSA, pattern ="2013.asc$")


#Create climate stacks for each year preceding June surveys
	
	setwd(cur)						
annclim2000 <- stack(raster(clim1999[25]), raster(clim1999[26]), raster(clim1999[27]), #May 1999
						raster(clim1999[19]), raster(clim1999[20]), raster(clim1999[21]), #June 1999
						raster(clim1999[16]), raster(clim1999[17]), raster(clim1999[18]), #July 1999
						raster(clim1999[4]), raster(clim1999[5]), raster(clim1999[6]), #Aug 1999
						raster(clim1999[34]), raster(clim1999[35]), raster(clim1999[36]), #Sept 1999
						raster(clim1999[31]), raster(clim1999[32]), raster(clim1999[33]), #Oct 1999
						raster(clim1999[28]), raster(clim1999[29]), raster(clim1999[30]), #Nov 1999
						raster(clim1999[7]), raster(clim1999[8]), raster(clim1999[9]), #Dec 1999
						raster(clim2000[13]), raster(clim2000[14]), raster(clim2000[15]), #Jan 2000
						raster(clim2000[10]), raster(clim2000[11]), raster(clim2000[12]), #Feb 2000
						raster(clim2000[22]), raster(clim2000[23]), raster(clim2000[24]), #Mar 2000
						raster(clim2000[1]), raster(clim2000[2]), raster(clim2000[3]), #Apr 2000
						raster(clim2000[25]), raster(clim2000[26]), raster(clim2000[27]), #May 2000
						raster(clim2000[19]), raster(clim2000[20]), raster(clim2000[21])) #June 2000							
names(annclim2000) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")
					

annclim2001 <- stack(raster(clim2000[25]), raster(clim2000[26]), raster(clim2000[27]), #May 2000
						raster(clim2000[19]), raster(clim2000[20]), raster(clim2000[21]), #June 2000
						raster(clim2000[16]), raster(clim2000[17]), raster(clim2000[18]), #July 2000
						raster(clim2000[4]), raster(clim2000[5]), raster(clim2000[6]), #Aug 2000
						raster(clim2000[34]), raster(clim2000[35]), raster(clim2000[36]), #Sept 2000
						raster(clim2000[31]), raster(clim2000[32]), raster(clim2000[33]), #Oct 2000
						raster(clim2000[28]), raster(clim2000[29]), raster(clim2000[30]), #Nov 2000
						raster(clim2000[7]), raster(clim2000[8]), raster(clim2000[9]), #Dec 2000
						raster(clim2001[13]), raster(clim2001[14]), raster(clim2001[15]), #Jan 2001
						raster(clim2001[10]), raster(clim2001[11]), raster(clim2001[12]), #Feb 2001
						raster(clim2001[22]), raster(clim2001[23]), raster(clim2001[24]), #Mar 2001
						raster(clim2001[1]), raster(clim2001[2]), raster(clim2001[3]), #Apr 2001
						raster(clim2001[25]), raster(clim2001[26]), raster(clim2001[27]), #May 2001
						raster(clim2001[19]), raster(clim2001[20]), raster(clim2001[21])) #June 2001	
names(annclim2001) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")
					
						
annclim2002 <- stack(raster(clim2003[25]), raster(clim2003[26]), raster(clim2003[27]), #May 2003
						raster(clim2003[19]), raster(clim2003[20]), raster(clim2003[21]), #June 2003
						raster(clim2001[16]), raster(clim2001[17]), raster(clim2001[18]), #July 2001
						raster(clim2001[4]), raster(clim2001[5]), raster(clim2001[6]), #Aug 2001
						raster(clim2001[34]), raster(clim2001[35]), raster(clim2001[36]), #Sept 2001
						raster(clim2001[31]), raster(clim2001[32]), raster(clim2001[33]), #Oct 2001
						raster(clim2001[28]), raster(clim2001[29]), raster(clim2001[30]), #Nov 2001
						raster(clim2001[7]), raster(clim2001[8]), raster(clim2001[9]), #Dec 2001
						raster(clim2002[13]), raster(clim2002[14]), raster(clim2002[15]), #Jan 2002
						raster(clim2002[10]), raster(clim2002[11]), raster(clim2002[12]), #Feb 2002
						raster(clim2002[22]), raster(clim2002[23]), raster(clim2002[24]), #Mar 2002
						raster(clim2002[1]), raster(clim2002[2]), raster(clim2002[3]), #Apr 2002
						raster(clim2002[25]), raster(clim2002[26]), raster(clim2002[27]), #May 2002
						raster(clim2002[19]), raster(clim2002[20]), raster(clim2002[21])) #June 2002	
names(annclim2002) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")
					
																		
annclim2003 <- stack(raster(clim2003[25]), raster(clim2003[26]), raster(clim2003[27]), #May 2003
						raster(clim2003[19]), raster(clim2003[20]), raster(clim2003[21]), #June 2003
						raster(clim2002[16]), raster(clim2002[17]), raster(clim2002[18]), #July 2002
						raster(clim2002[4]), raster(clim2002[5]), raster(clim2002[6]), #Aug 2002
						raster(clim2002[34]), raster(clim2002[35]), raster(clim2002[36]), #Sept 2002
						raster(clim2002[31]), raster(clim2002[32]), raster(clim2002[33]), #Oct 2002
						raster(clim2002[28]), raster(clim2002[29]), raster(clim2002[30]), #Nov 2002
						raster(clim2002[7]), raster(clim2002[8]), raster(clim2002[9]), #Dec 2002
						raster(clim2003[13]), raster(clim2003[14]), raster(clim2003[15]), #Jan 2003
						raster(clim2003[10]), raster(clim2003[11]), raster(clim2003[12]), #Feb 2003
						raster(clim2003[22]), raster(clim2003[23]), raster(clim2003[24]), #Mar 2003
						raster(clim2003[1]), raster(clim2003[2]), raster(clim2003[3]), #Apr 2003
						raster(clim2003[25]), raster(clim2003[26]), raster(clim2003[27]), #May 2003
						raster(clim2003[19]), raster(clim2003[20]), raster(clim2003[21])) #June 2003
names(annclim2003) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")

annclim2004 <- stack( raster(clim2003[25]), raster(clim2003[26]), raster(clim2003[27]), #May 2003
						raster(clim2003[19]), raster(clim2003[20]), raster(clim2003[21]), #June 2003
						raster(clim2003[16]), raster(clim2003[17]), raster(clim2003[18]), #July 2003
						raster(clim2003[4]), raster(clim2003[5]), raster(clim2003[6]), #Aug 2003
						raster(clim2003[34]), raster(clim2003[35]), raster(clim2003[36]), #Sept 2003
						raster(clim2003[31]), raster(clim2003[32]), raster(clim2003[33]), #Oct 2003
						raster(clim2003[28]), raster(clim2003[29]), raster(clim2003[30]), #Nov 2003
						raster(clim2003[7]), raster(clim2003[8]), raster(clim2003[9]), #Dec 2003
						raster(clim2004[13]), raster(clim2004[14]), raster(clim2004[15]), #Jan 2004
						raster(clim2004[10]), raster(clim2004[11]), raster(clim2004[12]), #Feb 2004
						raster(clim2004[22]), raster(clim2004[23]), raster(clim2004[24]), #Mar 2004
						raster(clim2004[1]), raster(clim2004[2]), raster(clim2004[3]), #Apr 2004
						raster(clim2004[25]), raster(clim2004[26]), raster(clim2004[27]), #May 2004
						raster(clim2004[19]), raster(clim2004[20]), raster(clim2004[21])) #June 2004	
names(annclim2004) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")					
					
						
annclim2005 <- stack( raster(clim2004[25]), raster(clim2004[26]), raster(clim2004[27]), #May 2004
						raster(clim2004[19]), raster(clim2004[20]), raster(clim2004[21]), #June 2004
						raster(clim2004[16]), raster(clim2004[17]), raster(clim2004[18]), #July 2004
						raster(clim2004[4]), raster(clim2004[5]), raster(clim2004[6]), #Aug 2004
						raster(clim2004[34]), raster(clim2004[35]), raster(clim2004[36]), #Sept 2004
						raster(clim2004[31]), raster(clim2004[32]), raster(clim2004[33]), #Oct 2004
						raster(clim2004[28]), raster(clim2004[29]), raster(clim2004[30]), #Nov 2004
						raster(clim2004[7]), raster(clim2004[8]), raster(clim2004[9]), #Dec 2004
						raster(clim2005[13]), raster(clim2005[14]), raster(clim2005[15]), #Jan 2005
						raster(clim2005[10]), raster(clim2005[11]), raster(clim2005[12]), #Feb 2005
						raster(clim2005[22]), raster(clim2005[23]), raster(clim2005[24]), #Mar 2005
						raster(clim2005[1]), raster(clim2005[2]), raster(clim2005[3]), #Apr 2005
						raster(clim2005[25]), raster(clim2005[26]), raster(clim2005[27]), #May 2005
						raster(clim2005[19]), raster(clim2005[20]), raster(clim2005[21])) #June 2005	
names(annclim2005) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")
					
						
annclim2006 <- stack( raster(clim2005[25]), raster(clim2005[26]), raster(clim2005[27]), #May 2005
						raster(clim2005[19]), raster(clim2005[20]), raster(clim2005[21]), #June 2005
						raster(clim2005[16]), raster(clim2005[17]), raster(clim2005[18]), #July 2005
						raster(clim2005[4]), raster(clim2005[5]), raster(clim2005[6]), #Aug 2005
						raster(clim2005[34]), raster(clim2005[35]), raster(clim2005[36]), #Sept 2005
						raster(clim2005[31]), raster(clim2005[32]), raster(clim2005[33]), #Oct 2005
						raster(clim2005[28]), raster(clim2005[29]), raster(clim2005[30]), #Nov 2005
						raster(clim2005[7]), raster(clim2005[8]), raster(clim2005[9]), #Dec 2005
						raster(clim2006[13]), raster(clim2006[14]), raster(clim2006[15]), #Jan 2006
						raster(clim2006[10]), raster(clim2006[11]), raster(clim2006[12]), #Feb 2006
						raster(clim2006[22]), raster(clim2006[23]), raster(clim2006[24]), #Mar 2006
						raster(clim2006[1]), raster(clim2006[2]), raster(clim2006[3]), #Apr 2006
						raster(clim2006[25]), raster(clim2006[26]), raster(clim2006[27]), #May 2006
						raster(clim2006[19]), raster(clim2006[20]), raster(clim2006[21])) #June 2006	
names(annclim2006) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")
	
									
annclim2007 <- stack(raster(clim2006[25]), raster(clim2006[26]), raster(clim2006[27]), #May 2006
						raster(clim2006[19]), raster(clim2006[20]), raster(clim2006[21]), #June 2006
						raster(clim2006[16]), raster(clim2006[17]), raster(clim2006[18]), #July 2006
						raster(clim2006[4]), raster(clim2006[5]), raster(clim2006[6]), #Aug 2006
						raster(clim2006[34]), raster(clim2006[35]), raster(clim2006[36]), #Sept 2006
						raster(clim2006[31]), raster(clim2006[32]), raster(clim2006[33]), #Oct 2006
						raster(clim2006[28]), raster(clim2006[29]), raster(clim2006[30]), #Nov 2006
						raster(clim2006[7]), raster(clim2006[8]), raster(clim2006[9]), #Dec 2006
						raster(clim2007[13]), raster(clim2007[14]), raster(clim2007[15]), #Jan 2007
						raster(clim2007[10]), raster(clim2007[11]), raster(clim2007[12]), #Feb 2007
						raster(clim2007[22]), raster(clim2007[23]), raster(clim2007[24]), #Mar 2007
						raster(clim2007[1]), raster(clim2007[2]), raster(clim2007[3]), #Apr 2007
						raster(clim2007[25]), raster(clim2007[26]), raster(clim2007[27]), #May 2007
						raster(clim2007[19]), raster(clim2007[20]), raster(clim2007[21])) #June 2007			
names(annclim2007) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")
annclim2007 <- crop(annclim2007,wintid)	
										
annclim2008 <- stack(raster(clim2007[25]), raster(clim2007[26]), raster(clim2007[27]), #May 2007
						raster(clim2007[19]), raster(clim2007[20]), raster(clim2007[21]), #June 2007
						raster(clim2007[16]), raster(clim2007[17]), raster(clim2007[18]), #July 2007
						raster(clim2007[4]), raster(clim2007[5]), raster(clim2007[6]), #Aug 2007
						raster(clim2007[34]), raster(clim2007[35]), raster(clim2007[36]), #Sept 2007
						raster(clim2007[31]), raster(clim2007[32]), raster(clim2007[33]), #Oct 2007
						raster(clim2007[28]), raster(clim2007[29]), raster(clim2007[30]), #Nov 2007
						raster(clim2007[7]), raster(clim2007[8]), raster(clim2007[9]), #Dec 2007
						raster(clim2008[13]), raster(clim2008[14]), raster(clim2008[15]), #Jan 2008
						raster(clim2008[10]), raster(clim2008[11]), raster(clim2008[12]), #Feb 2008
						raster(clim2008[22]), raster(clim2008[23]), raster(clim2008[24]), #Mar 2008
						raster(clim2008[1]), raster(clim2008[2]), raster(clim2008[3]), #Apr 2008
						raster(clim2008[25]), raster(clim2008[26]), raster(clim2008[27]), #May 2008
						raster(clim2008[19]), raster(clim2008[20]), raster(clim2008[21])) #June 2008		
names(annclim2008) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")
						
annclim2009 <- stack(raster(clim2008[25]), raster(clim2008[26]), raster(clim2008[27]), #May 2008
						raster(clim2008[19]), raster(clim2008[20]), raster(clim2008[21]), #June 2008
						raster(clim2008[16]), raster(clim2008[17]), raster(clim2008[18]), #July 2008
						raster(clim2008[4]), raster(clim2008[5]), raster(clim2008[6]), #Aug 2008
						raster(clim2008[34]), raster(clim2008[35]), raster(clim2008[36]), #Sept 2008
						raster(clim2008[31]), raster(clim2008[32]), raster(clim2008[33]), #Oct 2008
						raster(clim2008[28]), raster(clim2008[29]), raster(clim2008[30]), #Nov 2008
						raster(clim2008[7]), raster(clim2008[8]), raster(clim2008[9]), #Dec 2008
						raster(clim2009[13]), raster(clim2009[14]), raster(clim2009[15]), #Jan 2009
						raster(clim2009[10]), raster(clim2009[11]), raster(clim2009[12]), #Feb 2009
						raster(clim2009[22]), raster(clim2009[23]), raster(clim2009[24]), #Mar 2009
						raster(clim2009[1]), raster(clim2009[2]), raster(clim2009[3]), #Apr 2009
						raster(clim2009[25]), raster(clim2009[26]), raster(clim2009[27]), #May 2009
						raster(clim2009[19]), raster(clim2009[20]), raster(clim2009[21])) #June 2009	
names(annclim2009) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")
						
annclim2010 <- stack(raster(clim2009[25]), raster(clim2009[26]), raster(clim2009[27]), #May 2009
						raster(clim2009[19]), raster(clim2009[20]), raster(clim2009[21]), #June 2009
						raster(clim2009[16]), raster(clim2009[17]), raster(clim2009[18]), #July 2009
						raster(clim2009[4]), raster(clim2009[5]), raster(clim2009[6]), #Aug 2009
						raster(clim2009[34]), raster(clim2009[35]), raster(clim2009[36]), #Sept 2009
						raster(clim2009[31]), raster(clim2009[32]), raster(clim2009[33]), #Oct 2009
						raster(clim2009[28]), raster(clim2009[29]), raster(clim2009[30]), #Nov 2009
						raster(clim2009[7]), raster(clim2009[8]), raster(clim2009[9])) #Dec 2009
annclim2010 <- crop(annclim2010, raster(clim2010[13]))		
annclim2010 <- addLayer(annclim2010,				
						raster(clim2010[13]), raster(clim2010[14]), raster(clim2010[15]), #Jan 2010
						raster(clim2010[10]), raster(clim2010[11]), raster(clim2010[12]), #Feb 2010
						raster(clim2010[22]), raster(clim2010[23]), raster(clim2010[24]), #Mar 2010
						raster(clim2010[1]), raster(clim2010[2]), raster(clim2010[3]), #Apr 2010
						raster(clim2010[25]), raster(clim2010[26]), raster(clim2010[27]), #May 2010
						raster(clim2010[19]), raster(clim2010[20]), raster(clim2010[21])) #June 2010	
names(annclim2010) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")

annclim2011 <- stack(raster(clim2010[25]), raster(clim2010[26]), raster(clim2010[27]), #May 2010
						raster(clim2010[19]), raster(clim2010[20]), raster(clim2010[21]), #June 2010
						raster(clim2010[16]), raster(clim2010[17]), raster(clim2010[18]), #July 2010
						raster(clim2010[4]), raster(clim2010[5]), raster(clim2010[6]), #Aug 2010
						raster(clim2010[34]), raster(clim2010[35]), raster(clim2010[36]), #Sept 2010
						raster(clim2010[31]), raster(clim2010[32]), raster(clim2010[33]), #Oct 2010
						raster(clim2010[28]), raster(clim2010[29]), raster(clim2010[30]), #Nov 2010
						raster(clim2010[7]), raster(clim2010[8]), raster(clim2010[9]), #Dec 2010
						raster(clim2011[13]), raster(clim2011[14]), raster(clim2011[15]), #Jan 2011
						raster(clim2011[10]), raster(clim2011[11]), raster(clim2011[12]), #Feb 2011
						raster(clim2011[22]), raster(clim2011[23]), raster(clim2011[24]), #Mar 2011
						raster(clim2011[1]), raster(clim2011[2]), raster(clim2011[3]), #Apr 2011
						raster(clim2011[25]), raster(clim2011[26]), raster(clim2011[27]), #May 2011
						raster(clim2011[19]), raster(clim2011[20]), raster(clim2011[21])) #June 2011	
names(annclim2011) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")
					
annclim2012 <- stack(raster(clim2011[25]), raster(clim2011[26]), raster(clim2011[27]), #May 2011
						raster(clim2011[19]), raster(clim2011[20]), raster(clim2011[21]), #June 2011
						raster(clim2011[16]), raster(clim2011[17]), raster(clim2011[18]), #July 2011
						raster(clim2011[4]), raster(clim2011[5]), raster(clim2011[6]), #Aug 2011
						raster(clim2011[34]), raster(clim2011[35]), raster(clim2011[36]), #Sept 2011
						raster(clim2011[31]), raster(clim2011[32]), raster(clim2011[33]), #Oct 2011
						raster(clim2011[28]), raster(clim2011[29]), raster(clim2011[30]), #Nov 2011
						raster(clim2011[7]), raster(clim2011[8]), raster(clim2011[9]), #Dec 2011
						raster(clim2012[13]), raster(clim2012[14]), raster(clim2012[15]), #Jan 2012
						raster(clim2012[10]), raster(clim2012[11]), raster(clim2012[12]), #Feb 2012
						raster(clim2012[22]), raster(clim2012[23]), raster(clim2012[24]), #Mar 2012
						raster(clim2012[1]), raster(clim2012[2]), raster(clim2012[3]), #Apr 2012
						raster(clim2012[25]), raster(clim2012[26]), raster(clim2012[27]), #May 2012
						raster(clim2012[19]), raster(clim2012[20]), raster(clim2012[21])) #June 2012	
names(annclim2012) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")
											
					
annclim2013 <- stack(raster(clim2012[25]), raster(clim2012[26]), raster(clim2012[27]), #May 2012
						raster(clim2012[19]), raster(clim2012[20]), raster(clim2012[21]), #June 2012
						raster(clim2012[16]), raster(clim2012[17]), raster(clim2012[18]), #July 2012
						raster(clim2012[4]), raster(clim2012[5]), raster(clim2012[6]), #Aug 2012
						raster(clim2012[34]), raster(clim2012[35]), raster(clim2012[36]), #Sept 2012
						raster(clim2012[31]), raster(clim2012[32]), raster(clim2012[33]), #Oct 2012
						raster(clim2012[28]), raster(clim2012[29]), raster(clim2012[30]), #Nov 2012
						raster(clim2012[7]), raster(clim2012[8]), raster(clim2012[9]), #Dec 2012
						raster(clim2013[13]), raster(clim2013[14]), raster(clim2013[15]), #Jan 2013
						raster(clim2013[10]), raster(clim2013[11]), raster(clim2013[12]), #Feb 2013
						raster(clim2013[22]), raster(clim2013[23]), raster(clim2013[24]), #Mar 2013
						raster(clim2013[1]), raster(clim2013[2]), raster(clim2013[3]), #Apr 2013
						raster(clim2013[25]), raster(clim2013[26]), raster(clim2013[27]), #May 2013
						raster(clim2013[19]), raster(clim2013[20]), raster(clim2013[21])) #June 2013	
names(annclim2013) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")
																
						
setwd(curSA)
annclim2000sa <- stack(raster(saclim1999[25]), raster(saclim1999[26]), raster(saclim1999[27]), #May 1999
						raster(saclim1999[19]), raster(saclim1999[20]), raster(saclim1999[21]), #June 1999
						raster(saclim1999[16]), raster(saclim1999[17]), raster(saclim1999[18]), #July 1999
						raster(saclim1999[4]), raster(saclim1999[5]), raster(saclim1999[6]), #Aug 1999
						raster(saclim1999[34]), raster(saclim1999[35]), raster(saclim1999[36]), #Sept 1999
						raster(saclim1999[31]), raster(saclim1999[32]), raster(saclim1999[33]), #Oct 1999
						raster(saclim1999[28]), raster(saclim1999[29]), raster(saclim1999[30]), #Nov 1999
						raster(saclim1999[7]), raster(saclim1999[8]), raster(saclim1999[9]), #Dec 1999
						raster(saclim2000[13]), raster(saclim2000[14]), raster(saclim2000[15]), #Jan 2000
						raster(saclim2000[10]), raster(saclim2000[11]), raster(saclim2000[12]), #Feb 2000
						raster(saclim2000[22]), raster(saclim2000[23]), raster(saclim2000[24]), #Mar 2000
						raster(saclim2000[1]), raster(saclim2000[2]), raster(saclim2000[3]), #Apr 2000
						raster(saclim2000[25]), raster(saclim2000[26]), raster(saclim2000[27]), #May 2000
						raster(saclim2000[19]), raster(saclim2000[20]), raster(saclim2000[21])) #June 2000	
names(annclim2000sa) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")
	
annclim2001sa <- stack(raster(saclim2000[25]), raster(saclim2000[26]), raster(saclim2000[27]), #May 2000
						raster(saclim2000[19]), raster(saclim2000[20]), raster(saclim2000[21]), #June 2000
						raster(saclim2000[16]), raster(saclim2000[17]), raster(saclim2000[18]), #July 2000
						raster(saclim2000[4]), raster(saclim2000[5]), raster(saclim2000[6]), #Aug 2000
						raster(saclim2000[34]), raster(saclim2000[35]), raster(saclim2000[36]), #Sept 2000
						raster(saclim2000[31]), raster(saclim2000[32]), raster(saclim2000[33]), #Oct 2000
						raster(saclim2000[28]), raster(saclim2000[29]), raster(saclim2000[30]), #Nov 2000
						raster(saclim2000[7]), raster(saclim2000[8]), raster(saclim2000[9]), #Dec 2000
						raster(saclim2001[13]), raster(saclim2001[14]), raster(saclim2001[15]), #Jan 2001
						raster(saclim2001[10]), raster(saclim2001[11]), raster(saclim2001[12]), #Feb 2001
						raster(saclim2001[22]), raster(saclim2001[23]), raster(saclim2001[24]), #Mar 2001
						raster(saclim2001[1]), raster(saclim2001[2]), raster(saclim2001[3]), #Apr 2001
						raster(saclim2001[25]), raster(saclim2001[26]), raster(saclim2001[27]), #May 2001
						raster(saclim2001[19]), raster(saclim2001[20]), raster(saclim2001[21])) #June 2001	
names(annclim2001sa) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")
	
annclim2002sa <- stack(raster(saclim2001[25]), raster(saclim2001[26]), raster(saclim2001[27]), #May 2001
						raster(saclim2001[19]), raster(saclim2001[20]), raster(saclim2001[21]), #June 2001
						raster(saclim2001[16]), raster(saclim2001[17]), raster(saclim2001[18]), #July 2001
						raster(saclim2001[4]), raster(saclim2001[5]), raster(saclim2001[6]), #Aug 2001
						raster(saclim2001[34]), raster(saclim2001[35]), raster(saclim2001[36]), #Sept 2001
						raster(saclim2001[31]), raster(saclim2001[32]), raster(saclim2001[33]), #Oct 2001
						raster(saclim2001[28]), raster(saclim2001[29]), raster(saclim2001[30]), #Nov 2001
						raster(saclim2001[7]), raster(saclim2001[8]), raster(saclim2001[9]), #Dec 2001
						raster(saclim2002[13]), raster(saclim2002[14]), raster(saclim2002[15]), #Jan 2002
						raster(saclim2002[10]), raster(saclim2002[11]), raster(saclim2002[12]), #Feb 2002
						raster(saclim2002[22]), raster(saclim2002[23]), raster(saclim2002[24]), #Mar 2002
						raster(saclim2002[1]), raster(saclim2002[2]), raster(saclim2002[3]), #Apr 2002
						raster(saclim2002[25]), raster(saclim2002[26]), raster(saclim2002[27]), #May 2002
						raster(saclim2002[19]), raster(saclim2002[20]), raster(saclim2002[21])) #June 2002	
names(annclim2002sa) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")
	
annclim2003sa <- stack(raster(saclim2002[25]), raster(saclim2002[26]), raster(saclim2002[27]), #May 2002
						raster(saclim2002[19]), raster(saclim2002[20]), raster(saclim2002[21]), #June 2002
						raster(saclim2002[16]), raster(saclim2002[17]), raster(saclim2002[18]), #July 2002
						raster(saclim2002[4]), raster(saclim2002[5]), raster(saclim2002[6]), #Aug 2002
						raster(saclim2002[34]), raster(saclim2002[35]), raster(saclim2002[36]), #Sept 2002
						raster(saclim2002[31]), raster(saclim2002[32]), raster(saclim2002[33]), #Oct 2002
						raster(saclim2002[28]), raster(saclim2002[29]), raster(saclim2002[30]), #Nov 2002
						raster(saclim2002[7]), raster(saclim2002[8]), raster(saclim2002[9]), #Dec 2002
						raster(saclim2003[13]), raster(saclim2003[14]), raster(saclim2003[15]), #Jan 2003
						raster(saclim2003[10]), raster(saclim2003[11]), raster(saclim2003[12]), #Feb 2003
						raster(saclim2003[22]), raster(saclim2003[23]), raster(saclim2003[24]), #Mar 2003
						raster(saclim2003[1]), raster(saclim2003[2]), raster(saclim2003[3]), #Apr 2003
						raster(saclim2003[25]), raster(saclim2003[26]), raster(saclim2003[27]), #May 2003
						raster(saclim2003[19]), raster(saclim2003[20]), raster(saclim2003[21])) #June 2003	
names(annclim2003sa) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")
	
annclim2004sa <- stack(raster(saclim2003[25]), raster(saclim2003[26]), raster(saclim2003[27]), #May 2003
						raster(saclim2003[19]), raster(saclim2003[20]), raster(saclim2003[21]), #June 2003
						raster(saclim2003[16]), raster(saclim2003[17]), raster(saclim2003[18]), #July 2003
						raster(saclim2003[4]), raster(saclim2003[5]), raster(saclim2003[6]), #Aug 2003
						raster(saclim2003[34]), raster(saclim2003[35]), raster(saclim2003[36]), #Sept 2003
						raster(saclim2003[31]), raster(saclim2003[32]), raster(saclim2003[33]), #Oct 2003
						raster(saclim2003[28]), raster(saclim2003[29]), raster(saclim2003[30]), #Nov 2003
						raster(saclim2003[7]), raster(saclim2003[8]), raster(saclim2003[9]), #Dec 2003
						raster(saclim2004[13]), raster(saclim2004[14]), raster(saclim2004[15]), #Jan 2004
						raster(saclim2004[10]), raster(saclim2004[11]), raster(saclim2004[12]), #Feb 2004
						raster(saclim2004[22]), raster(saclim2004[23]), raster(saclim2004[24]), #Mar 2004
						raster(saclim2004[1]), raster(saclim2004[2]), raster(saclim2004[3]), #Apr 2004
						raster(saclim2004[25]), raster(saclim2004[26]), raster(saclim2004[27]), #May 2004
						raster(saclim2004[19]), raster(saclim2004[20]), raster(saclim2004[21])) #June 2004	
names(annclim2004sa) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")
	
annclim2005sa <- stack(raster(saclim2004[25]), raster(saclim2004[26]), raster(saclim2004[27]), #May 2004
						raster(saclim2004[19]), raster(saclim2004[20]), raster(saclim2004[21]), #June 2004
						raster(saclim2004[16]), raster(saclim2004[17]), raster(saclim2004[18]), #July 2004
						raster(saclim2004[4]), raster(saclim2004[5]), raster(saclim2004[6]), #Aug 2004
						raster(saclim2004[34]), raster(saclim2004[35]), raster(saclim2004[36]), #Sept 2004
						raster(saclim2004[31]), raster(saclim2004[32]), raster(saclim2004[33]), #Oct 2004
						raster(saclim2004[28]), raster(saclim2004[29]), raster(saclim2004[30]), #Nov 2004
						raster(saclim2004[7]), raster(saclim2004[8]), raster(saclim2004[9]), #Dec 2004
						raster(saclim2005[13]), raster(saclim2005[14]), raster(saclim2005[15]), #Jan 2005
						raster(saclim2005[10]), raster(saclim2005[11]), raster(saclim2005[12]), #Feb 2005
						raster(saclim2005[22]), raster(saclim2005[23]), raster(saclim2005[24]), #Mar 2005
						raster(saclim2005[1]), raster(saclim2005[2]), raster(saclim2005[3]), #Apr 2005
						raster(saclim2005[25]), raster(saclim2005[26]), raster(saclim2005[27]), #May 2005
						raster(saclim2005[19]), raster(saclim2005[20]), raster(saclim2005[21])) #June 2005	
names(annclim2005sa) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")
	
annclim2006sa <- stack(raster(saclim2005[25]), raster(saclim2005[26]), raster(saclim2005[27]), #May 2005
						raster(saclim2005[19]), raster(saclim2005[20]), raster(saclim2005[21]), #June 2005
						raster(saclim2005[16]), raster(saclim2005[17]), raster(saclim2005[18]), #July 2005
						raster(saclim2005[4]), raster(saclim2005[5]), raster(saclim2005[6]), #Aug 2005
						raster(saclim2005[34]), raster(saclim2005[35]), raster(saclim2005[36]), #Sept 2005
						raster(saclim2005[31]), raster(saclim2005[32]), raster(saclim2005[33]), #Oct 2005
						raster(saclim2005[28]), raster(saclim2005[29]), raster(saclim2005[30]), #Nov 2005
						raster(saclim2005[7]), raster(saclim2005[8]), raster(saclim2005[9]), #Dec 2005
						raster(saclim2006[13]), raster(saclim2006[14]), raster(saclim2006[15]), #Jan 2006
						raster(saclim2006[10]), raster(saclim2006[11]), raster(saclim2006[12]), #Feb 2006
						raster(saclim2006[22]), raster(saclim2006[23]), raster(saclim2006[24]), #Mar 2006
						raster(saclim2006[1]), raster(saclim2006[2]), raster(saclim2006[3]), #Apr 2006
						raster(saclim2006[25]), raster(saclim2006[26]), raster(saclim2006[27]), #May 2006
						raster(saclim2006[19]), raster(saclim2006[20]), raster(saclim2006[21])) #June 2006	
names(annclim2006sa) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")
						


annclim2007sa <- stack(raster(saclim2006[25]), raster(saclim2006[26]), raster(saclim2006[27]), #May 2006
						raster(saclim2006[19]), raster(saclim2006[20]), raster(saclim2006[21]), #June 2006
						raster(saclim2006[16]), raster(saclim2006[17]), raster(saclim2006[18]), #July 2006
						raster(saclim2006[4]), raster(saclim2006[5]), raster(saclim2006[6]), #Aug 2006
						raster(saclim2006[34]), raster(saclim2006[35]), raster(saclim2006[36]), #Sept 2006
						raster(saclim2006[31]), raster(saclim2006[32]), raster(saclim2006[33]), #Oct 2006
						raster(saclim2006[28]), raster(saclim2006[29]), raster(saclim2006[30]), #Nov 2006
						raster(saclim2006[7]), raster(saclim2006[8]), raster(saclim2006[9]), #Dec 2006
						raster(saclim2007[13]), raster(saclim2007[14]), raster(saclim2007[15]), #Jan 2007
						raster(saclim2007[10]), raster(saclim2007[11]), raster(saclim2007[12]), #Feb 2007
						raster(saclim2007[22]), raster(saclim2007[23]), raster(saclim2007[24]), #Mar 2007
						raster(saclim2007[1]), raster(saclim2007[2]), raster(saclim2007[3]), #Apr 2007
						raster(saclim2007[25]), raster(saclim2007[26]), raster(saclim2007[27]), #May 2007
						raster(saclim2007[19]), raster(saclim2007[20]), raster(saclim2007[21])) #June 2007	
names(annclim2007sa) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")
						

annclim2008sa <- stack(raster(saclim2007[25]), raster(saclim2007[26]), raster(saclim2007[27]), #May 2007
						raster(saclim2007[19]), raster(saclim2007[20]), raster(saclim2007[21]), #June 2007
						raster(saclim2007[16]), raster(saclim2007[17]), raster(saclim2007[18]), #July 2007
						raster(saclim2007[4]), raster(saclim2007[5]), raster(saclim2007[6]), #Aug 2007
						raster(saclim2007[34]), raster(saclim2007[35]), raster(saclim2007[36]), #Sept 2007
						raster(saclim2007[31]), raster(saclim2007[32]), raster(saclim2007[33]), #Oct 2007
						raster(saclim2007[28]), raster(saclim2007[29]), raster(saclim2007[30]), #Nov 2007
						raster(saclim2007[7]), raster(saclim2007[8]), raster(saclim2007[9]), #Dec 2007
						raster(saclim2008[13]), raster(saclim2008[14]), raster(saclim2008[15]), #Jan 2008
						raster(saclim2008[10]), raster(saclim2008[11]), raster(saclim2008[12]), #Feb 2008
						raster(saclim2008[22]), raster(saclim2008[23]), raster(saclim2008[24]), #Mar 2008
						raster(saclim2008[1]), raster(saclim2008[2]), raster(saclim2008[3]), #Apr 2008
						raster(saclim2008[25]), raster(saclim2008[26]), raster(saclim2008[27]), #May 2008
						raster(saclim2008[19]), raster(saclim2008[20]), raster(saclim2008[21])) #June 2008	
names(annclim2008sa) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")
						
annclim2009sa <- stack(raster(saclim2008[25]), raster(saclim2008[26]), raster(saclim2008[27]), #May 2008
						raster(saclim2008[19]), raster(saclim2008[20]), raster(saclim2008[21]), #June 2008
						raster(saclim2008[16]), raster(saclim2008[17]), raster(saclim2008[18]), #July 2008
						raster(saclim2008[4]), raster(saclim2008[5]), raster(saclim2008[6]), #Aug 2008
						raster(saclim2008[34]), raster(saclim2008[35]), raster(saclim2008[36]), #Sept 2008
						raster(saclim2008[31]), raster(saclim2008[32]), raster(saclim2008[33]), #Oct 2008
						raster(saclim2008[28]), raster(saclim2008[29]), raster(saclim2008[30]), #Nov 2008
						raster(saclim2008[7]), raster(saclim2008[8]), raster(saclim2008[9]), #Dec 2008
						raster(saclim2009[13]), raster(saclim2009[14]), raster(saclim2009[15]), #Jan 2009
						raster(saclim2009[10]), raster(saclim2009[11]), raster(saclim2009[12]), #Feb 2009
						raster(saclim2009[22]), raster(saclim2009[23]), raster(saclim2009[24]), #Mar 2009
						raster(saclim2009[1]), raster(saclim2009[2]), raster(saclim2009[3]), #Apr 2009
						raster(saclim2009[25]), raster(saclim2009[26]), raster(saclim2009[27]), #May 2009
						raster(saclim2009[19]), raster(saclim2009[20]), raster(saclim2009[21])) #June 2009	
names(annclim2009sa) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")
				
annclim2010sa <- stack(raster(saclim2009[25]), raster(saclim2009[26]), raster(saclim2009[27]), #May 2009
						raster(saclim2009[19]), raster(saclim2009[20]), raster(saclim2009[21]), #June 2009
						raster(saclim2009[16]), raster(saclim2009[17]), raster(saclim2009[18]), #July 2009
						raster(saclim2009[4]), raster(saclim2009[5]), raster(saclim2009[6]), #Aug 2009
						raster(saclim2009[34]), raster(saclim2009[35]), raster(saclim2009[36]), #Sept 2009
						raster(saclim2009[31]), raster(saclim2009[32]), raster(saclim2009[33]), #Oct 2009
						raster(saclim2009[28]), raster(saclim2009[29]), raster(saclim2009[30]), #Nov 2009
						raster(saclim2009[7]), raster(saclim2009[8]), raster(saclim2009[9]), #Dec 2009
						raster(saclim2010[13]), raster(saclim2010[14]), raster(saclim2010[15]), #Jan 2010
						raster(saclim2010[10]), raster(saclim2010[11]), raster(saclim2010[12]), #Feb 2010
						raster(saclim2010[22]), raster(saclim2010[23]), raster(saclim2010[24]), #Mar 2010
						raster(saclim2010[1]), raster(saclim2010[2]), raster(saclim2010[3]), #Apr 2010
						raster(saclim2010[25]), raster(saclim2010[26]), raster(saclim2010[27]), #May 2010
						raster(saclim2010[19]), raster(saclim2010[20]), raster(saclim2010[21])) #June 2010	
names(annclim2010sa) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")
						
annclim2011sa <- stack(raster(saclim2010[25]), raster(saclim2010[26]), raster(saclim2010[27]), #May 2010
						raster(saclim2010[19]), raster(saclim2010[20]), raster(saclim2010[21]), #June 2010
						raster(saclim2010[16]), raster(saclim2010[17]), raster(saclim2010[18]), #July 2010
						raster(saclim2010[4]), raster(saclim2010[5]), raster(saclim2010[6]), #Aug 2010
						raster(saclim2010[34]), raster(saclim2010[35]), raster(saclim2010[36]), #Sept 2010
						raster(saclim2010[31]), raster(saclim2010[32]), raster(saclim2010[33]), #Oct 2010
						raster(saclim2010[28]), raster(saclim2010[29]), raster(saclim2010[30]), #Nov 2010
						raster(saclim2010[7]), raster(saclim2010[8]), raster(saclim2010[9]), #Dec 2010
						raster(saclim2011[13]), raster(saclim2011[14]), raster(saclim2011[15]), #Jan 2011
						raster(saclim2011[10]), raster(saclim2011[11]), raster(saclim2011[12]), #Feb 2011
						raster(saclim2011[22]), raster(saclim2011[23]), raster(saclim2011[24]), #Mar 2011
						raster(saclim2011[1]), raster(saclim2011[2]), raster(saclim2011[3]), #Apr 2011
						raster(saclim2011[25]), raster(saclim2011[26]), raster(saclim2011[27]), #May 2011
						raster(saclim2011[19]), raster(saclim2011[20]), raster(saclim2011[21])) #June 2011	
names(annclim2011sa) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")						
						
annclim2012sa <- stack(raster(saclim2012[25]), raster(saclim2012[26]), raster(saclim2012[27]), #May 2012
						raster(saclim2012[19]), raster(saclim2012[20]), raster(saclim2012[21]), #June 2012
						raster(saclim2012[16]), raster(saclim2012[17]), raster(saclim2012[18]), #July 2012
						raster(saclim2012[4]), raster(saclim2012[5]), raster(saclim2012[6]), #Aug 2012
						raster(saclim2012[34]), raster(saclim2012[35]), raster(saclim2012[36]), #Sept 2012
						raster(saclim2012[31]), raster(saclim2012[32]), raster(saclim2012[33]), #Oct 2012
						raster(saclim2012[28]), raster(saclim2012[29]), raster(saclim2012[30]), #Nov 2012
						raster(saclim2012[7]), raster(saclim2012[8]), raster(saclim2012[9]), #Dec 2012
						raster(saclim2012[13]), raster(saclim2012[14]), raster(saclim2012[15]), #Jan 2012
						raster(saclim2012[10]), raster(saclim2012[11]), raster(saclim2012[12]), #Feb 2012
						raster(saclim2012[22]), raster(saclim2012[23]), raster(saclim2012[24]), #Mar 2012
						raster(saclim2012[1]), raster(saclim2012[2]), raster(saclim2012[3]), #Apr 2012
						raster(saclim2012[25]), raster(saclim2012[26]), raster(saclim2012[27]), #May 2012
						raster(saclim2012[19]), raster(saclim2012[20]), raster(saclim2012[21])) #June 2012	
names(annclim2012sa) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")
					
annclim2013sa <- stack(raster(saclim2012[25]), raster(saclim2012[26]), raster(saclim2012[27]), #May 2012
						raster(saclim2012[19]), raster(saclim2012[20]), raster(saclim2012[21]), #June 2012
						raster(saclim2012[16]), raster(saclim2012[17]), raster(saclim2012[18]), #July 2012
						raster(saclim2012[4]), raster(saclim2012[5]), raster(saclim2012[6]), #Aug 2012
						raster(saclim2012[34]), raster(saclim2012[35]), raster(saclim2012[36]), #Sept 2012
						raster(saclim2012[31]), raster(saclim2012[32]), raster(saclim2012[33]), #Oct 2012
						raster(saclim2012[28]), raster(saclim2012[29]), raster(saclim2012[30]), #Nov 2012
						raster(saclim2012[7]), raster(saclim2012[8]), raster(saclim2012[9]), #Dec 2012
						raster(saclim2013[13]), raster(saclim2013[14]), raster(saclim2013[15]), #Jan 2013
						raster(saclim2013[10]), raster(saclim2013[11]), raster(saclim2013[12]), #Feb 2013
						raster(saclim2013[22]), raster(saclim2013[23]), raster(saclim2013[24]), #Mar 2013
						raster(saclim2013[1]), raster(saclim2013[2]), raster(saclim2013[3]), #Apr 2013
						raster(saclim2013[25]), raster(saclim2013[26]), raster(saclim2013[27]), #May 2013
						raster(saclim2013[19]), raster(saclim2013[20]), raster(saclim2013[21])) #June 2013	
names(annclim2013sa) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")

merge2000 <- stack(merge(annclim2000sa[[1]],annclim2000[[1]]))
for (i in 2:nlayers(annclim2000sa)) {
	x <- merge(annclim2000sa[[i]],annclim2000[[i]])
	merge2000 <- addLayer(merge2000,x)
	}	
names(merge2000) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")							

						merge2001 <- stack(merge(annclim2001sa[[1]],annclim2001[[1]]))
for (i in 2:nlayers(annclim2001sa)) {
	x <- merge(annclim2001sa[[i]],annclim2001[[i]])
	merge2001 <- addLayer(merge2001,x)
	}
names(merge2001) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")
		
merge2002 <- stack(merge(annclim2002sa[[1]],annclim2002[[1]]))
for (i in 2:nlayers(annclim2002sa)) {
	x <- merge(annclim2002sa[[i]],annclim2002[[i]])
	merge2002 <- addLayer(merge2002,x)
	}	
names(merge2002) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")
	
merge2003 <- stack(merge(annclim2003sa[[1]],annclim2003[[1]]))
for (i in 2:nlayers(annclim2003sa)) {
	x <- merge(annclim2003sa[[i]],annclim2003[[i]])
	merge2003 <- addLayer(merge2003,x)
	}	
names(merge2003) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")

merge2004 <- stack(merge(annclim2004sa[[1]],annclim2004[[1]]))
for (i in 2:nlayers(annclim2004sa)) {
	x <- merge(annclim2004sa[[i]],annclim2004[[i]])
	merge2004 <- addLayer(merge2004,x)
	}	
names(merge2004) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")

merge2005 <- stack(merge(annclim2005sa[[1]],annclim2005[[1]]))
for (i in 2:nlayers(annclim2005sa)) {
	x <- merge(annclim2005sa[[i]],annclim2005[[i]])
	merge2005 <- addLayer(merge2005,x)
	}	
names(merge2005) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")

merge2006 <- stack(merge(annclim2006sa[[1]],annclim2006[[1]]))
for (i in 2:nlayers(annclim2006sa)) {
	x <- merge(annclim2006sa[[i]],annclim2006[[i]])
	merge2006 <- addLayer(merge2006,x)
	}	
names(merge2006) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")
				
merge2007 <- stack(merge(annclim2007sa[[1]],annclim2007[[1]]))
for (i in 2:nlayers(annclim2007sa)) {
	x <- merge(annclim2007sa[[i]],annclim2007[[i]])
	merge2007 <- addLayer(merge2007,x)
	}	
names(merge2007) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")
											
merge2008 <- stack(merge(annclim2008sa[[1]],annclim2008[[1]]))
for (i in 2:nlayers(annclim2008sa)) {
	x <- merge(annclim2008sa[[i]],annclim2008[[i]])
	merge2008 <- addLayer(merge2008,x)
	}	
names(merge2008) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")
											
merge2009 <- stack(merge(annclim2009sa[[1]],annclim2009[[1]]))
for (i in 2:nlayers(annclim2009sa)) {
	x <- merge(annclim2009sa[[i]],annclim2009[[i]])
	merge2009 <- addLayer(merge2009,x)
	}
names(merge2009) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")	

merge2010 <- stack(merge(annclim2010sa[[1]],annclim2010[[1]]))
for (i in 2:nlayers(annclim2010sa)) {
	x <- merge(annclim2010sa[[i]],annclim2010[[i]])
	merge2010 <- addLayer(merge2010,x)
	}
names(merge2010) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")	

merge2011 <- stack(merge(annclim2011sa[[1]],annclim2011[[1]]))
for (i in 2:nlayers(annclim2011sa)) {
	x <- merge(annclim2011sa[[i]],annclim2011[[i]])
	merge2011 <- addLayer(merge2011,x)
	}
names(merge2011) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")	


merge2012 <- stack(merge(annclim2012sa[[1]],annclim2012[[1]]))
for (i in 2:nlayers(annclim2012sa)) {
	x <- merge(annclim2012sa[[i]],annclim2012[[i]])
	merge2012 <- addLayer(merge2012,x)
	}
names(merge2012) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")	


merge2013 <- stack(merge(annclim2013sa[[1]],annclim2013[[1]]))
for (i in 2:nlayers(annclim2013sa)) {
	x <- merge(annclim2013sa[[i]],annclim2013[[i]])
	merge2013 <- addLayer(merge2013,x)
	}
names(merge2013) <- c("maypre0", "maytmax0", "maytmin0", "junpre0", "juntmax0", "juntmin0", "julpre0", "jultmax0", "jultmin0", "augpre0", "augtmax0", "augtmin0", "seppre0", "septmax0", "septmin0", "octpre0", "octtmax0", "octtmin0",
						"novpre0", "novtmax0", "novtmin0", "decpre0", "dectmax0", "dectmin0", "janpre1", "jantmax1", "jantmin1", "febpre1", "febtmax1", "febtmin1", 
						"marpre1", "martmax1", "martmin1", "aprpre1", "aprtmax1", "aprtmin1", "maypre1", "maytmax1", "maytmin1", "junpre1", "juntmax1", "juntmin1")	

writeRaster(merge2000, filename="G:/Boreal/InterannualVariability/annclim2000.grd",overwrite=TRUE)
writeRaster(merge2001, filename="G:/Boreal/InterannualVariability/annclim2001.grd",overwrite=TRUE)	
writeRaster(merge2002, filename="G:/Boreal/InterannualVariability/annclim2002.grd",overwrite=TRUE)
writeRaster(merge2003, filename="G:/Boreal/InterannualVariability/annclim2003.grd",overwrite=TRUE)
writeRaster(merge2004, filename="G:/Boreal/InterannualVariability/annclim2004.grd",overwrite=TRUE)
writeRaster(merge2005, filename="G:/Boreal/InterannualVariability/annclim2005.grd",overwrite=TRUE)
writeRaster(merge2006, filename="G:/Boreal/InterannualVariability/annclim2006.grd",overwrite=TRUE)
writeRaster(merge2007, filename="G:/Boreal/InterannualVariability/annclim2007.grd",overwrite=TRUE)
writeRaster(merge2008, filename="G:/Boreal/InterannualVariability/annclim2008.grd",overwrite=TRUE)
writeRaster(merge2009, filename="G:/Boreal/InterannualVariability/annclim2009.grd",overwrite=TRUE)
writeRaster(merge2010, filename="G:/Boreal/InterannualVariability/annclim2010.grd",overwrite=TRUE)
writeRaster(merge2011, filename="G:/Boreal/InterannualVariability/annclim2011.grd",overwrite=TRUE)
writeRaster(merge2012, filename="G:/Boreal/InterannualVariability/annclim2012.grd",overwrite=TRUE)
writeRaster(merge2013, filename="G:/Boreal/InterannualVariability/annclim2013.grd",overwrite=TRUE)
