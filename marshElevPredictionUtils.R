# TODO: Add comment
# 
# Author: lsalas
# Edited by: djongsomjit
###############################################################################


# mstk is a stak with the following *named* layers: elev, Organic, SSC, where elevation is in m
# slr is a value > 0 that indicates the amount of SLR in 100 years, in meters
# timeSpan is how long you want this function to project into the future
# byOut indicates how often you want an output prediction (see "values out" below)
# c is the adjustment parameter that causes the best fit of the brt model - see testCreateAccretionFunction.R for details
# accLog = TRUE indicates that the brt predicts on the log scale.
# NOTE: the current brt model uses the default values c=1.6 and accLog=TRUE

#Output: this function generates a stack with the current marsh elevation and a layer with the elevation of each year in timeSpan of interval byOut
library(gbm)
library(raster)

#directory with rasters and utilM98moels  currently saved here locally V:\Project\climate_change\models\sfbay\san_mateo_wetlands\test
mydir<-"YOUR DIRECTORY"

#create raster stack. These are converted to meter (divided by 10) to match Leo's code.
elev<-raster(paste0(mydir,"/elevmhhw.tif"))
SSC<-raster(paste0(mydir,"/sed150.tif"))
Organic<-raster(paste0(mydir,"/org3.tif"))

mystack<-stack(elev,SSC,Organic)

names(mystack)<-c("elev","SSC","Organic")


load(file=paste0(mydir,"/utilM98models.RData"))

selRasVal<-function(e,b,m){
	x<-e;x[]<-ifelse(x[] >= 0, 0, 
			ifelse(x[] <= -2.39,m[],b[]))
	return(x)
}

getSLRdiff<-function(mslra,mslrb,slr,timeSpan=100){
	#calculate the slr curve
	aval<-predict(mslra,data.frame(slr=slr))
	bval<-predict(mslrb,data.frame(slr=slr))
	slrdata<-data.frame(year=c(0:timeSpan));slrdata$SLR<-(slrdata$year*aval) + ((slrdata$year^2)*bval)
	slrdata$incSLR<-c(0,unlist(lapply(2:nrow(slrdata),FUN=function(x,slrdata){sv<-slrdata[x,"SLR"]-slrdata[(x-1),"SLR"];return(sv)},slrdata)))
	return(slrdata)
}

makeElevRasters<-function(mstk,slr=1,timeSpan=100,byOut=10,c=1.6,accLog=TRUE){
	#get the slrDiff data
	#adding a comment is useful
	slrdata<-getSLRdiff(mslra=mslra,mslrb=mslrb,slr=slr,timeSpan=timeSpan)
	#get output years
	outyears<-seq(0,timeSpan,by=byOut);outyears<-outyears[-1]
	
	#loop from 1 to timeSpan
	resstack<-mstk$elev
	curstk<-mstk
	for(yy in 1:timeSpan){
		slrval<-subset(slrdata,year==yy)$incSLR
		curstk$elevc<-(curstk$elev-c)*curstk$SSC
		pbrt<-predict(curstk,brtmdl)
		if(accLog==TRUE)pbrt<-exp(pbrt)
		pm0=predict(curstk,mdl0)
		tras<-stack(curstk$elev,pbrt,pm0);names(tras)<-c("elev","pbrt","pm0")
		acctime<-selRasVal(e=tras$elev,b=tras$pbrt,m=tras$pm0)
		elevtime<-curstk$elev+(acctime/1000)-slrval
		if(yy %in% outyears){
			resstack<-stack(resstack,elevtime)
		}
		curstk$elev<-elevtime
	}
	names(resstack)<-c("Year0",paste("Year",outyears,sep=""))
	return(resstack)
}



ptm <- proc.time()
#set parameters here
test<-makeElevRasters(mstk=mystack,slr=1.65,byOut=100,timeSpan=100)
print("total time")
 proc.time() - ptm
 
 #write file as needed. Here x10 to convert back to m*10 
 #writeRaster((10*test[[2]]),file="V:/Project/climate_change/models/sfbay/san_mateo_wetlands/test/test100y165m.tif.", format="GTiff",overwrite=TRUE)


#################### Test - not run?
#make the stack input
base <- raster(nrows=10, ncols=10)
elev<-base;elev[]<-rnorm(100,-1,0.4)
SSC<-base; SSC[]<-100
Organic<-base;Organic[]<-2
tststk<-stack(elev,SSC,Organic);names(tststk)<-c("elev","SSC","Organic")

#run the function
test<-makeElevRasters(mstk=tststk,byOut=10)

###############################################################
#are these values correct?
#take the starting value from the file Proof of SLR calcs.xlsx and insert it into one of the cells of the raster
tststk[10] <- -0.512428341333333
test<-makeElevRasters(mstk=tststk,slr=1.658,byOut=1)	#note that I am requesting outputs by year!
rdata<-data.frame(testRaster=as.numeric(test[10]),year=c(0:100))

#compare to the data in the proofing file:
library(XLConnect)
proof<-readWorksheetFromFile("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/SFBayProgram/ESAtables/Proof of SLR calcs.xlsx",sheet="testData")
pdf<-merge(rdata,proof,by="year")

library(ggplot2)
p<-ggplot(pdf,aes(x=Elevm,y=testRaster)) + geom_point()


