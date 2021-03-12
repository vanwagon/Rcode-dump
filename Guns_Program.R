# Empty dataset: Always a good idea
rm(list=ls())


# Libraries need to be installed first.....!!!

install.packages("gdata")
install.packages( "AER" )
install.packages( "car")
install.packages( "lmtest" )
install.packages( "zoo"  )
install.packages("stargazer")

# And, even after they are installed, they need to be called every time before they are used

library(stargazer)
library("gdata")
library("AER")
library("car")
library("lmtest")
library("zoo")

regpols<-lm(vio~shall,data=mydata)
regfes<-lm(vio~shall+factor(stateid),data=mydata)
regfesy<-lm(vio~shall+factor(year)+factor(stateid),data=mydata)
regfesyt<-lm(vio~shall+factor(year)+factor(stateid)+factor(stateid):year,data=mydata)
regfesytmulti<-lm(vio~shall+density+avginc+incarc_rate+pm1029+factor(year)+factor(stateid),data=mydata)
compareCoefs(regpols,regfes,digits=4)
compareCoefs(regfesy,regfesyt,digits=4)
compareCoefs(regfesytmulti,digits=4)

###################################################
####Now using plm package instead of lm #######

install.packages("Formula")
install.packages("plm")
library(plm)
library(Formula)
mydatap<-pdata.frame(mydata,c("stateid","year"))
pregpols<-plm(vio~shall,model="pooling",data=mydatap) #column 1
rvpols<-vcovHC(pregpols,method="arellano",cluster="group")
coeftest(pregpols,vcov=rvpols)
pregfes<-plm(vio~shall,model="within",data=mydatap)
rvfes<-vcovHC(pregfes,method="arellano",cluster="group")
coeftest(pregfes,vcov=rvfes)
pregfesy<-plm(vio~shall,effect="twoway",model="within",data=mydatap)
rvfesy<-vcovHC(pregfesy,method="arellano",cluster="group")
coeftest(pregfesy,vcov=rvfesy)

pregfesymulti<-plm(vio~shall+density+avginc+incarc_rate+pm1029,effect="twoway",model="within",data=mydatap)
rvfesymulti<-vcovHC(pregfesymulti,method="arellano",cluster="group")
coeftest(pregfesymulti,vcov=rvfesymulti)
