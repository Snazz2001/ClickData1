require(plyr)
library(lattice)
setwd("C:/My Projects/Zheng - Click Data/Data/")
setwd("C:/My Projects/Zheng - Click Data/Data/click0705")

#load the data in to R
ad04<-read.csv("apply-details02-04.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
colnames(ad04)
head(ad04)
ad08<-read.csv("apply-details04-08.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
colnames(ad08)
ad25<-read.csv("apply-details08-25.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
colnames(ad25)
ad27<-read.csv("apply-details24-27.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
colnames(ad27)
ad30<-read.csv("apply-details28-30.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
colnames(ad30)
ad<-rbind(ad04,ad08,ad25,ad27,ad30)
#load performance data
l0<-read.csv("l0appsdata.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
ln<-read.csv("lnappsdata.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
#check the overlap due to the SQL query not well separate the applications
intersect(l0$ApplicationNKey,ad04$applicationID)
intersect(ad25$applicationID,ad27$applicationID)

#merge data together, adUnique is all apply details
ad2<-rbind(ad25,ad27)
ad2Unique<-unique(ad2)

ad<-rbind(ad04,ad08,ad25,ad27,ad30)
dim(ad)
intersect(ad$applicationID,ln$ApplicationNKey)
adUnique<-unique(ad)#although it is unique, there will be many case associated with one applicationID
dim(adUnique)

adecl0<-merge(adUnique,l0,by.x='applicationID',by.y='ApplicationNKey')#multiple feature for one app will have multiple same decision information
dim(adecl0)
adecln<-merge(adUnique,ln,by.x='applicationID',by.y='ApplicationNKey')#strange??
dim(adecln)

click25<-read.table("click06.08-25.txt",header=TRUE,sep="\t",na.strings="NA",stringsAsFactors=FALSE);

#look at the l0 apps distribution 
require(plyr)
require(ggplot2)
#for each application day, how many L0 apps
l0size<-ddply(l0,"ApplicationDateSKey",function(df) dim(df)[1])
lnsize<-ddply(ln,"ApplicationDateSKey",function(df) dim(df)[1])
#for each application day, how many L0 apps having click information
l0clicksize<-ddply(adecl0,"ApplicationDateSKey", function(df) length(unique(df$applicationID)))
qplot(factor(ApplicationDateSKey),data=l0,geom="bar")
qplot(factor(ApplicationDateSKey),data=l0,geom="bar")+coord_flip()
qplot(factor(ApplicationDateSKey),data=subset(l0,Funded==1),geom="bar")+coord_flip()

qplot(factor(ApplicationDateSKey),data=ln,geom="bar")+coord_flip()
qplot(factor(ApplicationDateSKey),data=subset(ln,Funded==1),geom="bar")+coord_flip()

ad2factorl0<-data.frame(adecl0$ApplicationDateSKey,adecl0$applicationID,adecl0$Funded)
ad2factorl0<-unique(ad2factorl0)
colnames(ad2factorl0)<-c("ApplicationDateSKey","applcationID","Funded")
qplot(factor(ApplicationDateSKey),data=ad2factorl0,geom="bar")+coord_flip()
qplot(factor(ApplicationDateSKey),data=subset(ad2factorl0,Funded==1),geom="bar")+coord_flip()

setdiff(unique(adUnique$applicationID),unique(l0$ApplicationNKey))

#to evaluate the data, use june 29 information
click29<-read.csv("click29.csv",header=FALSE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
cnames<-c("SessionID","ApplicationID","Url","Timestamp","TimeOffset","ElementID","ElementName","EventType","Parameter1","Parameter2","Parameter3","Parameter4")
colnames(click29)<-cnames

length(unique(click29$ApplicationID))

intersect(unique(click29$applicationID),unique(ln$ApplicationNKey))
#filter out the error data caused by "+    Max R2000How long do you want it for? -+ days     Repayment date"
#click29full<-click29[!is.na(click29$ElementID),]
#write.csv(click29full,file="click29full.csv")

click29<-read.csv("click29.csv",header=FALSE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);

appdetail<-read.csv("apply-details29.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);

intersect(lx$ApplicationNKey,click29$ApplicationID)
intersect(lx$ApplicationNKey,appdetail$applicationID)
ttt<-ddply(click29,"Url",function(df) length(unique(df$ApplicationID)))
ttt[which(ttt$Url=='https://www.wonga.co.za/apply-details'),]
summary(appdetail)
length(unique(appdetail$applicationID))
length(unique(click29$ApplicationID))

c1a0appids<-setdiff(click29$ApplicationID,lx$ApplicationNKey)

click0705<-read.csv("click0705.csv",header=FALSE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
lx<-read.csv("lxapps07.01-10.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
colnames(click0705)<-cnames
summary(click0705)
length(unique(click0705$ApplicationID))
head(lx)
length(unique(lx$ApplicationNKey))
intersect(lx$ApplicationNKey,click0705$ApplicationID)
ttt<-ddply(click0705,"Url",function(df) length(unique(df$ApplicationID)))
colnames(ttt)[2]<-"UniqueVisitors"
ttt[order(ttt$UniqueVisitors),]
ad<-read.csv("apply-details0705.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
summary(ad)
ddply(ad,'applicationID',function(df) nrow(df))
length(unique(ad$applicationID))
c1a0appids<-setdiff(click29$ApplicationID,lx$ApplicationNKey)
intersect(ad$applicationID,lx$ApplicationNKey)
adperf<-merge(ad,lx,by.x='applicationID',by.y='ApplicationNKey')
dim(adperf)
adperf$outcome<-ifelse(adperf$DropOff==1,1,ifelse(adperf$Declined==1,2,ifelse(adperf$NTU==1,3,4)))
table(adperf$outcome)

adp<-adperf[which(adperf$outcome!=3),]
adp<-adp[,-c(2,3,4,5)]
adp<-adp[,-c(28,29,30,31,32,33,35,36,37,40,41,42,43,44)]
adp<-adp[,-c(18,19,20,21,22,23)]
adp<-adp[,-c(22)]
adp<-adp[,-c(2,3)]
colnames(adp)
summary(adp)
cor(adp[,-1])
symnum(cor(adp[,-1]))
adp$outcomelabel<-ifelse(adp$outcome==1,'DropOff',ifelse(adp$outcome==2,'Declined','Funded'))
head(adp)
library(lattice)
bwplot(MdeltaHistBucket1~outcomelabel,data=adp)
t.test(MdeltaHistBucket1~outcome,data=adp[which(adp$outcome!=2),])
t.test(MdeltaHistBucket2~outcome,data=adp[which(adp$outcome!=2),])
t.test(MdeltaHistBucket3~outcome,data=adp[which(adp$outcome!=2),])
t.test(MdeltaHistBucket4~outcome,data=adp[which(adp$outcome!=2),])
t.test(MdeltaHistBucket5~outcome,data=adp[which(adp$outcome!=2),])
t.test(MdeltaHistBucket6~outcome,data=adp[which(adp$outcome!=2),])
t.test(MdeltaHistBucket7~outcome,data=adp[which(adp$outcome!=2),])
t.test(MdeltaHistBucket8~outcome,data=adp[which(adp$outcome!=2),])
t.test(MdeltaHistBucket9~outcome,data=adp[which(adp$outcome!=2),])
t.test(MdeltaHistBucket10~outcome,data=adp[which(adp$outcome!=2),])
t.test(Duration~outcome,data=adp[which(adp$outcome!=2),])
t.test(NumMouseMoves~outcome,data=adp[which(adp$outcome!=2),])
t.test(NumScrolls~outcome,data=adp[which(adp$outcome!=2),])
t.test(MaxDwellTime~outcome,data=adp[which(adp$outcome!=2),])
t.test(HoverCountSubmit~outcome,data=adp[which(adp$outcome!=2),])
t.test(MaxSilence~outcome,data=adp[which(adp$outcome!=2),])
t.test(TotalKeyPresses~outcome,data=adp[which(adp$outcome!=2),])
t.test(MouseOffPageIntervals~outcome,data=adp[which(adp$outcome!=2),])

bwplot(MdeltaHistBucket1~outcomelabel,data=adp)
bwplot(HoverCountSubmit~outcomelabel,data=adp)






