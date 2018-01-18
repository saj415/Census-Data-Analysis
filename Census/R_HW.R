##### Author: Sachin Joshi #####
library("ggplot2")

##################################### Task 1 ##############################################################################

######Data Accumulation
censusdata <- read.table("DataSet.txt", header=TRUE, sep=",")
censusdict <- read.fwf("DataDict.txt", c(9, -1, 86, 5, -1, 7, -1, 9, -2, 7, -1, 9, -1, 6))
censusfips <- read.fwf("FIPS_CountyName.txt", c(5, -1, 100))
census <- merge(censusdata, censusfips, by.x="fips", by.y=1)
######End Data Accumulation

######Data Pre-processing
census$V2 <- as.character(census$V2)

##Getting all the state names
a=tail(census[,"fips"], n=1);
states <- as.vector(subset(census, fips %in% seq(0,a,by=1000), select=V2))

##Get the data frame indices for all the states
tmp <- which(census$V2 %in% states$V2)

##Duplicate dataset for census
newdata<-census[-c(tmp),]

##Removing the row with the country name
states<-states$V2[-c(1)]

##Converting all the counties to their respective states
for (i in 1:length(states)){
  first<-which(grepl(", ",newdata[,"V2"]))[1]
  sub<-substr(newdata[first,"V2"],nchar(newdata[first,"V2"])-3,nchar(newdata[first,"V2"]))
  while(grepl(sub,newdata[first,"V2"])[1]){
    newdata[first,"V2"] <- states[i];
    first<-first+1;
  }
}
#######End Data Pre-processing

#######Data Visualization
##Plotting data for each state in a sub-plot
task1<-qplot(x=newdata[,"PST045213"],y=newdata[,"EDU685212"],data=newdata,xlab="Population",ylab="Percentage of people with Bachelors degree")
task1<-task1+facet_wrap(~V2, scales="free_x",ncol=5)
#######End Data Visualization

################################### End Task 1 ############################################################################

##################################### Task 2 ##############################################################################
######Data Accumulation
data(UKDriverDeaths) #Road casualities in UK between 1969 and 1984
data(UKgas) 		 #Gas comsumption in UK
######End Data Accumulation

######Data Pre-processing
#Combining quarterly data into yearly data
ukgas<-aggregate(UKgas,nfrequency=1)
#Combining monthly data into yearly data
ukddmon<-aggregate(UKDriverDeaths, nfrequency=1)
#Finding the intersecting year
uk<-ts.intersect(ukgas,ukddmon)
ukdf<-data.frame(time(uk),uk[,"ukgas"],uk[,"ukddmon"])
######End Data Pre-processing

######Data Visualization
task2<-ggplot(ukdf,aes(time(uk)))+geom_line(aes(y=uk[,"ukgas"],colour="UK Yearly Gas Consumption"))+geom_line(aes(y=uk[,"ukddmon"],colour="Road Casualities"))+xlab("Year")+ylab("Gas consumption/Road Casualites")+ggtitle("UK yearly gas consumption vs Road Casualities between 1969 and 1984")+guides(fill=guide_legend(title="New Legend Title"))
######End Data Visualization
################################### End Task 2 ############################################################################

##Publishing data in to a PDF file
pdf(file="R_Prog_HW.pdf",width=20,height=21)
print(task1)
print(task2)
dev.off()