system("defaults write org.R-project.R force.LANG en_US.UTF-8")
setwd("/Users/georgiacharalambous/Desktop/EconomicsAssignment")
options(scipen = 999)
InternetUsers <- read.csv('finalData/InternetUsers.csv',header = T)

Inc2013v1 <- read.csv('finalData/Incidents2013.csv',header = T)
Inc2013v2 <- read.csv('finalData/Incidents2013v2.csv',header = T)
Inc2013v3 <- read.csv('finalData/Incidents2013v3.csv',header = T)
Inc2013v4 <- read.csv('finalData/Incidents2013v4.csv',header = T)

Inc2014v1 <- read.csv('finalData/Incidents2014.csv',header = T)
Inc2014v2 <- read.csv('finalData/Incidents2014v2.csv',header = T)
Inc2014v3 <- read.csv('finalData/Incidents2014v3.csv',header = T)
Inc2014v4 <- read.csv('finalData/Incidents2014v4.csv',header = T)

Inc2015v1 <- read.csv('finalData/Incidents2015.csv',header = T)
Inc2015v2 <- read.csv('finalData/Incidents2015v2.csv',header = T)
Inc2015v3 <- read.csv('finalData/Incidents2015v3.csv',header = T)

Inc2016v1 <- read.csv('finalData/Incidents2016.csv',header = T)
Inc2016v2 <- read.csv('finalData/Incidents2016v2.csv',header = T)

# LOAD GGPLOT2 GRAPHICS PACKAGE
library(ggplot2)

Inc2013<-Inc2013v1
Inc2013<-rbind(Inc2013,Inc2013v2)
Inc2013<-rbind(Inc2013,Inc2013v3)
Inc2013<-rbind(Inc2013,Inc2013v4)

Inc2014<-Inc2014v1
Inc2014<-rbind(Inc2014,Inc2014v2)
Inc2014<-rbind(Inc2014,Inc2014v3)
Inc2014<-rbind(Inc2014,Inc2014v4)

Inc2015<-Inc2015v1
Inc2015<-rbind(Inc2015,Inc2015v2)
Inc2015<-rbind(Inc2015,Inc2015v3)


Inc2016<-Inc2016v1
Inc2016<-rbind(Inc2016,Inc2016v2)

Incidents <- matrix(ncol=2)
colnames(Incidents) <- c('Year','Count') 

newRow <- data.frame(Year='2013',Count=nrow(Inc2013))
Incidents <- rbind(Incidents,newRow)

Incidents <- Incidents[2,]

newRow <- data.frame(Year='2014',Count=nrow(Inc2014))
Incidents <- rbind(Incidents,newRow)
newRow <- data.frame(Year='2015',Count=nrow(Inc2015))
Incidents <- rbind(Incidents,newRow)
newRow <- data.frame(Year='2016',Count=nrow(Inc2016))
Incidents <- rbind(Incidents,newRow)

InternetUsers <- subset(InternetUsers, select = c(InternetUsers))
Incidents <-cbind(Incidents,InternetUsers)


Inc <- rbind(Inc2014,Inc2015)
Inc <- rbind(Inc,Inc2016)

Inc$Time <- gsub(':', '', Inc$Time )
Inc$Time <-as.numeric(as.character(Inc$Time))
IncDay <- subset(Inc, Time>080000 & Time<200000)
IncNight <- subset(Inc, Time>200000)
IncTemp <-subset(Inc, Time<080000)
IncNight <-rbind(IncNight,IncTemp)

nrow(IncDay) - nrow(IncNight)
nrow(IncNight)
nrow(Inc)



IncidentsTime <- matrix(ncol=2)
colnames(IncidentsTime) <- c('Time','Count') 

newRow <- data.frame(Time='Day',Count=nrow(IncDay))
IncidentsTime <- rbind(IncidentsTime,newRow)

IncidentsTime <- IncidentsTime[2,]

newRow <- data.frame(Time='Night',Count=nrow(IncNight))
IncidentsTime <- rbind(IncidentsTime,newRow)

ggplot(IncidentsTime, aes(x=Time,y = Count)) +geom_bar(stat = "identity") +  ggtitle("Number of Incidents, Day-Night") 


Incidents$InternetUsers<-as.numeric(as.character(Incidents$InternetUsers))
Incidents<-cbind(Incidents,InternetUsers)

Incidents$Value <- Incidents$Count / Incidents$InternetUsers

# PLOT WITH GGPLOT2 PACKAGE
ggplot(data=Incidents, aes(x=Year, y=Count,group=1)) +
  geom_line(color="#aa0022", size=1.75) +
  geom_point(color="#aa0022", size=3.5) +
  ggtitle("Number of Incidents, 2013-2106") 

ggplot(Incidents, aes(x=Year,y = Count)) +geom_bar(stat = "identity") +  ggtitle("Number of Incidents, 2013-2106") 


ggplot(data=Incidents, aes(x=Year, y=Value,group=1)) +
  geom_line(color="#aa0022", size=1.75) +
  geom_point(color="#aa0022", size=3.5) +
  ggtitle("Number of Incidents-Per Internet Users, 2014-2106") 


Inc2013firshalf <- subset(Inc2013,Month<7)
Inc2013sechalf <- subset(Inc2013,Month>6)

Inc2014firshalf <- subset(Inc2014,Month<7)
Inc2014sechalf <- subset(Inc2014,Month>6)

Inc2015firshalf <- subset(Inc2015,Month<7)
Inc2015sechalf <- subset(Inc2015,Month>6)

Inc2016firshalf <- subset(Inc2016,Month<7)
Inc2016sechalf <- subset(Inc2016,Month>6)


IncidentsHalfYear <- matrix(ncol=2)
colnames(IncidentsHalfYear) <- c('Semester','Count') 

newRow <- data.frame(Semester='1',Count=nrow(Inc2013firshalf))
IncidentsHalfYear <- rbind(IncidentsHalfYear,newRow)

IncidentsHalfYear <- IncidentsHalfYear[2,]

newRow <- data.frame(Semester='2',Count=nrow(Inc2013sechalf))
IncidentsHalfYear <- rbind(IncidentsHalfYear,newRow)
newRow <- data.frame(Semester='1',Count=nrow(Inc2014firshalf))
IncidentsHalfYear <- rbind(IncidentsHalfYear,newRow)
newRow <- data.frame(Semester='2',Count=nrow(Inc2014sechalf))
IncidentsHalfYear <- rbind(IncidentsHalfYear,newRow)
newRow <- data.frame(Semester='3',Count=nrow(Inc2015firshalf))
IncidentsHalfYear <- rbind(IncidentsHalfYear,newRow)
newRow <- data.frame(Semester='4',Count=nrow(Inc2015sechalf))
IncidentsHalfYear <- rbind(IncidentsHalfYear,newRow)
newRow <- data.frame(Semester='5',Count=nrow(Inc2016firshalf))
IncidentsHalfYear <- rbind(IncidentsHalfYear,newRow)
newRow <- data.frame(Semester='6',Count=nrow(Inc2016sechalf))
IncidentsHalfYear <- rbind(IncidentsHalfYear,newRow)

# PLOT WITH GGPLOT2 PACKAGE
ggplot(data=IncidentsHalfYear, aes(x=Semester, y=Count,group=1)) +
  geom_line(color="#aa0022", size=1.75) +
  geom_point(color="#aa0022", size=3.5) +
  ggtitle("Number of Incidents, 2014-2106 (Per 6 months)") 

#Season Counter

Inc2014Winter <-subset(Inc2014,Month=1)
nrow(Inc2014Winter)
Inc2014Winter2 <-subset(Inc2014,Month=2)
nrow(Inc2014Winter2)
Inc2014Winter <-subset(Inc2014,Month=12)

Inc2014Spring <-subset(Inc2014,Month=3)
nrow(Inc2014Spring)
Inc2014Spring <-subset(Inc2014,Month=4)
Inc2014Spring <-subset(Inc2014,Month=5)

Inc2014Summer <-subset(Inc2014,Month=3)
Inc2014Summer <-subset(Inc2014,Month=4)
Inc2014Summer <-subset(Inc2014,Month=5)


Inc2014firQ <- subset(Inc2014,Month<5)
nrow(Inc2014firQ)
Inc2014secQ <- subset(Inc2014,Month>4 & Month < 9)
Inc2014thiQ <- subset(Inc2014,Month>9)


Inc2015firQ <- subset(Inc2015,Month<5)
Inc2015secQ <- subset(Inc2015,Month>4 & Month < 9)
Inc2015thiQ <- subset(Inc2015,Month>9)

Inc2016firQ <- subset(Inc2016,Month<5)
Inc2016secQ <- subset(Inc2016,Month>4 & Month < 9)
Inc2016thiQ <- subset(Inc2016,Month>9)

IncQ  <- matrix(ncol=2)
colnames(IncQ) <- c('Quarter','Count') 

newRow <- data.frame(Quarter='1',Count=nrow(Inc2014firQ))
IncQ <- rbind(IncQ,newRow)

IncQ <- IncQ[2,]

newRow <- data.frame(Quarter='2',Count=nrow(Inc2014secQ))
IncQ <- rbind(IncQ,newRow)
newRow <- data.frame(Quarter='3',Count=nrow(Inc2014thiQ))
IncQ <- rbind(IncQ,newRow)
newRow <- data.frame(Quarter='4',Count=nrow(Inc2015firQ))
IncQ <- rbind(IncQ,newRow)
newRow <- data.frame(Quarter='5',Count=nrow(Inc2015thiQ))
IncQ <- rbind(IncQ,newRow)
newRow <- data.frame(Quarter='6',Count=nrow(Inc2015thiQ))
IncQ <- rbind(IncQ,newRow)
newRow <- data.frame(Quarter='7',Count=nrow(Inc2016firQ))
IncQ <- rbind(IncQ,newRow)
newRow <- data.frame(Quarter='8',Count=nrow(Inc2016secQ))
IncQ <- rbind(IncQ,newRow)
newRow <- data.frame(Quarter='9',Count=nrow(Inc2016thiQ))
IncQ <- rbind(IncQ,newRow)


ggplot(data=IncQ, aes(x=Quarter, y=Count,group=1)) +
  geom_line(color="#aa0022", size=1.75) +
  geom_point(color="#aa0022", size=3.5) +
  ggtitle("Number of Incidents, 2014-2106 (Per 4 months)") 

Domain<-strsplit(gsub("http://|https://|www\\.", "", Inc2013$Link), "/")[[c(1, 1)]]


findDomain <- function(x) strsplit(gsub("http://|https://|www\\.", "", x), "/")[[c(1, 1)]]

Domain <- strsplit(gsub("http://|https://|www\\.", "", Inc2013v2$Link), "/")[[c(5, 1)]]
df <- data.frame(matrix(unlist(Domain), nrow=5, byrow=T),stringsAsFactors=FALSE)

DomainTest<-data.frame()
for (i in 1:nrow(Inc2013v2)){
  x<-data.frame(DomainTest=findDomain(Inc2013v2$Link[i]))
  DomainTest<-rbind(DomainTest,x)
}
DomainTest2=DomainTest[2:nrow(Domain),]
Inc2013<-cbind(Inc2013,Domain)
write.csv(Inc2013,"Incident2013.csv")
Domain<-data.frame()

for (i in 1:nrow(Inc2014)){
  print(i)
  x<-data.frame(Domain=findDomain(Inc2014$Link[i]))
  Domain<-rbind(Domain,x)
}
nrow(Inc2014)

n <- nrow(Inc2014) * 0.1
Inc2014sample <-Inc2014[sample(nrow(Inc2014), n), ]
n <- nrow(Inc2015) * 0.1
Inc2015sample <-Inc2015[sample(nrow(Inc2015), n), ]
n <- nrow(Inc2016) * 0.1
Inc2016sample <-Inc2014[sample(nrow(Inc2016), n), ]
for (i in 1:nrow(Inc2014sample)){
  print(i)
  x<-data.frame(Domain=findDomain(Inc2014sample$Link[i]))
  Domain<-rbind(Domain,x)
}
write.csv(Domain,"Domain.csv")
Domain <- read.csv('finalData/Domain.csv',header = T)
Inc2014sample<-cbind(Inc2014sample,Domain)

library(stringr)
df<-str_split_fixed(Inc2014sample$Domain, "\\.", 2)
Inc2014sample<-cbind(df, Inc2014sample)
write.csv(Domain,"Inc2014sampleDomain.csv")


Domain2015<-data.frame()
#########################
for (i in 1:nrow(Inc2015sample)){
  print(i)
  x<-data.frame(Domain2015=findDomain(Inc2015sample$Link[i]))
  Domain2015<-rbind(Domain2015,x)
}
nrow(Inc2014sample)
nrow(Inc2015sample)
nrow(Domain)
nrow(Inc2014sample) + nrow(Inc2015sample)
Domain2016<-data.frame()
#########################
for (i in 1:nrow(Inc2016sample)){
  print(i)
  x<-data.frame(Domain2016=findDomain(Inc2016sample$Link[i]))
  Domain2016<-rbind(Domain2016,x)
}
write.csv(Domain2016,"Domain2016.csv")
Inc2016sample<-cbind(Inc2016sample,Domain2016)
write.csv(Inc2016sample,"Inc2016sampleDomain.csv")
Domain<-Domain[-(1:99832),]
write.csv(Domain,"Domain2015.csv")
