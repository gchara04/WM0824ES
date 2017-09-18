system("defaults write org.R-project.R force.LANG en_US.UTF-8")
setwd("/Users/georgiacharalambous/Desktop/EconomicsAssignment")
options(scipen = 999)

Inc2014v1 <- read.csv('finalData/Incidents2014.csv',header = T)
Inc2014v2 <- read.csv('finalData/Incidents2014v2.csv',header = T)
Inc2014v3 <- read.csv('finalData/Incidents2014v3.csv',header = T)
Inc2014v4 <- read.csv('finalData/Incidents2014v4.csv',header = T)

Inc2015v1 <- read.csv('finalData/Incidents2015.csv',header = T)
Inc2015v2 <- read.csv('finalData/Incidents2015v2.csv',header = T)
Inc2015v3 <- read.csv('finalData/Incidents2015v3.csv',header = T)

Inc2016v1 <- read.csv('finalData/Incidents2016.csv',header = T)
Inc2016v2 <- read.csv('finalData/Incidents2016v2.csv',header = T)

Inc2014<-Inc2014v1
Inc2014<-rbind(Inc2014,Inc2014v2)
Inc2014<-rbind(Inc2014,Inc2014v3)
Inc2014<-rbind(Inc2014,Inc2014v4)

Inc2015<-Inc2015v1
Inc2015<-rbind(Inc2015,Inc2015v2)
Inc2015<-rbind(Inc2015,Inc2015v3)


Inc2016<-Inc2016v1
Inc2016<-rbind(Inc2016,Inc2016v2)

nrow(Inc2014) + nrow(Inc2015) + nrow(Inc2016)

write.csv(Inc2016sample,"Inc2016sample.csv")
write.csv(Inc2015sample,"Inc2015sample.csv")
write.csv(Inc2014sample,"Inc2014sample.csv")
Domain2014 <- read.csv('finalData/Domain2014',header = T)
Domain2015 <- read.csv('finalData/Domain2015.csv',header = T)
Domain2016 <- read.csv('finalData/Inc2016sampleDomain.csv',header = T)
Domain2014Test <- read.csv('finalData/Inc2014sample.csv',header = T)
Inc2016sample<-cbind(Inc2016sample,Domain2016)
df<-str_split_fixed(Inc2016sample$Domain2016, "\\.", 2)
Inc2016sample<-cbind(df, Inc2016sample)
library(dplyr)
DomainType <- Domain2014Test %>% 
  group_by(TypeDomain) %>%
  summarise(Freq = n())
DomainType 
DomainTypeSubset <- subset(DomainType, Freq>100)
barplot(DomainTypeSubset$Freq, names = DomainTypeSubset$TypeDomain, 
        xlab = "Domain", ylab = "Frequency", main = "Domain Frequencies(>100) Sample 2014")

ServiceType <- Domain2014Test %>% 
  group_by(X1) %>%
  summarise(Freq = n())
ServiceType 
ServiceTypesSubset <- subset(ServiceType, Freq>100)
barplot(ServiceTypesSubset$Freq, names = ServiceTypesSubset$X1, 
        xlab = "Service", ylab = "Frequency", main = "Service Frequencies(>100) Sample 2014")


Inc2015sample<-cbind(Inc2015sample,Domain2015)
df<-str_split_fixed(Inc2015sample$Domain, "\\.", 2)
Inc2015sample<-cbind(df, Inc2015sample)
