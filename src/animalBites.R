setwd("/Users/alexander.jaenisch/Desktop/project2/")

#data source: https://data.louisvilleky.gov/dataset/animal-bites

#read and attach data
AnimalData <- read.csv("data/Health_AnimalBites.csv")

#First we will take a look at our data
dim(AnimalData)
names(AnimalData)

#data cleaning, convert missing values to NA
AnimalData[AnimalData == ""] <- NA
apply(is.na(AnimalData), 2, sum)

#almost all coulums have missing values
#the most complete columns are: bite_date, SpeciesIDDesc
#the most values are missing here: ResolveCompYesNo, head_sent_date
#head_sent_date has the most missing values since not
#every head of the animal which was registered is cut off and send to the department of health care
library(ggplot2)
#animal bites per Species
AnimalData$SpeciesIDDesc <-
  factor(AnimalData$SpeciesIDDesc, levels = unique(as.character(AnimalData$SpeciesIDDesc)))
ggplot(data = subset(AnimalData,!is.na(SpeciesIDDesc)),aes(x = SpeciesIDDesc[], fill = SpeciesIDDesc))+ geom_bar(stat = "count")+ xlab("Species") + ylab("Bites")+ ggtitle("Animalbites per Species")+ xlim(names(sort(table(AnimalData$SpeciesIDDesc), decreaZsing = TRUE)[1:5]))

#dog bites per breed

AnimalData$BreedIDDesc <-factor(AnimalData$BreedIDDesc, levels = unique(as.character(AnimalData$BreedIDDesc)))
ggplot(data = subset(AnimalData,!is.na(BreedIDDesc)),aes(x = BreedIDDesc[], fill =BreedIDDesc))+geom_bar(stat = "count")+xlab("Species") + ylab("Bites")+ggtitle("Animalbites per Species")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+xlim(names(sort(table(AnimalData$BreedIDDesc), decreaZsing = TRUE)))+ guides(fill=FALSE)



       