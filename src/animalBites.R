#setwd("/Users/rafa/Documents/SS2018/DataVis/dataVizualisation")
setwd("~/Desktop/project2/")
## @knitr part1

#data source: https://data.louisvilleky.gov/dataset/animal-bites

#read and attach data
AnimalData <- read.csv("data/Health_AnimalBites.csv", stringsAsFactors = FALSE)

#First we will take a look at our data
dim(AnimalData)
names(AnimalData)
#data cleaning, convert missing values to NA
AnimalData[AnimalData == ""] <- NA
apply(is.na(AnimalData), 2, sum)
#clean wrong dates
AnimalData$bite_date = as.Date(AnimalData$bite_date, format('%Y-%m-%d'))
AnimalData<-AnimalData[AnimalData$bite_date > as.Date("1985-05-05") & AnimalData$bite_date < as.Date("2018-06-21 00:00:00"),]


#almost all coulums have missing values
#the most complete columns are: bite_date, SpeciesIDDesc
#the most values are missing here: ResolveCompYesNo, head_sent_date
#head_sent_date has the most missing values since not
#every head of the animal which was registered is cut off and send to the department of health care
library(ggplot2)
## @knitr animalBitesPerSpecies
AnimalData$SpeciesIDDesc <-
  factor(AnimalData$SpeciesIDDesc, levels = unique(as.character(AnimalData$SpeciesIDDesc)))
ggplot(data = subset(AnimalData,!is.na(SpeciesIDDesc)),aes(x = SpeciesIDDesc[], fill = SpeciesIDDesc))+ geom_bar(stat = "count")+ xlab("Species") + ylab("Bites")+ ggtitle("Animalbites per Species")+ xlim(names(sort(table(AnimalData$SpeciesIDDesc), decreasing = TRUE)[1:5]))


## @knitr pieChartForBites
theme_set(theme_classic())
df <- as.data.frame(table(AnimalData$SpeciesIDDesc))
colnames(df) <- c("class", "freq")
pie <- ggplot(df, aes(x = "", y=freq, fill = factor(class))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="class", 
       x=NULL, 
       y=NULL, 
       title="Bites per Species", 
       caption="")

pie + coord_polar(theta = "y", start=0)

## @knitr dogBitesPerBreed

AnimalData$BreedIDDesc <-factor(AnimalData$BreedIDDesc, levels = unique(as.character(AnimalData$BreedIDDesc)))
ggplot(data = subset(AnimalData,!is.na(BreedIDDesc)),aes(x = BreedIDDesc[], fill =BreedIDDesc))+geom_bar(stat = "count")+xlab("Breed") + ylab("Bites")+ggtitle("Animalbites per Breed")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+xlim(names(sort(table(AnimalData$BreedIDDesc), decreasing = TRUE)))+ guides(fill=FALSE)
## @knitr mostCommon30
AnimalData$BreedIDDesc <-factor(AnimalData$BreedIDDesc, levels = unique(as.character(AnimalData$BreedIDDesc)))
ggplot(data = subset(AnimalData,!is.na(BreedIDDesc)),aes(x = BreedIDDesc[], fill =BreedIDDesc))+geom_bar(stat = "count")+xlab("Breed") + ylab("Bites")+ggtitle("Top 30 Animalbites per Breed")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+xlim(names(sort(table(AnimalData$BreedIDDesc), decreasing = TRUE)[1:30]))+ guides(fill=FALSE)



## @knitr genderOfDogsInvolvedInBitesBarplot
AnimalData$GenderIDDesc[which(is.na(AnimalData$GenderIDDesc))]<-"UNKNOWN"
ggplot(data=AnimalData, aes(x=GenderIDDesc[], fill =GenderIDDesc))+geom_bar(stat = "count")+xlab("Gender")+ylab("Bites")+ggtitle("Bites per Gender")+ guides(fill=FALSE)

## @knitr genderOfDogsInvolvedInBites
ggplot(AnimalData, aes(x="", y=GenderIDDesc[], fill=GenderIDDesc))+ggtitle("Gender of Dogs involved in Bites")+ ylab("Proportion of Bites")+geom_bar(width = 1, stat = "identity")+theme(axis.title.x=element_blank(),axis.text=element_blank(),axis.ticks.x=element_blank(),legend.title = element_blank())

## @knitr breedOfMaleDogsInvolvedInBites
#breed of male dogs involved in bites
#30 most common
AnimalDataMale<-AnimalData[AnimalData$GenderIDDesc=="MALE",]
AnimalDataMale$BreedIDDesc <-factor(AnimalDataMale$BreedIDDesc, levels = unique(as.character(AnimalDataMale$BreedIDDesc)))
ggplot(data = subset(AnimalDataMale,!is.na(BreedIDDesc)),aes(x = BreedIDDesc[], fill =BreedIDDesc))+geom_bar(stat = "count")+xlab("Breed") + ylab("Bites")+ggtitle("Top 30 Animalbites per Male Breed")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+xlim(names(sort(table(AnimalDataMale$BreedIDDesc), decreasing = TRUE)[1:30]))+ guides(fill=FALSE)

## @knitr breedOfFemaleDogsInvolvedInBites
#breed of female dogs involved in bites
#30 most common
AnimalDataFemale<-AnimalData[AnimalData$GenderIDDesc=="FEMALE",]
AnimalDataFemale$BreedIDDesc <-factor(AnimalDataFemale$BreedIDDesc, levels = unique(as.character(AnimalDataFemale$BreedIDDesc)))
ggplot(data = subset(AnimalDataFemale,!is.na(BreedIDDesc)),aes(x = BreedIDDesc[], fill =BreedIDDesc))+geom_bar(stat = "count")+xlab("Breed") + ylab("Bites")+ggtitle("Top 30 Animalbites per Female Breed")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+xlim(names(sort(table(AnimalDataFemale$BreedIDDesc), decreasing = TRUE)[1:30]))+ guides(fill=FALSE)

## @knitr breedOfUnknownDogsInvolvedInBites
#30 most common
AnimalDataUnknown<-AnimalData[AnimalData$GenderIDDesc=="UNKNOWN",]
AnimalDataUnknown$BreedIDDesc <-factor(AnimalDataUnknown$BreedIDDesc, levels = unique(as.character(AnimalDataUnknown$BreedIDDesc)))
ggplot(data = subset(AnimalDataUnknown,!is.na(BreedIDDesc)),aes(x = BreedIDDesc[], fill =BreedIDDesc))+geom_bar(stat = "count")+xlab("Breed") + ylab("Bites")+ggtitle("Top 30 Animalbites per Breed with Unknown gender")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+xlim(names(sort(table(AnimalDataUnknown$BreedIDDesc), decreasing = TRUE)[1:30]))+ guides(fill=FALSE)

## @knitr dogBitesPerYear
#create copy of orig data
AnimalDataOnlyYear<-AnimalData
#remove rows with NA
AnimalDataOnlyYear<-AnimalDataOnlyYear[!is.na(AnimalDataOnlyYear$bite_date),]
#extract year
AnimalDataOnlyYear$bite_date = format(as.Date(AnimalDataOnlyYear$bite_date, format="%Y-%m-%d"),"%Y")
ggplot(AnimalDataOnlyYear, aes(x=bite_date[], fill =AnimalDataOnlyYear$GenderIDDesc)) + geom_bar(stat="count")+xlab("Year")+ylab("Number of Bites")+ggtitle("Dog Bites per Year")+theme(legend.title=element_blank())+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

## @knitr dogBitesPerMonth
AnimalDataOnlyMonth<-AnimalData
AnimalDataOnlyMonth<-AnimalDataOnlyMonth[!is.na(AnimalDataOnlyMonth$bite_date),]
AnimalDataOnlyMonth$bite_date = format(as.Date(AnimalDataOnlyMonth$bite_date, format="%Y-%m-%d"),"%m")
ggplot(AnimalDataOnlyMonth, aes(x=bite_date,fill = AnimalDataOnlyMonth$GenderIDDesc)) + geom_bar(stat="count")+xlab("Month of the Year")+ylab("Number of Bites")+ggtitle("Dog Bites per Month")+theme(legend.title=element_blank())

## @knitr dogBitesPerDay
AnimalDataOnlyDay<-AnimalData
AnimalDataOnlyDay<-AnimalDataOnlyDay[!is.na(AnimalDataOnlyDay$bite_date),]
AnimalDataOnlyDay$bite_date = format(as.Date(AnimalDataOnlyDay$bite_date, format="%Y-%m-%d"),"%d")
ggplot(AnimalDataOnlyDay, aes(x=bite_date,fill = AnimalDataOnlyYear$GenderIDDesc)) + geom_bar(stat="count")+xlab("Day of the Month")+ylab("Number of Bites")+ggtitle("Dog Bites per Day")+theme(legend.title=element_blank())

## @knitr dogBitesPerBreedInTheHead
AnimalDataHead<-AnimalData[AnimalData$WhereBittenIDDesc=="HEAD",]
AnimalDataHead$BreedIDDesc <-factor(AnimalDataHead$BreedIDDesc, levels = unique(as.character(AnimalDataHead$BreedIDDesc)))
ggplot(data = subset(AnimalDataHead,!is.na(BreedIDDesc)),aes(x = BreedIDDesc[], fill =BreedIDDesc))+geom_bar(stat = "count")+xlab("Breed") + ylab("Bites")+ggtitle("Top 30 Breeds with Head Bites")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+xlim(names(sort(table(AnimalDataHead$BreedIDDesc), decreasing = TRUE)[1:30]))+ guides(fill=FALSE)


## @knitr dogBitesPerBreedInTheBody
AnimalDataBody<-AnimalData[AnimalData$WhereBittenIDDesc=="BODY",]
AnimalDataBody$BreedIDDesc <-factor(AnimalDataBody$BreedIDDesc, levels = unique(as.character(AnimalDataBody$BreedIDDesc)))
ggplot(data = subset(AnimalDataBody,!is.na(BreedIDDesc)),aes(x = BreedIDDesc[], fill =BreedIDDesc))+geom_bar(stat = "count")+xlab("Breed") + ylab("Bites")+ggtitle("Top 30 Breeds with Body Bites")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+xlim(names(sort(table(AnimalDataBody$BreedIDDesc), decreasing = TRUE)[1:30]))+ guides(fill=FALSE)

## @knitr pitbullBitesPerYear
AnimalDataPitBull<-AnimalData[AnimalData$BreedIDDesc=="PIT BULL",]
AnimalDataPitBull$WhereBittenIDDesc[which(is.na(AnimalDataPitBull$WhereBittenIDDesc))]<-"UNKNOWN"
AnimalDataPitBull<-AnimalDataPitBull[!is.na(AnimalDataPitBull$bite_date),]
AnimalDataPitBull$bite_date = format(as.Date(AnimalDataPitBull$bite_date, format="%Y-%m-%d"),"%Y")
ggplot(AnimalDataPitBull, aes(x=bite_date[], fill = AnimalDataPitBull$WhereBittenIDDesc)) + geom_bar(stat="count")+xlab("Year")+ylab("Number of Bites")+ggtitle("Pit Bull Bites per Year")+theme(legend.title=element_blank())



## @knitr catBitesPerGenderAsWafflechart
#cat bites per gender
AnimalDataCat<-AnimalData[AnimalData$SpeciesIDDesc=="CAT",]
AnimalDataCat$GenderIDDesc[which(is.na(AnimalDataCat$GenderIDDesc))]<-"UNKNOWN"
#ggplot(data=AnimalDataCat, aes(x=GenderIDDesc[], fill =GenderIDDesc))+geom_bar(stat = "count")+xlab("Gender")+ylab("Bites")+ggtitle("Cat Bites per Gender")+guides(fill=FALSE)
var <- AnimalDataCat$GenderIDDesc  # the categorical data 
nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
categ_table <- round(table(var) * ((nrows*nrows)/(length(var))))
df$category <- factor(rep(names(categ_table), categ_table))  
## Plot
ggplot(df, aes(x = x, y = y, fill = category)) + 
  geom_tile(color = "black", size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_brewer(palette = "Set3") +
  labs(title="Cat Bites", subtitle="per Gender",
       caption="Number of Bites vs Gender") + 
  theme(#panel.border = element_rect(size = 2),
        plot.title = element_text(size = rel(1.2)),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.position = "right")

## @knitr catBitesPerYear
AnimalDataCatYear<-AnimalData[AnimalData$SpeciesIDDesc=="CAT",]
AnimalDataCatYear<-AnimalDataCatYear[!is.na(AnimalDataCatYear$bite_date),]
AnimalDataCatYear$bite_date = format(as.Date(AnimalDataCatYear$bite_date, format="%Y-%m-%d"),"%Y")
ggplot(AnimalDataCatYear, aes(x=bite_date, fill = bite_date)) + geom_bar(stat="count")+xlab("Year")+ylab("Number of Bites")+ggtitle("Cat Bites per Year")+guides(fill=FALSE)+theme(legend.title=element_blank())

## @knitr catBitesPerMonth
CatDataOnlyMonth<-AnimalData[AnimalData$SpeciesIDDesc=="CAT",]
CatDataOnlyMonth<-CatDataOnlyMonth[!is.na(CatDataOnlyMonth$bite_date),]
CatDataOnlyMonth$bite_date = format(as.Date(CatDataOnlyMonth$bite_date, format="%Y-%m-%d"),"%m")
ggplot(CatDataOnlyMonth, aes(x=bite_date,fill = bite_date)) + geom_bar(stat="count")+guides(fill=FALSE)+xlab("Month of the Year")+ylab("Number of Bites")+ggtitle("Cat Animal Bites per Month")+theme(legend.title=element_blank())

## @knitr catBitesPerDay
CatDataOnlyDay<-AnimalData[AnimalData$SpeciesIDDesc=="CAT",]
CatDataOnlyDay<-CatDataOnlyDay[!is.na(CatDataOnlyDay$bite_date),]
CatDataOnlyDay$bite_date = format(as.Date(CatDataOnlyDay$bite_date, format="%Y-%m-%d"),"%d")
ggplot(CatDataOnlyDay, aes(x=bite_date,fill = bite_date)) + geom_bar(stat="count")+guides(fill=FALSE)+xlab("Day of the Month")+ylab("Number of Bites")+ggtitle("Cat Bites per Day")+theme(legend.title=element_blank())

## @knitr otherSpeciesExcludingDogsAndCats
otherAnimalsData<-AnimalData[AnimalData$SpeciesIDDesc != "DOG" & AnimalData$SpeciesIDDesc != "CAT",]
otherAnimalsData$SpeciesIDDesc <-
  factor(otherAnimalsData$SpeciesIDDesc, levels = unique(as.character(otherAnimalsData$SpeciesIDDesc)))
ggplot(data = subset(otherAnimalsData,!is.na(SpeciesIDDesc)),aes(x = SpeciesIDDesc[], fill = SpeciesIDDesc))+ geom_bar(stat = "count")+ xlab("Other Secies") + ylab("Number of Bites")+ ggtitle("Animalbites per  other Species")+ xlim(names(sort(table(otherAnimalsData$SpeciesIDDesc), decreasing = TRUE)))+theme(legend.title=element_blank())

## @knitr otherAnimalBitesPerYear
otherAnimalsDataYear<-AnimalData[AnimalData$SpeciesIDDesc != "DOG" & AnimalData$SpeciesIDDesc != "CAT",]
otherAnimalsDataYear<-otherAnimalsDataYear[!is.na(otherAnimalsDataYear$bite_date),]
otherAnimalsDataYear$bite_date = format(as.Date(otherAnimalsDataYear$bite_date, format="%Y-%m-%d"),"%Y")
ggplot(otherAnimalsDataYear, aes(x=bite_date, fill = otherAnimalsDataYear$SpeciesIDDesc)) + geom_bar(stat="count")+xlab("Year")+ylab("Number of Bites")+ggtitle("Other Animal Bites per Year")+theme(legend.title=element_blank())


## @knitr otherAnimalBitesPerMonth
otherAnimalsDataMonth<-AnimalData[AnimalData$SpeciesIDDesc != "DOG" & AnimalData$SpeciesIDDesc != "CAT",]
otherAnimalsDataMonth<-otherAnimalsDataMonth[!is.na(otherAnimalsDataMonth$bite_date),]
otherAnimalsDataMonth$bite_date = format(as.Date(otherAnimalsDataMonth$bite_date, format="%Y-%m-%d"),"%m")
ggplot(otherAnimalsDataMonth, aes(x=bite_date,fill = otherAnimalsDataMonth$SpeciesIDDesc)) + geom_bar(stat="count")+xlab("Month of the Year")+ylab("Number of Bites")+ggtitle("Other Animal Bites per Month")+theme(legend.title=element_blank())


## @knitr otherAnimalBitesPerDay
otherAnimalsDataDay<-AnimalData[AnimalData$SpeciesIDDesc != "DOG" & AnimalData$SpeciesIDDesc != "CAT",]
otherAnimalsDataDay<-otherAnimalsDataDay[!is.na(otherAnimalsDataDay$bite_date),]
otherAnimalsDataDay$bite_date = format(as.Date(otherAnimalsDataDay$bite_date, format="%Y-%m-%d"),"%d")
ggplot(otherAnimalsDataDay, aes(x=bite_date,fill = otherAnimalsDataDay$SpeciesIDDesc)) + geom_bar(stat="count")+xlab("Day of the Month")+ylab("Number of Bites")+ggtitle("Other Animal Bites per Day")+theme(legend.title=element_blank())


