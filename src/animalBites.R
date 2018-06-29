setwd("/Users/rafa/Documents/SS2018/DataVis/task2")

#data source: https://data.louisvilleky.gov/dataset/animal-bites
AnimalData <- read.csv("data/Health_AnimalBites.csv")

#First we will take a look at our data
dim(AnimalData)
names(AnimalData)
#data cleaning, convert missing values to NA
AnimalData[AnimalData == ""]<-NA
apply(is.na(data),2,sum)

#almost all coulums have missing values
#the most complete columns are: bite_date, SpeciesIDDesc 
#the most values are missing here: ResolveCompYesNo, head_sent_date
#head_sent_date has the most missing values since not
#every head of the animal which was registered is cut off and send to the department of health care

AnimalData$SpeciesIDDesc <- factor(AnimalData$SpeciesIDDesc, levels=unique(as.character(AnimalData$SpeciesIDDesc)) )
attach(AnimalData)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cols = rainbow(unique(SpeciesIDDesc))
# To use for fills, add
scale_fill_manual(values=cbPalette)
#barplot(table(sort(SpeciesIDDesc,decreasing=TRUE)), ylab='Bites', xlab='Species', 
#     main = 'Animalbites per Species', col = rainbow(unique(SpeciesIDDesc)))
AnimalData$SpeciesIDDesc <- factor(AnimalData$SpeciesIDDesc, levels = AnimalData$SpeciesIDDesc[order(data$val)])
#ggplot(data=subset(AnimalData, !is.na(SpeciesIDDesc)), aes(x=SpeciesIDDesc))+geom_bar(stat = "count", aes(colour=rainbow(unique(SpeciesIDDesc))))

ggplot(data=subset(AnimalData, !is.na(SpeciesIDDesc)), aes(x=SpeciesIDDesc[], fill = SpeciesIDDesc))+
  geom_bar(stat = "count")+
  xlab("Species")+ylab("Bites")+
  ggtitle("Animalbites per Species")+
  xlim(names(sort(table(AnimalData$SpeciesIDDesc), decreasing=TRUE)[1:5]))
#p+scale_fill_manual(values=cols)

# To use for fills, add

       