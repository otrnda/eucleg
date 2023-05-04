setwd("d:/work/statistika/Eucleg/analize")
source("d:/work/statistika/Eucleg/analize/loader.r")

podaciFolder = "d:/work/statistika/Eucleg/data/"
rezultatiFolder = resultsFolderCreator("d:/work/statistika/Eucleg/results/")


library(GGEBiplots)
library(GGEBiplotGUI)

data(Ontario)
View(Ontario)

cols <- c("character", "integer", "integer", "character", "character", "integer", "character", "integer", "numeric")
ggeData <- readExcelFajl(1, cols, "GGE_X05a_Flow_days.xlsx")
ggeData$Row <- NULL
ggeData$Column <- NULL
ggeData$EUCLEG.ID <-NULL
ggeData$EntryNo <- NULL
ggeData$Plot <- NULL
ggeData$Accession.name <- NULL
library(tidyr)
ggeDataGrouped <- ggeData %>% group_by(Location, EucIdShort)%>% summarise_at(vars(X05a_Flow_days), list(X05a_Flow_days = mean))

ggeDataWide <- ggeDataGrouped %>% spread(Location, X05a_Flow_days)

ggeDataWide$EucIdShort <- NULL


list_na <- colnames(ggeDataWide)[ apply(ggeDataWide, 2, anyNA) ]

average_missing <- apply(ggeDataWide[,colnames(ggeDataWide) %in% list_na], 2, mean, na.rm =  TRUE)
View(ggeDataWideNoNa)

ggeDataWide[220,] <- NULL


write.csv(ggeDataWide,"test.csv", row.names = TRUE)
GGE1<-GGEModel(ggeDataWide)
GGEPlot(GGE1)
GGEBiplot(Data = ggeDataWide)
?GGEModel


library(agridat)
data(yan.winterwheat)
dat1 <- yan.winterwheat

library(gge)
m1 <- gge(dat1, yield~gen*env, scale=FALSE)
biplot(m1, main="yan.winterwheat - GGE biplot",flip=c(1,0), origin=0, hull=TRUE)

m1 <- gge(ggeDataGrouped, X05a_Flow_days~EucIdShort*Location, scale=TRUE)
biplot(m1, main="yan.winterwheat - GGE biplot",flip=c(1,0), origin=0, hull=TRUE)



