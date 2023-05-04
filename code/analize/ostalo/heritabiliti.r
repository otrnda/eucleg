
setwd("D:/work/statistika/Eucleg/analize")
source("loader.r")

podaciFolder = "d:/work/statistika/Eucleg/data/"
rezultatiFolder = resultsFolderCreator("d:/work/statistika/Eucleg/results/")

# agro18 <- readLocationData("agrovegetal1811.xlsx")
# agro19 <- readLocationData("agrovegetal1911.xlsx")
# bor18 <- readLocationData("boreal1805.xlsx")
# bor19 <- readLocationData("boreal1906.xlsx")
# ikbks18 <- readLocationData("ikbks1804.xlsx")
# ikbks19 <- readLocationData("ikbks1903.xlsx")
# ikbks20 <- readLocationData("ikbks2003.xlsx")
# melle18 <- readLocationData("melle1804.xlsx") 
# melle19 <- readLocationData("melle1904.xlsx")
# allDataList <- list(agro18, agro19, bor18, bor19, ikbks18, ikbks19, ikbks20, melle18, melle19)


dataForHeritabiliti <- readExcelFajl(1, c("character", "character", "double"), "mainTraits V2 small.xlsx")


library(lme4)

m1 <- lmer(X09_Plant_height ~ (1 | Location)  + (1 | EucIdShort ), data = dataForHeritabiliti, REML = TRUE)
m1 <- lmer(X09_Plant_height ~ Location  + (1 | EucIdShort ), data = dataForHeritabiliti, REML = TRUE)
summary(m1)
coef(m1)
View(m1)



