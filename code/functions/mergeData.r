
source(paste(getwd(), "functionsEucleg/commonFunctions.r",sep = "/"))
rezList <- mrgeByAllTirals(allDataList)

merged9 <- rezList[[1]]
countData <- rezList[[2]]
writeToExcelFile("mergedData", "Report", rezList[2], TRUE)
writeToExcelFile("mergedData", "9 Trials", rezList[1], FALSE)

for(row in c(1:nrow(countData)))
{
    varName = as.character(countData[row,1])
    cnt = as.integer(countData[row,2])
    comTrial <- NULL
    if(cnt < 9)
    {
        comTrial <- mrgeByCommonTrial(allDataList, varName)
        writeToExcelFile("mergedData", varName, list(comTrial),FALSE)
    }
}

