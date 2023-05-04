# install.packages("xlsx", dependencies = TRUE)

readLocationData <- function(fileName)
{
    
    if (fileName == "agrovegetal1811.xlsx") 
    {
        writeLines(fileName)
        cols = c("character", rep("integer", 4), rep("character", 9), "integer", "double", rep("integer", 6),
                 rep("double", 2),"integer", "integer", "integer", "double", "double", "integer", "double", "integer", "double", 
                 "double", "integer", "integer", rep("integer", 5))
    }
    if (fileName == "agrovegetal1911.xlsx") 
    {
        writeLines(fileName)
        cols = c("character", rep("integer", 4), rep("character", 9), rep("integer", 5), rep("double", 3),
                 rep("integer", 4), rep("double", 3), rep("integer", 3), "double", "double", "integer", rep("integer", 5))
    }
    if (fileName == "boreal1805.xlsx") 
    {
        writeLines(fileName)
        cols = c("character", rep("integer", 4), rep("character", 3), rep("integer", 4), rep("double", 2), 
                 "integer", "double", "double", "integer", rep("double", 3), "integer", "double", "integer", "integer", "double",
                 rep("character", 6), rep("integer", 5)
                 )
    }
    
    if (fileName == "boreal1906.xlsx") 
    {
        writeLines(fileName)
        cols = c("character", rep("integer", 4), rep("character", 3), rep("integer", 4), "double", rep("integer", 3),
                 rep("double", 5), "integer", "double", "integer", "integer", "double", rep("character", 6), rep("integer", 5))
    }
    
    if (fileName == "ikbks1804.xlsx") 
    {
        writeLines(fileName)
        cols = c("character", rep("integer", 4), rep("character", 3), rep("character", 6), "integer", rep("double", 2),
                 "integer", "double", rep("integer", 10), "double", "double", "integer", "double", rep("integer", 4),
                 rep("double", 4), rep("integer", 6))

    }
    if (fileName == "ikbks1903.xlsx") 
    {
        writeLines(fileName)
        cols = c("character", rep("integer", 4), rep("character", 3), rep("character", 6), "integer", "double", "integer", "integer",
                 "double", "integer", "double", "double", rep("integer", 11), "double", "integer", "integer", rep("double",5),
                 rep("integer", 7))
        
    }
    if (fileName == "ikbks2003.xlsx") 
    {
        writeLines(fileName)
        cols = c("character", rep("integer", 4), rep("character", 3), rep("character", 5), "integer", "double", rep("integer", 3), 
                 rep("double", 2), rep("integer", 11), "double", "integer", "double", "integer", rep("double", 4),
                 "integer","character", rep("integer", 5))
        
        
    }
    if (fileName == "melle1804.xlsx") 
    {
        writeLines(fileName)
        cols = c("character", rep("integer", 4), rep("character", 3), rep("character", 6), rep("integer", 2), "double", rep("integer", 2),
                 "double", "integer", rep("double", 4), "integer", "double", rep("integer", 2), rep("double", 2), rep("integer", 6))
        
    }
    if (fileName == "melle1904.xlsx") 
    {
        writeLines(fileName)
        cols = c("character", rep("integer", 4), rep("character", 3), rep("character", 6), "integer", "double", rep("integer", 4),
                 "double", "integer", rep("double", 4), rep("integer", 2), rep("double", 3), rep("integer", 7))
        
    }
    if (fileName == "allDataForCorrelationsV6.xlsx") 
    {
        writeLines(fileName)
        cols = c("character", "character", "integer", rep("double", 7), "integer")
        
    }

    brojLista = 2
    excelData <- readExcelFajl(brojLista, cols, fileName)
    # excelData$Location = as.factor(excelData$Location)
    # excelData$Row = as.factor(excelData$Row)
    # excelData$Column = as.factor(excelData$Column)
    # excelData$EntryNo = as.factor(excelData$EntryNo)
    # excelData$EUCLEG.ID = as.factor(excelData$EUCLEG.ID)
    str(excelData)
    return(excelData)
}






# Ucitava list sa hemijskim podacima
readHemijaData <- function()
{
    writeLines("HEMIJA")
    brojLista = 3
    cols = c("character", "character", "numeric", "numeric", "numeric")
    hemijaData = readExcelFajl(brojLista, cols)
    hemijaData$Population = as.factor(hemijaData$Population)
    hemijaData$PopDok <- NULL
    print(sapply(hemijaData, class))
    writeLines("")
    print(head(hemijaData))
    writeLines("")
    return(hemijaData)
}

# Ucitava list sa anatomskim osobinama lista
readAnatomijaData <- function()
{
    writeLines("ANATOMIJA LISTA")
    brojLista = 4
    cols = c("character", "character", "integer", "integer", "numeric", "numeric", "numeric", "numeric", "numeric")
    anatomijaData = readExcelFajl(brojLista, cols)
    
    anatomijaData$Leaf = as.factor(anatomijaData$Leaf)
    anatomijaData$Cut = as.factor(anatomijaData$Cut)
    anatomijaData$Population = as.factor(anatomijaData$Population)
    anatomijaData$PopDok <- NULL
    print(sapply(anatomijaData, class))
    writeLines("")
    print(head(anatomijaData))
    writeLines("")
    return(anatomijaData)
}

# Ucitava list sa debljinom centralnog nerva
readCNData <- function()
{
    writeLines("DEBLJINA CENTRALNOG NERVA")
    brojLista = 5
    cols = c("character", "character", "integer", "numeric")
    cnData = readExcelFajl(brojLista, cols)
    
    cnData$Leaf = as.factor(cnData$Leaf)
    cnData$Population = as.factor(cnData$Population)
    cnData$PopDok <- NULL
    print(sapply(cnData, class))
    writeLines("")
    print(head(cnData))
    writeLines("")
    return(cnData)
}

# Ucitava list sa sekundarnim metabolitima
readAntioksData <- function()
{
    writeLines("ANTIOKSIDATIVNA AKTIVNOST")
    brojLista = 6
    cols = c("character", "character", "numeric", "numeric", "numeric")
    antioksData = readExcelFajl(brojLista, cols)
    
    antioksData$Population = as.factor(antioksData$Population)
    antioksData$PopDok <- NULL
    print(sapply(antioksData, class))
    writeLines("")
    print(head(antioksData))
    writeLines("")
    return(antioksData)
}





