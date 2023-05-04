


#' Nalazi srednje vrednosti date osobine i smesta ih u eksel. U vrhu su Environments
#' a u vertikalno stoje genotipovi. Rezultae upisuje u ekel koji ima naziv osobine
#'
#' @param dfList lista sa lokalitetima (1 lok = 1 df)
#' @param trialName  osobina koju ekstrahujemo. Moze i da se ne nalazi u svim df-ovima
#'
#' @return
#' @export
#'
#' @examples
trialAverages <- function(dfList, trialName)
{
    oneTrialData <- NULL
    allTrialData <- NULL
    for(data in dfList)
    {
        oneTrialData <- NULL
        if(trialName %in% colnames(data))
        {
            oneTrialData <- cbind(as.data.frame(data[ ,"Environment"]))
            oneTrialData <- cbind(oneTrialData, as.data.frame(data[ ,"EUCLEG.ID"]))
            oneTrialData <- cbind(oneTrialData, data[ ,match(trialName, colnames(data))])
            colnames(oneTrialData) <- c("Environment", "EUCLEG.ID", trialName)
        }
        allTrialData <- rbind(allTrialData, oneTrialData)
    }
    allTrialDataAvg <- dataAveragers(allTrialData, Environment, EUCLEG.ID)
    allTrialDataAvgWide <- dcast(allTrialDataAvg, EUCLEG.ID ~ Environment, value.var=trialName)
    allTrialDataAvgWide <- allTrialDataAvgWide[order(allTrialDataAvgWide$EUCLEG.ID),]
    rownames(allTrialDataAvgWide) <- as.vector(allTrialDataAvgWide$EUCLEG.ID)
    allTrialDataAvgWide$EUCLEG.ID <- NULL
    
    
    ggeTrialData <- trialForGGE(dfList, trialName)
    ggeTrialData$Row <- as.numeric(as.character(ggeTrialData$Row))
    ggeTrialData$Column <- as.numeric(as.character(ggeTrialData$Column))
    ggeTrialData$EntryNo <- as.numeric(as.character(ggeTrialData$EntryNo))
    writeToExcelFile("TAV", substr(trialName,1,30), list(allTrialDataAvgWide), TRUE)
    writeToExcelFile("GGE", paste(substr(trialName,1,26),sep = "-"), list(ggeTrialData), FALSE)
    return(allTrialDataAvgWide)
}


#' uprosecuje podatke po zadatima kolonama
#'
#' @param data data frame sa podacima
#' @param COL1 prva kolona po kojoj se traze proseci
#' @param COL2 druga kolona po kojoj se traze proseci (moze da se izostavi)
#'
#' @return data framve
dataAveragers <- function(data, COL1, COL2)
{
    if(!missing(COL1) && missing(COL2) )
    {
        dataAveragers <- data %>% group_by({{ COL1 }}) %>% summarise_all(list(mean))    
    }
    if(!missing(COL1) && !missing(COL2) )
    {
        dataAveragers <- data %>% group_by({{ COL1 }}, {{ COL2 }}) %>% summarise_all(list(mean))    
    }
    return (dataAveragers)
}


#' racuna sve deskriptivnu statistiku za dati niz podataka.
#'
#' @param data data.frame ex: AG18
#' @param variables koje promeljive da racuna ex: c("Branching", "Plant H")
#' @param g1 prvi kriterijum grupisanja ex: 
#' @param g2 drugi kriterijum grupisanja (moze da bude prazan)
#'
#' @return data.frame
deskriptivnaStatistika <- function(data, variables, g1, g2) 
{
    # data <- AG18
    # variables <- c("Branching", "PlantHeight")
    if(missing(g1) && missing(g2))
    {
        tmp <- data %>% group_by()
    }
    if(!missing(g1) && missing(g2))
    {
        tmp <- data %>% group_by({{ g1 }})    
    }
    if(!missing(g1) && !missing(g2))
    {
        tmp <- data %>% group_by({{ g1 }}, {{ g2 }})
    }
    
    tmp <- data %>% group_by()
    mergedRez = data.frame(Osobina = c("n", "mean", "sd", "cv", "se", "min", "max", "range", 'median', "Q1", "Q3", "IQR"))
    
    for(j in 1:length(variables))
    {
        varName = variables[j]
        rezTmp <- tmp %>% 
            dplyr::summarise( 
                n = n(),
                mean = mean(.data[[ varName ]], na.rm = TRUE),
                sd = sd(.data[[ varName ]], na.rm = TRUE),
                cv = koeficijent_varijacije(.data[[ varName ]]),
                se = standard_error(.data[[ varName ]]),
                min = min(.data[[ varName ]], na.rm = TRUE),
                max = max(.data[[ varName ]], na.rm = TRUE),
                range = max(.data[[ varName ]], na.rm = TRUE) - min(.data[[ varName ]], na.rm = TRUE),
                median = median(.data[[ varName ]], na.rm = TRUE),
                Q1 = quantile(.data[[ varName ]] , 0.25, na.rm = TRUE),
                Q3 = quantile(.data[[ varName ]] , 0.75, na.rm = TRUE),
                IQR = quantile(.data[[ varName ]] , 0.75, na.rm = TRUE) - quantile(.data[[ varName ]] , 0.25, na.rm = TRUE)
            )
        toMerge <- as.data.frame(t(rezTmp[1,]))
        colnames(toMerge) <- c(varName)
        mergedRez <- cbind(mergedRez, toMerge)
    }
    mergedRez <- as.data.frame(mergedRez)
    return(mergedRez)
}




#' U data frame skuplja sve NUMERICKE osobine koije su merene na svim lokalitetima.
#' U drugi df upisje za svaku osobinu broj lokaliteta na kojoj je merena. 
#'
#' @param listOfDF lista podataka sa svih lokaliteta
#'
#' @return list(data.frame, tabela sa izvestajem za svaku osobinu)
mrgeByAllTirals <- function(listOfDF)
{
    partialTrials <- c()
    allTrials <- c()
    for(df in listOfDF)
    {
        allTrials <- c(allTrials, colnames(df))
    }
    allTrialsDF <- as.data.frame(table(allTrials))
    
    result <- NULL
    
    for(df in listOfDF)
    {
        toMerge <- df %>% select(Environment, Row, Column, EucIdShort, EUCLEG.ID, EntryNo, Accession.name)
        result <- rbind(result, toMerge)   
    }
    for(trialRow in c(1:nrow(allTrialsDF)))
    {
        trialName = as.character(allTrialsDF[trialRow, 1])
        trialCount = as.integer(allTrialsDF[trialRow, 2])
        
        if(trialCount == 9)
        {
            rowToMerge <- NULL
            for(df in listOfDF)
            {
                if(class(df[[trialName]]) == "integer" || class(df[[trialName]]) == "numeric")
                {
                    rowToMerge <- rbind(rowToMerge, as.data.frame(df[[trialName]]))
                }
            }
            if(!is.null(rowToMerge))
            {
                colnames(rowToMerge) <- c(trialName)
                result <- cbind(result, rowToMerge)
            }
        }
        else
        {
            partialTrials <- c(partialTrials, trialName)
        }
        
    }
    return (list(result, allTrialsDF))
}

trialForGGE <- function(listOfDF, trialName)
{
    curResult <- NULL
    for(df in listOfDF)
    {
        if(trialName %in% colnames(df))
        {
            curDF <- df %>% select(Environment, Row, Column, EucIdShort, EUCLEG.ID, EntryNo, Accession.name, trialName)
            {
                curResult <- rbind(curResult, curDF)
            }
        }
    }
    curResult <- curResult[order(curResult$EUCLEG.ID, curResult$Environment),]
    return(curResult)
}

mrgeByCommonTrial <- function(listOfDF, colName)
{
    curResult <- NULL
    for(df in listOfDF)
    {
        if(class(df[[colName]]) == "integer" || class(df[[colName]]) == "numeric")
        {
            
            if(colName %in% colnames(df))
            {
                curDF <- df %>% select(Environment, Row, Column, EucIdShort, EUCLEG.ID, colName)
                if(is.null(curResult))
                {
                    curResult <- curDF
                }
                else
                {
                    curResult <- rbind(curResult, curDF)   
                }
            }
            else
            {
                
            }
        }
        else
        {
              
        }
    }
    
    return(curResult);
}




#' ucitava zadati list eksel fajla u data frame. Putanja se zadaje u funkciji.
#'
#' @param redniBrojLista redni broj lista u eksel fajlu
#' @param cols tip podatka po kolonama na prime c("character", "integer", "numeric")
#' @param fileName naziv fajla
#'
#' @return data.frame
#' 
readExcelFajl <- function(redniBrojLista, cols, fileName) 
{
    path = paste(PODACI_FOLDER,fileName, sep = "")
    rez <- xlsx::read.xlsx(file = path, sheetIndex = redniBrojLista, header = TRUE, 
                           colClasses=cols, stringsAsFactors = FALSE)
    return(rez)
    #rez <- rez[rowSums(is.na(rez)) != ncol(rez), ]
    #rez <- na.omit(rez)
    return(rez)
}


resultsFolderCreator <- function(baseFolder)
{
    ind <- 1
    
    tmpFolder = paste(baseFolder, "V01", sep = "")
    while(file.exists(tmpFolder))
    {
        ind <- ind + 1
        nulaStr <- "0"
        if(ind > 9)
        {
            nulaStr <- ""
        }
        tmpFolder = paste(baseFolder, "V", nulaStr, ind, sep = "")
    }
    dir.create(tmpFolder)
    tmpFolder = paste(tmpFolder, "/", sep = "")
    return(tmpFolder)
}


#' ggplot grafike snama u fajl. Folder (REZULTATI_FOLDER) je eksterna promenjljiva.
#' Ako fajl postoji onda se vrsi indeksiranje.
#'
#' @param dataPlot 
#' @param fileName 
#'
#' @return NULL
saveGGplot2 <- function(dataPlot, folder = "", fileName, isWide, aspect_ratio)
{
    # PROMENITI NA TIFF ZA SLANJE
    i <- 0
    if(folder != "")
    {
        folder = paste(folder, "/")
    }
    fulFileName = paste(REZULTATI_FOLDER, folder, fileName, ".png", sep = "")
    while(file.exists(fulFileName))
    {
        i  <- i + 1
        fulFileName = paste(REZULTATI_FOLDER, fileName, "-", i, ".png" ,sep = "")
    }

    w = 85
    if(isWide)
    {
        w = 170
    }
    if(aspect_ratio > 0)
    {
        ggsave(fulFileName, 
               plot = dataPlot, 
               device = "png",
               height = round(w * aspect_ratio,0), 
               width = w, 
               dpi = 300,
               units = "mm")        
    }
    else
    {
        ggsave(fulFileName, 
               plot = dataPlot, 
               device = "png",
               width = w, 
               dpi = 300,
               units = "mm")        
    }
}

#' lsitu ucitava u eksel. Ako postoji dati fajl sa datim nazivom lista onda javlja gresku.
#'
#' @param fileName naziv eksel fajla (folder je eksterna promenjljiva)
#' @param sheetName naziv lista u koji da se upise
#' @param df_list lista sa podacima
#'
#' @return NULL
writeToExcelFile <- function(fileName, sheetName, df_list, writeRowNames = FALSE)
{
    fileName = paste(REZULTATI_FOLDER, fileName, ".xlsx", sep = "")

    
    if( file.exists(fileName))
    {
        wb <- loadWorkbook(fileName)
    }
    else
    {
        wb <- createWorkbook()
    }
    if(file.exists(fileName))
    {
        if(sheetName %in% openxlsx::getSheetNames(fileName))
        {
            openxlsx::removeWorksheet(wb, sheetName)
        }        
    }

    openxlsx::addWorksheet(wb, sheetName)
    curr_row <- 1
    for(i in seq_along(df_list)) 
    {
        currDF = as.data.frame(df_list[[i]])
        if(writeRowNames)
        {
            currDF = cbind(row.names(currDF), currDF)
            names(currDF)[1] <- "Factor"
        }
        
        writeData(wb, sheetName, t(colnames(currDF)), startCol = 1, startRow = curr_row )
        writeData(wb, sheetName, currDF, startCol = 1, startRow = curr_row+1)
        curr_row <- curr_row + nrow(currDF) + 5
    }
    
    saveWorkbook(wb, fileName, overwrite = TRUE)
    print(paste(fileName, " IS CREATED"))
}


#' racuna sve deskriptivnu statistiku za dati niz podataka.
#'
#' @param data data.frame ex: AG18
#' @param expr koju promeljivu da racuna ex: "Branching"
#' @param g1 prvi kriterijum grupisanja ex: 
#' @param g2 drugi kriterijum grupisanja (moze da bude prazan)
#'
#' @return data.frame
deskriptivnaStatistika2 <- function(data, cols, g1, g2) 
{
    if(missing(g1) && missing(g2))
    {
        tmp <- data %>% group_by()
    }
    if(!missing(g1) && missing(g2))
    {
        tmp <- data %>% group_by({{ g1 }})    
    }
    if(!missing(g1) && !missing(g2))
    {
        tmp <- data %>% group_by({{ g1 }}, {{ g2 }})
    }
    
    rez <- tmp %>% 
        dplyr::summarise(
            "C0" = "mean", 
            "C1" = mean(.data[[cols[1]]], na.rm = TRUE), 
            "C2" = mean(.data[[cols[2]]], na.rm = TRUE), 
            "C3" = mean(.data[[cols[3]]], na.rm = TRUE), 
            "C4" = mean(.data[[cols[4]]], na.rm = TRUE),
            "C5" = mean(.data[[cols[5]]], na.rm = TRUE), 
            "C6" = mean(.data[[cols[6]]], na.rm = TRUE),
            "C7" = mean(.data[[cols[7]]], na.rm = TRUE), 
            "C8" = mean(.data[[cols[8]]], na.rm = TRUE),
            "C9" = mean(.data[[cols[9]]], na.rm = TRUE),
        )
    
    rez = rbind(rez, tmp %>% 
                    dplyr::summarise(
                        "C0" = "sd",
                        "C1" = sd(.data[[cols[1]]], na.rm = TRUE), 
                        "C2" = sd(.data[[cols[2]]], na.rm = TRUE), 
                        "C3" = sd(.data[[cols[3]]], na.rm = TRUE),
                        "C4" = sd(.data[[cols[4]]], na.rm = TRUE),
                        "C5" = sd(.data[[cols[5]]], na.rm = TRUE),
                        "C6" = sd(.data[[cols[6]]], na.rm = TRUE),
                        "C7" = sd(.data[[cols[7]]], na.rm = TRUE),
                        "C8" = sd(.data[[cols[8]]], na.rm = TRUE),
                        "C9" = sd(.data[[cols[9]]], na.rm = TRUE),
                    ))
    rez = rbind(rez, tmp %>% 
                    dplyr::summarise(
                        "C0" = "cv",
                        "C1" = koeficijent_varijacije(.data[[cols[1]]]), 
                        "C2" = koeficijent_varijacije(.data[[cols[2]]]), 
                        "C3" = koeficijent_varijacije(.data[[cols[3]]]),
                        "C4" = koeficijent_varijacije(.data[[cols[4]]]),
                        "C5" = koeficijent_varijacije(.data[[cols[5]]]),
                        "C6" = koeficijent_varijacije(.data[[cols[6]]]),
                        "C7" = koeficijent_varijacije(.data[[cols[7]]]),
                        "C8" = koeficijent_varijacije(.data[[cols[8]]]),
                        "C9" = koeficijent_varijacije(.data[[cols[9]]]),
                    ))
    rez = rbind(rez, tmp %>% 
                    dplyr::summarise(
                        "C0" = "se",
                        "C1" = standard_error(.data[[cols[1]]]), 
                        "C2" = standard_error(.data[[cols[2]]]), 
                        "C3" = standard_error(.data[[cols[3]]]),
                        "C4" = standard_error(.data[[cols[4]]]),
                        "C5" = standard_error(.data[[cols[5]]]),
                        "C6" = standard_error(.data[[cols[6]]]),
                        "C7" = standard_error(.data[[cols[7]]]),
                        "C8" = standard_error(.data[[cols[8]]]),
                        "C9" = standard_error(.data[[cols[9]]]),
                    ))  
    rez = rbind(rez, tmp %>% 
                    dplyr::summarise(
                        "C0" = "min",
                        "C1" = min(.data[[cols[1]]]), 
                        "C2" = min(.data[[cols[2]]]), 
                        "C3" = min(.data[[cols[3]]]),
                        "C4" = min(.data[[cols[4]]]),
                        "C5" = min(.data[[cols[5]]]),
                        "C6" = min(.data[[cols[6]]]),
                        "C7" = min(.data[[cols[7]]]),
                        "C8" = min(.data[[cols[8]]]),
                        "C9" = min(.data[[cols[9]]]),
                    ))
    rez = rbind(rez, tmp %>% 
                    dplyr::summarise(
                        "C0" = "max",
                        "C1" = max(.data[[cols[1]]]), 
                        "C2" = max(.data[[cols[2]]]), 
                        "C3" = max(.data[[cols[3]]]),
                        "C4" = max(.data[[cols[4]]]),
                        "C5" = max(.data[[cols[5]]]),
                        "C6" = max(.data[[cols[6]]]),
                        "C7" = max(.data[[cols[7]]]),
                        "C8" = max(.data[[cols[8]]]),
                        "C9" = max(.data[[cols[9]]]),
                    ))
    rez = rbind(rez, tmp %>% 
                    dplyr::summarise(
                        "C0" = "range",
                        "C1" = max(.data[[cols[1]]]) - min(.data[[cols[1]]]), 
                        "C2" = max(.data[[cols[2]]]) - min(.data[[cols[2]]]), 
                        "C3" = max(.data[[cols[3]]]) - min(.data[[cols[3]]]), 
                        "C4" = max(.data[[cols[4]]]) - min(.data[[cols[4]]]), 
                        "C5" = max(.data[[cols[5]]]) - min(.data[[cols[5]]]), 
                        "C6" = max(.data[[cols[6]]]) - min(.data[[cols[6]]]), 
                        "C7" = max(.data[[cols[7]]]) - min(.data[[cols[7]]]), 
                        "C8" = max(.data[[cols[8]]]) - min(.data[[cols[8]]]), 
                        "C9" = max(.data[[cols[9]]]) - min(.data[[cols[9]]]), 
                    ))
    rez = rbind(rez, tmp %>% 
                    dplyr::summarise(
                        "C0" = "median",
                        "C1" = median(.data[[cols[1]]]), 
                        "C2" = median(.data[[cols[2]]]), 
                        "C3" = median(.data[[cols[3]]]),
                        "C4" = median(.data[[cols[4]]]),
                        "C5" = median(.data[[cols[5]]]),
                        "C6" = median(.data[[cols[6]]]),
                        "C7" = median(.data[[cols[7]]]),
                        "C8" = median(.data[[cols[8]]]),
                        "C9" = median(.data[[cols[9]]]),
                    ))
    rez = rbind(rez, tmp %>% 
                    dplyr::summarise(
                        "C0" = "Q1",
                        "C1" = quantile(.data[[cols[1]]], 0.25, na.rm = TRUE), 
                        "C2" = quantile(.data[[cols[2]]], 0.25, na.rm = TRUE),  
                        "C3" = quantile(.data[[cols[3]]], 0.25, na.rm = TRUE), 
                        "C4" = quantile(.data[[cols[4]]], 0.25, na.rm = TRUE), 
                        "C5" = quantile(.data[[cols[5]]], 0.25, na.rm = TRUE), 
                        "C6" = quantile(.data[[cols[6]]], 0.25, na.rm = TRUE), 
                        "C7" = quantile(.data[[cols[7]]], 0.25, na.rm = TRUE), 
                        "C8" = quantile(.data[[cols[8]]], 0.25, na.rm = TRUE), 
                        "C9" = quantile(.data[[cols[9]]], 0.25, na.rm = TRUE), 
                    ))
    rez = rbind(rez, tmp %>% 
                    dplyr::summarise(
                        "C0" = "Q3",
                        "C1" = quantile(.data[[cols[1]]], 0.75, na.rm = TRUE), 
                        "C2" = quantile(.data[[cols[2]]], 0.75, na.rm = TRUE),  
                        "C3" = quantile(.data[[cols[3]]], 0.75, na.rm = TRUE), 
                        "C4" = quantile(.data[[cols[4]]], 0.75, na.rm = TRUE), 
                        "C5" = quantile(.data[[cols[5]]], 0.75, na.rm = TRUE), 
                        "C6" = quantile(.data[[cols[6]]], 0.75, na.rm = TRUE), 
                        "C7" = quantile(.data[[cols[7]]], 0.75, na.rm = TRUE), 
                        "C8" = quantile(.data[[cols[8]]], 0.75, na.rm = TRUE), 
                        "C9" = quantile(.data[[cols[9]]], 0.75, na.rm = TRUE), 
                    ))
    colnames(rez) <- c("Osobina", cols)
    return(rez)
}





#' Racuna standardnu gresku
#'
#' @param x niz brojeve
#'
#' @return float
standard_error <- function(x) 
{
    return (sd(x, na.rm = TRUE) / sqrt(length(x)) )
}


#' Racuna koeficijent varijacije za dati niz
#'
#' @param x niz brojeva
#'
#' @return float
koeficijent_varijacije <- function(x) 
{
    return ( sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100) 
}



