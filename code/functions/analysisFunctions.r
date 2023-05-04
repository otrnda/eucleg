
corelationMatrixFull <- function(data)
{
    data$Plot <- NULL
    data$EnvCode <- NULL
    data <- data[,order(colnames(data))]
    data.kor <- cor(data[ , sapply(data, is.numeric)], method = "pearson", use = "complete.obs")
    return (data.kor)
}

correlations <- function(matData, filename, sheetName)
{
    corrAll = rcorr(as.matrix(matData))
    rRound = round(corrAll$r, 4)
    pRound = round(corrAll$P, 4)
    
    l = list("korelacije" = as.data.frame(rRound), "P" = as.data.frame(pRound))
    writeToExcelFile(filename, sheetName, l, writeRowNames = TRUE)
    plot = ggcorr(matData, label = TRUE)
    saveGGplot(plot, sheetName)
    return(corrAll)
    # qplot(correlationData$LB, correlationData$TFC, data = correlationData, geom = c("point", "smooth"), method = "lm", alpha = I(1 / 2), se = FALSE)
}

oneTwoWayAnova <- function(df, dependantVariables, factor1, factor2)
{
    library(rlist)
    rezList = list()
    names = c()
    for(dependantVariable in dependantVariables)
    {
        varName <- names(df)[dependantVariable]
        if(missing(factor2))
        {
            command = paste("aov(" , varName , " ~ ", factor1 , ", data = df)", sep = "")    
        }
        else
        {
            command = paste("aov(" , varName , " ~ ", factor1 , " * ", factor2, ", data = df)", sep = "")    
        }
        rez <- eval(parse(text = command))
        rezs = summary(rez)
        names = append(names, varName)
        rezList = rlist::list.append(rezList, rezs[[1]])
    }
    names(rezList) <- names
    print(rezList)
}

#' racuna H index za kolonu ciji je dat redni broj
#'
#' @param df data.frave
#' @param columnNumbers array kolona ciji Hprim trazimo
#' @param numSegments  broj segmenta
#'
#' @return data.frame

phenoDiverIndex <- function(df, columnNumbers, numSegments = 3)
{
    library(tidyverse)
    names = c()
    hprims = c()
    counts = c()
    for(columnNumber in columnNumbers)
    {
        varName <- names(df)[columnNumber]
        values <- df %>% select(columnNumber)
        values = as.numeric(values[[1]])
        min <- min(values)
        max <- max(values)
        segmentsArr <- rep(0, numSegments + 1)
        segmentsArr[1] <- min
        for(i in 1:numSegments)
        {
            segmentsArr[i + 1] <- min + i * (max - min)/numSegments
        }
        
        countPerSegmentArr <-rep(0, numSegments)
        procPerSegmentArr <-rep(0, numSegments)
        for(elem in values)
        {
            if(elem >= segmentsArr[length(segmentsArr) - 1])
            {
                countPerSegmentArr[numSegments] <- countPerSegmentArr[numSegments] + 1
            }
            else
            {
                for(i in 1: numSegments - 1)
                {
                    
                    donjaGranica = segmentsArr[i]
                    gornjaGranica = segmentsArr[i + 1]
                    if(elem >= donjaGranica && elem < gornjaGranica)
                    {
                        countPerSegmentArr[i] <- countPerSegmentArr[i] + 1
                    }
                }            
            }
        }
        
        for(i in 1:numSegments)
        {
            procPerSegmentArr[i] <- countPerSegmentArr[i] / sum(countPerSegmentArr)
        }
        print(procPerSegmentArr)
        H <- 0
        for(i in 1:numSegments)
        {
            if(procPerSegmentArr[i] > 0)
            {
                H <- H -  procPerSegmentArr[i] * log(procPerSegmentArr[i])    
            }
            
        }
        Hmax <- log(numSegments)
        Hprim <- H / Hmax
        names <- append(names, varName)
        hprims <- append(hprims, Hprim)
        countsStr <- "[ ";
        for(i in 1 : numSegments)
        {
            countsStr <- paste(countsStr, countPerSegmentArr[i])
                
        }
        countsStr = paste(countsStr, " ]");
        
        counts = append(counts, countsStr)
        
    }
    rez <-data.frame(names, hprims, counts)
    print(rez)
    return(rez)
}



#' radi PCA za podateke koji se grupisu po kategoriji koja se zove group. 
#' Rezultati se smestaju u fileName.xlsx i fileName.pdf
#'
#' @param dataAverages matrijca sa prosecinim vrednostima
#' @param numColsToAnalyse prvih koliko colona da se analizira
#' @param fileName naziv fajla 
#'
#' @return NULL
pcaSakategorijamaBezLabela <- function(dataAverages, numColsToAnalyse,  fileName, drugaosa = 2)
{
    # dataAverages <- BLUPS_WITH_BT %>% dplyr::rename(Group = BT)
    # numColsToAnalyse <- 9
    # fileName <- "pcaBotanicalType"
    pcaResult <- prcomp(dataAverages[ ,1:numColsToAnalyse], 
                        retx = TRUE, 
                        center = TRUE, 
                        scale. = TRUE)
    
    summ = summary(pcaResult)
    importance = as.data.frame(summ$importance)
    pcas = as.data.frame(summ$rotation)
    coordinates = as.data.frame(as.data.frame(summ$x)) %>% mutate("Genotype" = dataAverages$GE)
    lista = list(importance, pcas, coordinates)
    inputLista = list(
        "EigenVal, Proc Obj, Kumul Proc" = importance, 
        "Principal Components" = pcas,
        "Point Coordinates" = coordinates
    )
    procPCA1 = round(importance[2,1] * 100, digits = 1)
    procPCA2 = round(importance[2,2] * 100, digits = 1)
    procPCA3 = round(importance[2,3] * 100, digits = 1)
    
    ylab = paste("PCA2 (",procPCA2,"%)", sep = "")
    if(drugaosa == 3)
    {
        ylab = paste("PCA3 (",procPCA3,"%)", sep = "")    
    }
    
    
    grafik <- ggbiplot::ggbiplot(pcaResult, 
                                choices=c(1,drugaosa),  
                                scale = 1, 
                                obs.scale = 1,
                                var.scale = 1,
                                groups = dataAverages$Group, 
                                circle = TRUE,
                                varname.adjust = 1,
                                varname.size = 2.5,
                                
                                ) + 
        geom_point(aes(color=groups, fill=groups), size = 1.5, shape = 21, color = "black") + 
        scale_color_manual(values=c("white", "white", "white", "white", "white", "white", "white")) + 
        scale_fill_manual(values=c("#9ff8fb", "#f6ff8c", "magenta", "black", "#76ff68", "#1532e6", "#4caa50")) + 
        
        geom_point(aes(x = c(0), y = c(0)), size = 1, colour = "darkred")+
        xlab(paste("PCA1 (",procPCA1,"%)", sep = "")) + 
        ylab(ylab) +
        ylim(-4.5, 3.2)+
        theme_bw() +
        theme(legend.position = "bottom",
              legend.text = element_text(size = 10, face="bold"),
              legend.title = element_text(size = 10, face="bold"),
              axis.text = element_text(size = 8, face = "bold"),
              axis.title = element_text(size = 8, face="bold")
              )+
        guides(color = "none",
               fill = guide_legend(nrow = 1, title = "Clusers"))
    
    grafik <- gginnards::move_layers(grafik, "GeomPoint", position = "bottom")
    
    saveGGplot2(dataPlot = grafik, folder = "pca", fileName = fileName, isWide = TRUE, aspect_ratio = 0.75)
    return (grafik)
    #writeToExcelFile(fileName = paste("pca/", fileName, sep = ""), sheetName = "pcaSaKategorijama", inputLista)
}




#' radi PCA za podateke koji se grupisu po nekoj kategoriji. Rezultati se smestaju u 
#' fileName.xlsx i fileName.pdf
#'
#' @param dataAverages matrijca sa prosecinim vrednostima
#' @param fileName kolona po kojoj se vrsi grupisanje
#'
#' @return NULL
pcaSakategorijama <- function(dataAverages, labelsZaBirisanje, fileName)
{
    for (value in labelsZaBirisanje) 
    { 
        dataAverages$EucIdShortMain <- str_replace(dataForPCA$EucIdShortMain, value, "")
    }
    
    pcaResult <- prcomp(dataAverages[ ,2:9], retx = TRUE, center = TRUE, scale. = TRUE)
    summ = summary(pcaResult)
    
    importance = as.data.frame(summ$importance)
    print(importance)
    pcas = as.data.frame(summ$rotation)
    coordinates = as.data.frame(as.data.frame(summ$x))
    lista = list(importance, pcas, coordinates)
    inputLista = list(
        "EigenVal, Proc Obj, Kumul Proc" = importance, 
        "Principal Components" = pcas,
        "Point Coordinates" = coordinates
    )
    procPCA1 = round(importance[2,1] * 100, digits = 2)
    procPCA2 = round(importance[2,2] * 100, digits = 2)
    # writeToExcelFile(fileName, "pcaRezultati", inputLista)
    
    
    grafik = ggbiplot::ggbiplot(pcaResult, 
                       choices=c(1,2),  
                       scale = 1, 
                       obs.scale = 1,
                       groups = dataAverages$PointCategory, 
                       labels = dataAverages$EucIdShortMain, 
                       circle = TRUE,
                       labels.size = 4, 
                       varname.adjust = 1,
                       varname.size = 4) + 
        geom_point(aes(x = pcaResult$x[ ,1], y = pcaResult$x[ ,2] - 0.2), size = 1)+
        xlab(paste("PCA1 (",procPCA1,"%)")) + ylab(paste("PCA1 (",procPCA2,"%)")) +
        theme(legend.position = "bottom") +
        theme(legend.text = element_text(size=12, face="bold")) +
        theme(axis.text = element_text(size=12, face = "bold"), axis.title = element_text(size = 12, face="bold")) +
        scale_color_manual(name = "Year", values = c("orange", "purple")) +
        scale_color_discrete(name = 'Year')
    
    saveGGplot(grafik, fileName)
    grafik
    return(grafik)
}




#' Radi PCA za podatke kod kojih nema grupisanja.
#'
#' @param dataAverages matrijca sa prosecinim vrednostima
#' @param visibleLabels labels koji ce da budu vidiljivi
#' @param labelsShifting pomeranje odredjenih labela
#' @param fileName fajl u koji se upisuju rezultati
#'
#' @return NULL
pcaBezKategorija <- function(dataAverages, visibleLabels, labelsShifting, fileName)
{
    dataAveragesB <- dataAverages
    genOr <- dataAverages$Genotype
    for (x in 1:nrow(dataAverages)) 
    { 
        v <- dataAverages$Genotype[x]
        if(!(v %in% visibleLabels))
        {
            dataAverages$Genotype[x] <- ""
            dataAverages$textColor[x] <- ""
        }
        else
        {
            if(v == "BARACA" || v == "MISTRAL" || v == "FANFARE" || v == "MERKUR" || v == "MERLIN")
            {
             
                dataAverages$textColor[x] <- "red"
                #dataAverages$Genotype[x] <- paste("bold('", dataAverages$Genotype[x],")", sep = "")
            }
        }
    }
    
    pcaResult <- prcomp(dataAverages[ ,1:9], retx = TRUE, center = TRUE, scale. = TRUE)
    summ = summary(pcaResult)
    importance = as.data.frame(summ$importance)
    pcas = as.data.frame(summ$rotation)
    coordinates = as.data.frame(as.data.frame(summ$x)) %>% 
        dplyr::mutate("Genotype" = genOr, "Visible" = dataAverages$Genotype)
    inputLista = list(
        "EigenVal, Proc Obj, Kumul Proc" = importance, 
        "Principal Components" = pcas,
        "Point Coordinates" = coordinates
    )
    procPCA1 = round(importance[2,1] * 100, digits = 1)
    procPCA2 = round(importance[2,2] * 100, digits = 1)
    # writeToExcelFile(fileName, "pcaRezultati", inputLista)
    
    
    blackDotsX <- pcaResult$x[ ,1]
    blackDotsY <- pcaResult$x[ ,2]

    # specijalno oznacene tacke
    specialPointsInd = c(180, 179, 177, 178, 176)
    specialPointsIndX <- rep(0, length(blackDotsX))
    specialPointsIndY <- rep(0.15, length(blackDotsY))
    for (index in specialPointsInd) 
    {
        specialPointsIndX[index] <- blackDotsX[index]
        specialPointsIndY[index] <- blackDotsY[index]
        blackDotsX[index] <- 0;
        blackDotsY[index] <- 0;
    }
    

    # ko ima label ne treba da ima crnu tacku
    for (index in 1:nrow(dataAverages)) 
    { 
        if(dataAverages$Genotype[index] != "")
        {
            blackDotsX[index] <- 0;
            blackDotsY[index] <- 0;
        }
    }
    
    # labels shifting
    if(!is.null(labelsShifting))
    {
        for(i in 1: nrow(labelsShifting))
        {
            labelZaPomeranje = labelsShifting[[1]][[i]]
            pomerajX = labelsShifting[[2]][[i]]
            pomerajY = labelsShifting[[3]][[i]]
            labelIndex <- which(dataAverages$Genotype == labelZaPomeranje)
            if(length(labelIndex) > 0)
            {
                labelIndex <- labelIndex[1]
                pcaResult$x[labelIndex,1] <- pcaResult$x[labelIndex,1] + pomerajX
                pcaResult$x[labelIndex,2] <- pcaResult$x[labelIndex,2] + pomerajY
            }
        }        
    }

    grafik <- ggbiplot::ggbiplot(pcaResult, 
                                choices=c(1,2),  
                                scale = 1, 
                                obs.scale = 1,
                                labels = dataAverages$Genotype, 
                                groups = dataAverages$textColor,
                                circle = TRUE,
                                labels.size = 2.8, 
                                varname.adjust = 1,
                                varname.size = 3) + 
        geom_point(aes(x = blackDotsX, y = blackDotsY), size = 0.5)+
        geom_point(aes(x = c(0), y = c(0)), size = 1.5, colour = "darkred")+
        scale_color_manual(name="Variety", values=c("darkred", "black", "blue")) +
        xlab(paste("PCA1 (", procPCA1, "%)")) +
        ylab(paste("PCA2 (", procPCA2, "%)")) +
        theme_bw() +
        theme(
            legend.position = "none",
            legend.text = element_text(size=10, face="bold"),
            axis.text = element_text(size=10, face = "bold"),
            axis.title = element_text(size = 10, face="bold")
            )
    grafik    
    saveGGplot2(dataPlot = grafik, folder = "pca", fileName = fileName, isWide = TRUE, aspect_ratio = 0.75)
    writeToExcelFile(fileName = paste("pca/", fileName, sep = ""), 
                     sheetName = fileName, list(importance, pcas, coordinates))
    return(pcaResult)
}

