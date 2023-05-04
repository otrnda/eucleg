

wwwPlot <- function(fileNameIndex)
{
    folder <- "GGE_Excel_Polygon/"
    fileNames <- c( 
        "01_WWW_DaysToFlower.xlsx",
        "02_WWW_DaysToMature.xlsx",
        "03_WWW_PlantHeight.xlsx",
        "04_WWW_Branching.xlsx",
        "05_WWW_HeightFirstPod.xlsx",
        "06_WWW_PodsPerFlower.xlsx",
        "07_WWW_PodsPerNode.xlsx",
        "08_WWW_PodLenght.xlsx",
        "09_WWW_SeedsPerPod.xlsx")
    
    fileName <- fileNames[fileNameIndex]
    path <- paste(folder,fileName,sep = "")
    mainGenotypes <- data.frame(label = c("G180","G179","G177","G178","G176"), 
                                realName = c("BARACA", "FANFARE", "MERKUR", "MERLIN","MISTRAL"))
    
    genData <- readExcelFajl(1, c("character", "double", "double"), path)
    envData <- readExcelFajl(2, c("character", "double", "double"), path)
    polyData <- readExcelFajl(3, c("character", "double", "double"), path)
    linesData <- readExcelFajl(4, c("double", "double", "double", "double"), path)
    axisData <- readExcelFajl(5, c("character", "double"), path)
    axisData$Percentage <- round(axisData$Percentage, 1)
    
    # genLabelsToDisplay => [label, xShift, yShift]
    genLabelsToDisplay <- readExcelFajl(6, c("character", "double", "double"), path)
    genLabelsNotIncluded <- mainGenotypes %>% 
        dplyr::filter(!(label %in% as.vector(genLabelsToDisplay$label))) %>% 
        dplyr::select(label) %>% 
        dplyr::mutate(xShift = as.numeric(NA), yShift = as.numeric(NA))
    colnames(genLabelsNotIncluded) <- colnames(genLabelsToDisplay)
    genLabelsToDisplay <- rbind(genLabelsToDisplay, genLabelsNotIncluded)
    genLabelsToDisplay[is.na(genLabelsToDisplay)] <- 0
    
    
    labelsData <- data.frame(matrix(ncol = 4, nrow = 0))
    colnames(labelsData) <- c("labelName", "x", "y", "type")
    labelsData$x <- as.double(labelsData$x)
    labelsData$y <- as.double(labelsData$y)
    
    # promeni imane glavnih genotipova u genData i polyData
    for (x in 1:nrow(mainGenotypes)) 
    {
        genData[genData == mainGenotypes[x,1]] <- mainGenotypes[x,2]
        polyData[polyData == mainGenotypes[x,1]] <- mainGenotypes[x,2]
        genLabelsToDisplay[genLabelsToDisplay == mainGenotypes[x,1]] <- mainGenotypes[x,2]
    }
    
    # svi iz eviroment data idu u labels data
    for (i in 1:nrow(envData)) 
    {
        labelsData[nrow(labelsData) + 1,] = append(envData[i, ], "env")
    }
    
    # svi iz poligona idu u labels data
    for (i in 1 : nrow(polyData))
    {
        if(!is.na(polyData$label[i]))
        {
            label = polyData[i, 1]
            x = polyData[i, 2]
            y = polyData[i, 3]
            for(j in 1:nrow(genLabelsToDisplay))
            {
                if(label == genLabelsToDisplay[j,1])
                {
                    x = x + genLabelsToDisplay[j,2]/10
                    y = y + genLabelsToDisplay[j,3]/10                   
                }
            }
            labelsData[nrow(labelsData) + 1,] <- c(label, x, y, "pol")  
        }
    }
    
    labelsDataBack <- labelsData
    
    # svi iz genLabelsToDisplay idu u labels data a izbacaju se iz points data
    for(i in 1:nrow(genLabelsToDisplay))
    {
        row <- genLabelsToDisplay[i, ]
        gen <- row[1,1]
        pos <- match(gen, genData$label)
        
        if(!is.na(pos))
        {
            if(is.na(match(gen, labelsData$labelName)))
            {
                if(gen %in% mainGenotypes$realName)
                {
                    type = 'maingen'
                }
                else
                {
                    type = 'gen'
                }
                toAppend <- c(genData[pos,1], genData[pos,2] + row[1,2]/10, genData[pos,3] + row[1,3]/10, type)
                labelsData[nrow(labelsData) + 1,] <- toAppend
            }
            genData <- genData[- c(pos), ]
        }
    }
    

    for (i in 1:nrow(labelsData)) 
    {
        if(labelsData[i,4] == "env")
        {
            labelsData[i,1] = paste("bold('",labelsData[i,1],"')", sep = "");
        }
        if(labelsData[i,4] == "pol")
        {
            labelsData[i,1] = paste("bold('",labelsData[i,1],"')", sep = "");
        }
    }
    for (i in 1:nrow(labelsData)) 
    {
        l = labelsData[i,1]
        if(l == "BARACA" | l == "FANFARE" | l == "MERKUR" | l == "MERLIN" | l == "MISTRAL")
        {
            labelsData[i,1] = paste("bold('",l,"')", sep = "");
        }
    }
    labelsData <- labelsData %>% arrange(type)
    labelsData$x <- as.double(labelsData$x)
    labelsData$y <- as.double(labelsData$y)
    
    wwwPlot <- ggplot() + 
        theme_bw()+
        geom_point(data = genData, mapping = aes(x = x, y = y), size = 0.2, color = "black") +
        geom_path(data = polyData, mapping = aes(x = x, y = y), linewidth = 0.4, color = "black") +
        geom_point(data = polyData, mapping = aes(x = x, y = y), size = 0.4, color = BLUE) +
        geom_segment(data = linesData, 
                     aes( x = xStart, y = yStart, xend = xEnd, yend = yEnd), 
                     colour = BLUE, size = 0.3, linetype = "dashed") +
        geom_text(data = labelsData, 
                  mapping = aes(x = x, y = y, label = labelName, color = type), 
                  size = 1.7,  
                  show.legend = FALSE,
                  parse = TRUE
        ) +
        coord_fixed(ratio = 1) +
        scale_colour_manual(values = c(ORANGE, "black", "red", BLUE)) +
        ggtitle(VAR_NAMES[fileNameIndex]) +
        xlab(label = paste("AXIS 1: ", axisData[1,2], "%", sep = "")) +
        ylab(label = paste("AXIS 2: ", axisData[2,2], "%", sep = '')) +
        theme(plot.title = element_text(color="black", size=8, face="plain"),
              axis.text = element_text(size = 6, colour = "black"),
              axis.title = element_text(size = 6),
              plot.background = element_rect(colour = "white"), 
              )
        
    
    return(wwwPlot)
    # saveGGplot2(dataPlot = wwwPlot,
    #              folder = "www-plots",
    #              fileName = fileNames[fileNameIndex],
    #              isWide = FALSE,
    #              aspect_ratio = 1)
}

p1 <- wwwPlot(1) 
p2 <- wwwPlot(2)
p3 <- wwwPlot(3)
p4 <- wwwPlot(4)
p5 <- wwwPlot(5)
p6 <- wwwPlot(6)
p7 <- wwwPlot(7)
p8 <- wwwPlot(8)
p9 <- wwwPlot(9)

mergedPlot <- ggpubr::ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9,
          labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I"),
          font.label = list(size = 8, color = "black", face = "bold", family = NULL),
          ncol = 3, 
          nrow = 3, 
          hjust = -1,
          vjust = 2,
          common.legend = FALSE
          ) + theme(plot.background = element_rect(fill = "white", colour = "white"))

mergedPlot

saveGGplot2(dataPlot = mergedPlot,
            folder = "www-plots",
            fileName = "www-merged",
            isWide = TRUE,
            aspect_ratio = 1.3)

for(i in 1:9)
{
    wwwPlot(i)
}
rm(i, mergedPlot, p1, p2, p3, p4, p5, p6, p7, p8, p9)






