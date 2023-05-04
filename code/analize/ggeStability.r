
require(ggrepel)
require(ggpp)
stabilityPlot <- function(fileNameIndex, plotName = "", saveToFile = FALSE)
{
    folder <- "GGE_Excel_Stability/"
    fileNames <- c( "01_STA_DaysToFlower.xlsx", "01_STA_ME1_DaysToFlower.xlsx", "01_STA_ME2_DaysToFlower.xlsx",
                    
                    "02_STA_DaysToMature.xlsx", "02_STA_ME1_DaysToMature.xlsx", "02_STA_ME2_DaysToMature.xlsx",
                    
                    "03_STA_ME1_PlantHeight.xlsx", "03_STA_ME2_PlantHeight.xlsx", "03_STA_PlantHeight.xlsx",
                    "04_STA_Branching.xlsx", "04_STA_ME1_Branching.xlsx", "04_STA_ME2_Branching.xlsx",
                    "05_STA_HeightFirstPod.xlsx", "05_STA_ME1_HeightFirstPod.xlsx", "05_STA_ME2_HeightFirstPod.xlsx",
                    "06_STA_ME1_PodsPerFlower.xlsx", "06_STA_ME2PodsPerFlower.xlsx", "06_STA_PodsPerFlower.xlsx",
                    
                    "07_STA_ME1_PodsPerNode.xlsx", "07_STA_ME2_PodsPerNode.xlsx", "07_STA_PodsPerNode.xlsx",
                    "08_STAB_ME1_PodLength.xlsx", "08_STAB_ME2_PodLength.xlsx", "08_STAB_PodLength.xlsx",
                    "09_STAB_ME1_SeedsPerPod.xlsx", "09_STAB_ME2_SeedsPerPod.xlsx", "09_STAB_SeedsPerPod.xlsx")

    fileName <- fileNames[fileNameIndex]
    if(plotName == "")
    {
        plotName <- fileName
    }
    mainGenotypes <- data.frame(label = c("G180","G179","G177","G178","G176"), 
                                realName = c("BARACA", "FANFARE", "MERKUR", "MERLIN","MISTRAL"))
    path <- paste(folder,fileName,sep = "")
    genData <- readExcelFajl(1, c("character", "double", "double"), path)
    envData <- readExcelFajl(2, c("character", "double", "double"), path)
    xyAxisData <- readExcelFajl(3, c("double", "double", "double", "double"), path)
    blueAxisData <- readExcelFajl(4, c("double", "double", "double", "double"), path)
    greenLinesData <- readExcelFajl(5, c("double", "double", "double", "double"), path)
    axisPercentageData <- readExcelFajl(6, c("character", "double"), path)
    axisPercentageData$Percentage <- round(axisPercentageData$Percentage, 1)
    
    # genLabelsToDisplay => [label, xShift, yShift]
    genLabelsToDisplay <- readExcelFajl(7, c("character", "double", "double"), path)
    genLabelsNotIncluded <- mainGenotypes %>% 
        filter(!(label %in% as.vector(genLabelsToDisplay$label))) %>% dplyr::select(label) %>% 
        mutate(xShift = as.numeric(NA), yShift = as.numeric(NA))
    colnames(genLabelsNotIncluded) <- colnames(genLabelsToDisplay)
    genLabelsToDisplay <- rbind(genLabelsToDisplay, genLabelsNotIncluded)
    genLabelsToDisplay[is.na(genLabelsToDisplay)] <- 0
    
    
    labelsData <- data.frame(matrix(ncol = 4, nrow = 0))
    colnames(labelsData) <- c("labelName", "x", "y", "type")
    labelsData$x <- as.double(labelsData$x)
    labelsData$y <- as.double(labelsData$y)
    

    # svi iz eviroment data idu u labelsData
    for (i in 1:nrow(envData)) 
    {
        labelsData[nrow(labelsData) + 1,] = append(envData[i, ], "env")
    }
    
    
    
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
    
    # Promeni imena glavnih genotipova u genData
    for (i in 1:nrow(mainGenotypes)) 
    {
        labelsData[labelsData == mainGenotypes[i,1]] <- mainGenotypes[i,2]
    }
    for (i in 1:nrow(labelsData)) 
    {
        if(labelsData[i,1] %in% as.vector(mainGenotypes$realName))
        {
            labelsData[i,4] <- "maingen"
        }
    }
    
    for (i in 1:nrow(labelsData)) 
    {
        if(labelsData[i,4] == "env")
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
    
    
    idealEnv <- data.frame("x" = mean(envData$x, na.rm = TRUE),
                           "y" = mean(envData$y, na.rm = TRUE))
    

    
    ggePlot <- ggplot() + 
        theme_bw() +
        geom_segment(data = xyAxisData, 
                     aes( x = xStart, y = yStart, xend = xEnd, yend = yEnd), 
                     colour = "black", 
                     size = 0.4, 
                     linetype = "solid",
                     alpha = 0
                     ) +
        geom_segment(data = greenLinesData, 
                     aes( x = xStart, y = yStart, xend = xEnd, yend = yEnd), 
                     colour = "#6fd46c", 
                     size = 0.3, 
                     linetype = "dotted") +
        geom_point(data = genData, 
                   mapping = aes(x = x, y = y), 
                   size = 0.1, 
                   color = "black"
                   ) +
        geom_segment(data = blueAxisData, 
                     aes( x = xStart, y = yStart, xend = xEnd, yend = yEnd), 
                     colour = BLUE, 
                     size = 0.4, 
                     linetype = "solid",
                     ) +
        geom_point(data = idealEnv, 
                   mapping = aes(x = x, y = y),
                   shape = 1,
                   size = 1.3, 
                   color = BLUE) +
        geom_segment(data = idealEnv, 
                     aes( x = 0, y = 0, xend = x, yend = y), 
                     colour = BLUE, 
                     size = 0.4, 
                     linetype = "solid",
                     arrow = arrow(length = unit(2, "mm"),
                                   angle = 10,
                                   type = "closed"),
                     arrow.fill = "black"  
        ) +
        geom_text(data = labelsData, 
                  mapping = aes(x = x, y = y, label = labelName, color = type), 
                  size = 1.6,
                  show.legend = FALSE,
                  parse = TRUE,
        ) +
        ggtitle(plotName) +
        coord_fixed(ratio = 1) +
       
        scale_colour_manual(values = c(ORANGE, "black", "red")) +
        xlab(label = paste("AXIS 1: ", axisPercentageData[1,2], "%", sep = "")) +
        ylab(label = paste("AXIS 2: ", axisPercentageData[2,2], "%", sep = "")) +
        theme(
            plot.title = element_text(color="black", size=8, face="plain"),
            axis.text = element_text(size = 6, colour = "black"),
            axis.title = element_text(size = 8),
            plot.background = element_rect(colour = "white"), 
              )
    
    if(saveToFile)
    {
        saveGGplot2(dataPlot = ggePlot, 
                    folder = "stability-plots",
                    fileName = fileName, 
                    isWide = FALSE,
                    aspect_ratio = 1)
    }
        return(ggePlot)
}


p1 <- stabilityPlot(2, "Dayst to flower SE-ME")
p2 <- stabilityPlot(3, "Dayst to flower NE-ME")
p3 <- stabilityPlot(11, "Number of branches SE-ME")
p4 <- stabilityPlot(12, "Number of branches NE-ME")
p5 <- stabilityPlot(13, "Height first pod SE-ME")
p6 <- stabilityPlot(14, "Height first pod NE-ME")
p7 <- stabilityPlot(19, "Pods per node SE-ME")
p8 <- stabilityPlot(20, "Pods per node NE-ME")
p9 <- stabilityPlot(25, "Seeds per pod SE-ME")
p10 <- stabilityPlot(26, "Seeds per pod NE-ME")


mergedPlot <- ggpubr::ggarrange(p7, p8, p9, p10,
                                labels = c("D1", "D2", "E1", "E2"),
                                font.label = list(size = 10, color = "black", face = "bold", family = NULL),
                                ncol = 2, 
                                nrow = 2,
                                common.legend = FALSE,
                                align = 'hv'
) + theme(plot.background = element_rect(fill = "white", colour = "white"))
mergedPlot
saveGGplot2(dataPlot = mergedPlot,
            folder = "stability-plots",
            fileName = "merged-D1-E2",
            isWide = TRUE,
            aspect_ratio = 1)


mergedPlot <- ggpubr::ggarrange(p1, p2, p3, p4, p5, p6,
                                labels = c("A1", "A2", "B1", "B2", "C1", "C2"),
                                font.label = list(size = 10, color = "black", face = "bold", family = NULL),
                                ncol = 2, 
                                nrow = 3, 
                                hjust = -1,
                                vjust = 2,
                                common.legend = FALSE
                                #align = 'hv'
) + theme(plot.background = element_rect(fill = "white", colour = "white"))
mergedPlot
saveGGplot2(dataPlot = mergedPlot,
            folder = "stability-plots",
            fileName = "merged-A1-C2",
            isWide = TRUE,
            aspect_ratio = 1.5)



p1 <- stabilityPlot(4, "Dayst to Mature")
p2 <- stabilityPlot(9, "Plant height")
p3 <- stabilityPlot(18, "Pods per Flower")
p4 <- stabilityPlot(24, "Pod Length")


mergedPlot <- ggpubr::ggarrange(p1, p2, p3, p4,
                                labels = c("A", "B", "C", "D"),
                                font.label = list(size = 10, color = "black", face = "bold", family = NULL),
                                ncol = 2, 
                                nrow = 2, 
                                hjust = -1,
                                vjust = 2,
                                common.legend = FALSE
) + theme(plot.background = element_rect(fill = "white", colour = "white"))
mergedPlot
saveGGplot2(dataPlot = mergedPlot,
            folder = "stability-plots",
            fileName = "merged-A-D",
            isWide = TRUE,
            aspect_ratio = 1)


for(i in 1:27)
{
    stabilityPlot(fileNameIndex = i, plotName = "", saveToFile = TRUE)
}
rm(i, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,mergedPlot)



