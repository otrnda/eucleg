
folder <- "GGE_Excel_Polygon/"
fileNames <- c(
    "01_DaysToFlower.xlsx", "02_Branching.xlsx", "03_PlantHeight.xlsx",
    "04_HeightFirstPod.xlsx", "05_PodsPerNode.xlsx", "06_PodsPerFlower.xlsx",
    "07_PodLenght.xlsx","08_SeedsPerPod.xlsx","09_DaysToMature.xlsx")

fileName <- fileNames[1]

path <- paste(folder,fileName,sep = "")
mainGenotypes <- data.frame(codeName = c("E180","E179","E177","E178","E176"), 
                            realName = c("BARACA", "FANFARE", "MERKUR", "MERLIN","MISTRAL"))

genData <- readExcelFajl(1, c("character", "double", "double"), path)
envData <- readExcelFajl(2, c("character", "double", "double"), path)
polyData <- readExcelFajl(3, c("character", "double", "double"), path)
linesData <- readExcelFajl(4, c("double", "double", "double", "double"), path)
axisData <- readExcelFajl(5, c("character", "double"), path)
axisData$Percentage <- round(axisData$Percentage, 1)


envLabelsToDisplay <- readExcelFajl(6, c("character"), path) %>% na.omit(envLabelsToDisplay)
envLabelsToDisplay <- c(envLabelsToDisplay$visible.genotypes, mainGenotypes$realName)
envLabelsToDisplay

labelsData <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(labelsData) <- c("labelName", "x", "y", "type")


# promeni imane glavnih genotipova
for (x in 1:nrow(mainGenotypes)) 
{
    genData[genData == mainGenotypes[x,1]] <- mainGenotypes[x,2]
    polyData[polyData == mainGenotypes[x,1]] <- mainGenotypes[x,2]
}
# svi iz eviroment data idu u labels data
for (x in 1:nrow(envData)) 
{
    labelsData[nrow(labelsData) + 1,] = append(envData[x, ], "env")
}
# svi iz poligona idu u labels data
for (x in 1 : (nrow(polyData)-1)) 
{
    labelsData[nrow(labelsData) + 1,] <- append(polyData[x, ], "pol")
}

# svi iz envLabelsToDisplay idu u labels data a izbacaju se iz points data
for(value in envLabelsToDisplay)
{
    pos <- match(value, genData[ , 1])
    if(!is.na(pos))
    {
        if(is.na(match(value, labelsData[ , 1])))
        {
            
            if(value %in% mainGenotypes$realName)
            {
                labelsData[nrow(labelsData) + 1,] <- append(genData[pos, ], 'maingen')    
            }
            else
            {
                labelsData[nrow(labelsData) + 1,] <- append(genData[pos, ], 'gen')
            }
            
        }
        genData <- genData[- c(pos), ]
    }
}
labelsData
labelsData2 <- labelsData
labelsData2$labelName[labelsData2$type == 'pol'] <- paste("bold(", labelsData2$labelName,")", sep = "")
labelsData2$labelName[labelsData2$type == 'env'] <- paste("bold(", labelsData2$labelName,")", sep = "")
labelsData2
wwwPlot <- ggplot() + 
    theme_bw()+
    geom_point(data = genData, mapping = aes(x = x, y = y), size = 2, color = "black") +
    geom_path(data = polyData, mapping = aes(x = x, y = y), size = 1.1, color = "#333333") +
    geom_point(data = polyData, mapping = aes(x = x, y = y), size = 2, color = BLUE) +
    geom_segment(data = linesData, 
                 aes( x = xStart, y = yStart, xend = xEnd, yend = yEnd), 
                 colour = BLUE, size = 1.5, linetype = "dashed") +
    geom_vline(xintercept = 0, size = 1) +
    geom_hline(yintercept = 0, size = 1) +
    coord_fixed(ratio = 1) +
    geom_text_repel(data = labelsData2, 
                    mapping = aes(x = x, y = y, label = labelName, color = type), 
                    size = 7,  
                    show.legend = FALSE,
                    parse = TRUE
                    ) +
    scale_colour_manual(values = c(ORANGE, "black", BLUE, "purple"),) +
    
    xlab(label = paste("AXIS 1 : ", axisData[1,2], "%", sep = "")) +
    ylab(label = paste("AXIS 2 : ", axisData[2,2], "%", sep = '')) +
    theme(axis.text = element_text(size = 20, colour = "black"),
          axis.title = element_text(size = 20)); saveGGplot(wwwPlot, fileName, isSquare = TRUE)
source("functions/commonFunctions.r")







