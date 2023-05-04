
# setwd("d:/work/statistika/Eucleg/analize")
# podaciFolder = "d:/work/statistika/Eucleg/data/GGE_Excel_Stability/"
# source("d:/work/statistika/Eucleg/analize/loader.r")
# rezultatiFolder = resultsFolderCreator("d:/work/statistika/Eucleg/results/")

fileNames <- c("GGE_ME1_X05a_Flow_days.xlsx","GGE_ME1_X08_Branching.xlsx",
"GGE_ME1_X09_Plant_height.xlsx","GGE_ME1_X17_Height_first_pod.xlsx","GGE_ME1_X18_Pods_node.xlsx",
"GGE_ME1_X18a_PodsPerFlower.xlsx","GGE_ME1_X19_Pod_lenght.xlsx","GGE_ME1_X20_Seeds_pod.xlsx","GGE_ME1_X23a_Matur_days.xlsx",
"GGE_ME2_X05a_Flow_days.xlsx","GGE_ME2_X08_Branching.xlsx","GGE_ME2_X09_Plant_height.xlsx",
"GGE_ME2_X17_Height_first_pod.xlsx","GGE_ME2_X18_Pods_node.xlsx","GGE_ME2_X18a_PodsPerFlower.xlsx",
"GGE_ME2_X19_Pod_lenght.xlsx","GGE_ME2_X20_Seeds_pod.xlsx","GGE_ME2_X23a_Matur_days.xlsx")
fileName <- fileNames[18]
mainGenotypes <- data.frame(codeName = c("E180","E179","E177","E178","E176"), 
                            realName = c("BARACA", "FANFARE", "MERKUR", "MERLIN","MISTRAL"))

genData <- readExcelFajl(1, c("character", "double", "double"), fileName)
envData <- readExcelFajl(2, c("character", "double", "double"), fileName)
xyAxisData <- readExcelFajl(3, c("double", "double", "double", "double"), fileName)
blueAxisData <- readExcelFajl(4, c("double", "double", "double", "double"), fileName)
greenLinesData <- readExcelFajl(5, c("double", "double", "double", "double"), fileName)
axisPercentageData <- readExcelFajl(6, c("character", "double"), fileName)
envLabelsToDisplay <- readExcelFajl(7, c("character"), fileName)
envLabelsToDisplay <- c(envLabelsToDisplay$visible.genotypes , mainGenotypes$realName)


allLabelsData <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(allLabelsData) <- c("labelName", "x", "y", "type")


# Upisi imena glavnih genotipova u genData
for (x in 1:nrow(mainGenotypes)) 
{
    genData[genData == mainGenotypes[x,1]] <- mainGenotypes[x,2]
}


# svi iz eviroment data idu u allLabelsData
for (x in 1:nrow(envData)) 
{
    allLabelsData[nrow(allLabelsData) + 1,] = append(envData[x, ], "env")
}



# svi iz envLabelsToDisplay idu u allLabelsData a izbacaju se iz genData
for(value in envLabelsToDisplay)
{
    posGenData <- match(value, genData[ ,1])
    if(!is.na(posGenData))
    {
        if(is.na(match(value, allLabelsData[ ,1])))
        {
            
            if(value %in% mainGenotypes$realName)
            {
                allLabelsData[nrow(allLabelsData) + 1,] <- append(genData[posGenData, ], 'mainGen')    
            }
            else
            {
                allLabelsData[nrow(allLabelsData) + 1,] <- append(genData[posGenData, ], 'basicGen')
            }
            
        }
        genData <- genData[- c(posGenData), ]
    }
}



ggePlot <- ggplot() + 
    geom_point(data = genData, mapping = aes(x = x, y = y), size = 0.9, color = "#111111") +
    geom_segment(data = xyAxisData, aes( x = xStart, y = yStart, xend = xEnd, yend = yEnd), colour = "#333333", size = 1, linetype = "solid") +
    geom_segment(data = blueAxisData, aes( x = xStart, y = yStart, xend = xEnd, yend = yEnd), colour = "#0000FF", size = 1, linetype = "solid") +
    geom_segment(data = greenLinesData, aes( x = xStart, y = yStart, xend = xEnd, yend = yEnd), colour = "#6fd46c", size = 0.5, linetype = "dotted") +
    coord_fixed(ratio = 1) +
    geom_text_repel(data = allLabelsData, mapping = aes(x = x, y = y, label = labelName, color = type), size = 3.5,  show.legend = FALSE) +
    scale_colour_manual(values = c("black", "red", "blue", "purple")) +
    labs(x = paste("AXIS 1 : ", axisPercentageData[1,2], " % "), y = paste("AXIS 2 : ", axisPercentageData[2,2], " % ")) +
    theme_minimal()

saveGGplot(ggePlot, fileName)







