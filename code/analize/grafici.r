




# SCATER BEZ KATEGORIJE -> JEDNA OSOBINE
scaterBezKategorije <- function(folderName)
{
    dir.create(file.path(REZULTATI_FOLDER, folderName), showWarnings = FALSE)
    vars <- DATA_GEN_AVE %>% ungroup() %>% dplyr::select(Environment, 
                                                         DaysToFlower, DaysToMature, PlantHeight, Branching, 
                                                         HeightFirstPod, PodsPerFlower, PodsPerNode,
                                                         PodLenght, SeedsPerPod)
    
    for(i in 2:length(vars))
    {
        plot <- ggplot(data = vars, aes(x = Environment, y = vars[[i]])) +
            geom_point(position = position_jitter(width = 0.3, height = 0.3)) +
            theme_bw() +
            ylab(VAR_NAMES[i-1])
        
        saveGGplot2(dataPlot = plot, 
                    folder = folderName, 
                    fileName = paste("0", i-1, " ", VAR_NAMES[i-1], sep = ""),
                    isWide = TRUE, aspect_ratio = 0.75)        
    }
}
scaterBezKategorije("scater-bez-kategorije")



# BOX + SCATER -> JEDNA OSOBINA
scaterBox <- function(folderName)
{
    dir.create(file.path(REZULTATI_FOLDER, folderName), showWarnings = FALSE)

    vars <- DATA_GEN_AVE %>% ungroup() %>% dplyr::select(Environment, 
                                                         DaysToFlower, DaysToMature, PlantHeight, Branching, 
                                                         HeightFirstPod, PodsPerFlower, PodsPerNode,
                                                         PodLenght, SeedsPerPod)
    for(i in 2:length(vars))
    {
        plot <- DATA_GEN_AVE %>%
            ggplot(aes(x = Environment, y = vars[[i]], fill = Environment)) +
            geom_boxplot() +
            scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
            geom_jitter(color="black", size=0.4, alpha = 0.9, width = 0.2, height = 0.5) +
            theme_bw() +
            theme(
                legend.position="none",
                plot.title = element_text(size=11)
            ) +
            xlab("") + 
            ylab(VAR_NAMES[i-1])
        
        saveGGplot2(dataPlot = plot, 
                    folder = folderName, 
                    fileName = paste("0", i-1, " ", VAR_NAMES[i-1], sep = ""),
                    isWide = TRUE, aspect_ratio = 0.75)
    }
}
scaterBox("scater-box")


# SCATER BOTANICAL TYPE -> JEDNA OSOBINA
scaterBotanical <- function(folderName)
{
    dir.create(file.path(REZULTATI_FOLDER, "scater-botanical"), showWarnings = FALSE)
    vars <- DATA_FILTER_BOT_TYPE %>% ungroup() %>% dplyr::select(Environment, 
                                                         DaysToFlower, DaysToMature, PlantHeight, Branching, 
                                                         HeightFirstPod, PodsPerFlower, PodsPerNode,
                                                         PodLenght, SeedsPerPod)

    for(i in 2:length(vars))
    {
        plot <- DATA_FILTER_BOT_TYPE %>% ggplot(aes(x = Environment, y = vars[[i]], color = BotanicalType )) +
            geom_point(position = position_jitter(width = 0.3, height = 0.1)) +
            scale_fill_discrete(limits =c('Paucijuga', 'Minor', 'Equina', 'Major')) +
            theme_bw() +
            xlab("Environment") + 
            ylab(VAR_NAMES[i-1])
        saveGGplot2(dataPlot = plot, 
                    folder = "scater-botanical", 
                    fileName = paste("0", i-1, " ", VAR_NAMES[i-1], sep = ""),
                    isWide = TRUE, 
                    aspect_ratio = 0.75)
    }
}
scaterBotanical(folderName = "scater-botanical")


# BOX BOTANICAL TYPE -> JEDNA OSOBINA
boxBotanical <- function(folderName, variableIndex)
{
    dir.create(file.path(REZULTATI_FOLDER, "scater-botanical"), showWarnings = FALSE)
    vars <- DATA_FILTER_BOT_TYPE %>% ungroup() %>% dplyr::select(Environment, 
                                                                 DaysToFlower, DaysToMature, PlantHeight, Branching, 
                                                                 HeightFirstPod, PodsPerFlower, PodsPerNode,
                                                                 PodLenght, SeedsPerPod)
    i <- variableIndex
    plot <- DATA_FILTER_BOT_TYPE %>% 
        ggplot(aes(x = Environment, y = vars[[i]], fill = BotanicalType)) + 
        geom_boxplot() +
        theme_bw() +
        theme(legend.position = c(0.9, 0.35), plot.title = element_text(size = 12)) +
        facet_wrap(~Environment, scales = "free_x", nrow = 1)+
        xlab(VAR_NAMES[i-1]) + ylab("")
    saveGGplot2(dataPlot = plot, 
                folder = folderName, 
                fileName = paste("0", i-1, " ", VAR_NAMES[i-1], sep = ""),
                isWide = TRUE, 
                aspect_ratio = 0.75)
    return(plot)
    
    # for(i in 2:length(vars))
    # {
    #     plot <- DATA_FILTER_BOT_TYPE %>% 
    #         ggplot(aes(x = Environment, y = vars[[i]], fill = BotanicalType)) + 
    #         geom_boxplot() +
    #         theme_bw() +
    #         theme(legend.position = c(0.9, 0.35), plot.title = element_text(size = 12)) +
    #         facet_wrap(~Environment, scales = "free_x", nrow = 2)+
    #         xlab(VAR_NAMES[i-1]) + ylab("")
    #     saveGGplot2(dataPlot = plot, 
    #                 folder = folderName, 
    #                 fileName = paste("0", i-1, " ", VAR_NAMES[i-1], sep = ""),
    #                 isWide = TRUE, 
    #                 aspect_ratio = 0.75)
    # }
}
pDTF <- boxBotanical(folderName = "box-botanical", variableIndex = 2)
pDTM <- boxBotanical(folderName = "box-botanical", variableIndex = 3)
pPH <- boxBotanical(folderName = "box-botanical", variableIndex = 4)
pBR <- boxBotanical(folderName = "box-botanical", variableIndex = 5)
pHFP <- boxBotanical(folderName = "box-botanical", variableIndex = 6)
pPPF <- boxBotanical(folderName = "box-botanical", variableIndex = 7)
pPPN <- boxBotanical(folderName = "box-botanical", variableIndex = 8)
pPL <- boxBotanical(folderName = "box-botanical", variableIndex = 9)
pSPP <- boxBotanical(folderName = "box-botanical", variableIndex = 10)


mergedPlot <- ggpubr::ggarrange(pDTF, pDTM, pPH, pBR, pHFP, pPPF, pPPN, pPL, pSPP,
                                labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I"),
                                font.label = list(size = 8, color = "black", face = "bold", family = NULL),
                                ncol = 1, 
                                nrow = 9, 
                                hjust = -2,
                                vjust = 2,
                                common.legend = TRUE,
                                legend = "bottom"
) + theme(plot.background = element_rect(fill = "white", colour = "white"))

mergedPlot <- ggpubr::ggarrange(pBR, pPL, pPPN, pSPP,
                                labels = c("A", "B", "C", "D"),
                                font.label = list(size = 8, color = "black", face = "bold", family = NULL),
                                ncol = 1, 
                                nrow = 4, 
                                hjust = -2,
                                vjust = 2,
                                common.legend = TRUE,
                                legend = "bottom"
) + theme(plot.background = element_rect(fill = "white", colour = "white"))

mergedPlot <- ggpubr::ggarrange(pDTM, pHFP, pPH, 
                                labels = c("A", "B", "C"),
                                font.label = list(size = 8, color = "black", face = "bold", family = NULL),
                                ncol = 1, 
                                nrow = 3, 
                                hjust = -2,
                                vjust = 2,
                                common.legend = TRUE,
                                legend = "bottom"
) + theme(plot.background = element_rect(fill = "white", colour = "white"))

mergedPlot

saveGGplot2(dataPlot = mergedPlot,
            folder = "box-botanical",
            fileName = "box-botanical-merged-3traits",
            isWide = TRUE,
            aspect_ratio = 1)




# DENSITY -> JEDNA OSOBINA
density <- function(folderName)
{
    dir.create(file.path(REZULTATI_FOLDER, folderName), showWarnings = FALSE)
    vars <- DATA_GEN_AVE %>% ungroup() %>% dplyr::select(Environment, 
                                                         DaysToFlower, DaysToMature, PlantHeight, Branching, 
                                                         HeightFirstPod, PodsPerFlower, PodsPerNode,
                                                         PodLenght, SeedsPerPod)
    for(i in 2:length(vars))
    {
       plot <- ggplot(data = DATA_GEN_AVE, 
               aes(x = vars[[i]], 
                   group = DATA_GEN_AVE$Environment, fill = DATA_GEN_AVE$Environment)) + 
            geom_density() +
            facet_wrap(~ Environment) +
            theme_bw() +
            theme(legend.position = "none", 
                  panel.spacing = unit(0.1, "lines"),
                  axis.ticks.x=element_blank())+
            xlab(label = VAR_NAMES[i-1]) + ylab(label = "")
       saveGGplot2(dataPlot = plot, 
                   folder = folderName, 
                   fileName = paste("0", i-1, " ", VAR_NAMES[i-1], sep = ""),
                   isWide = TRUE, 
                   aspect_ratio = 0.75)
    }
}

density("density-plots")






