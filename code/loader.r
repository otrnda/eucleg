
# LIBRARIES
    require(dplyr)
    require(tidyr)
    require(tidyverse)
    require(ggplot2)
    require(ggrepel)
    require(rlang)
    require(GGally)
    require(stringr)
    require(xlsx)
    require(openxlsx)
    require(ppcor)
    require(lme4)

# SOURCES
    setwd("D:/mDocuments/GitRepositoriess/milos-eucleg/code")
    source("functions/commonFunctions.r")
    source("functions/graphFunctions.r")
    source("functions/analysisFunctions.r")
    source("functions/readDataFromExcel.r")

# DATA
    
    PODACI_FOLDER = "D:/mDocuments/GitRepositoriess/milos-eucleg/data/"
    REZULTATI_FOLDER = resultsFolderCreator("D:/mDocuments/GitRepositoriess/milos-eucleg/results/")
    
    cols <- c("character", "integer", "integer", "integer", "character", "character", "integer", 
                  "character", "integer", "character", "integer", rep("double", 9))
    DATA <- readExcelFajl(2, cols, 'allDataV8fix.xlsx')
    rm(cols)
    
    DATA$BotanicalCode <- DATA$BotanicalCode - 1
    DATA_GEN_AVE <- DATA %>% dplyr::select(Environment, Genotype, BotanicalType, 
                                           DaysToFlower, DaysToMature, PlantHeight, 
                                           Branching, HeightFirstPod, PodsPerFlower,
                                           PodsPerNode, PodLenght, SeedsPerPod) %>% 
        dplyr::group_by(Environment, Genotype, BotanicalType) %>% 
        dplyr::summarise(
                    DaysToFlower = mean(DaysToFlower, rm.na = TRUE),
                    DaysToMature = mean(DaysToMature, rm.na = TRUE),
                    PlantHeight = mean(PlantHeight, rm.na = TRUE),
                    Branching = mean(Branching, rm.na = TRUE),
                    HeightFirstPod = mean(HeightFirstPod, rm.na = TRUE),
                    PodsPerFlower = mean(PodsPerFlower, rm.na = TRUE),
                    PodsPerNode = mean(PodsPerNode, rm.na = TRUE),
                    PodLenght = mean(PodLenght, rm.na = TRUE),
                    SeedsPerPod = mean(SeedsPerPod, rm.na = TRUE),
                  )

    DATA_FILTER_BOT_TYPE <- DATA %>% filter(DATA$BotanicalType != "Unknown") %>% arrange(BotanicalCode)
    DATA_FILTER_BOT_TYPE$BotanicalType <- factor(DATA_FILTER_BOT_TYPE$BotanicalType, 
                                                  levels = c('Paucijuga', 'Minor', 'Equina', 'Major'))
    DATA_VARIABLES <- DATA %>% dplyr::select(DaysToFlower, DaysToMature, PlantHeight,  Branching, 
                                        HeightFirstPod, PodsPerFlower, PodsPerNode,
                                         PodLenght, SeedsPerPod)
    
    VAR_NAMES <- c("Days to flower", "Days to mature", "Plant height", "Number of branches",
                   "Height first pod", "Pods per flower", 
                   "Pods per node", "Pod length", "Seeds per pod")
    VAR_NAMES_SHORT <- c("DTF", "DTM", "PH", "NOB", "HFP", "PPF", "PPN", "PL", "SPP")
    
    
    BLUPS <- readExcelFajl(1, c("character", rep("double", 9)), 'allBlupsV6fix.xlsx')
    rownames(BLUPS) <- BLUPS$Genotype

    BLUPS <- dplyr::select(BLUPS,  DaysToFlower, DaysToMature, PlantHeight, Branching, 
                    HeightFirstPod, PodsPerFlower, PodsPerNode, PodLenght, SeedsPerPod)

    
    genoBot <- data.frame(DATA$Genotype, DATA$BotanicalType)
    genoBotDist <- distinct(genoBot, genoBot$DATA.Genotype, genoBot$DATA.BotanicalType, .keep_all= FALSE) 
    colnames(genoBotDist) <- c("GE", "BT")
    GENOTYPE_BT <- genoBotDist %>% arrange(genoBotDist$GE)
    
    BLUPS_WITH_BT <- cbind(BLUPS, rownames(BLUPS))
    colnames(BLUPS_WITH_BT) <- c(colnames(BLUPS), "GE")
    BLUPS_WITH_BT <- left_join(BLUPS_WITH_BT, genoBotDist) %>% filter(BT != "Unknown")
    BLUPS_WITH_BT$BT <- factor(BLUPS_WITH_BT$BT, levels = c('Paucijuga', 'Minor', 'Equina', 'Major'))
    
    rm(genoBot, genoBotDist)
        
# BOJE
    BLUE <- "#1134A6"
    BLUE_BACK <- "royalblue"
    ORANGE <- "#e47d00"




