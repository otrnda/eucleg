

require(devtools)
require(gginnards)

source("functions/analysisFunctions.r")
mainGenotypes <- data.frame(codeName = c("G180","G179","G177","G178","G176"), 
                            realName = c("BARACA", "FANFARE", "MERKUR", "MERLIN","MISTRAL"))

blups_sca <- as.data.frame(scale(as.matrix(BLUPS)))
dataForPCA <- blups_sca %>% mutate(Genotype = rownames(blups_sca))
colnames(dataForPCA) <- c(VAR_NAMES,"Genotype")

for (x in 1:nrow(mainGenotypes)) 
{
    dataForPCA[dataForPCA == mainGenotypes[x,1]] <- mainGenotypes[x,2]
}
visibleLabels <- c("BARACA", "FANFARE", "MERKUR", "MERLIN","MISTRAL",
                     "G100","G099","G098",
                     "G051","G112","G282","G115","G052","G114","G126","G113","G298","G131",
                     "G027","G186","G103","G287","G092","G137","G101","G104","G111",
                     "G089","G090","G088","G171","G121","G177","G093","G245","G004","G179","G176","G117","G178",
                     "G249","G372","G139","G082","G118","G174","G029","G347","G199","G359","G170",
                     "G146","G155","G152","G145","G018","G025","G164","G153","G042","G107","G013","G192","G223",
                     "G022","G063","G072","G045","G011","G091","G148","G151","G010","G110","G023","G031","G097",
                     "G080","G002")



labelsShifting <- data.frame(
    labels = c("G080","G152","G013","G136","G298","G089","G249","G091","G145","G089","G004","G170","G088","FANFARE",
               "G092","G245","G097","G372","G126","G155","G126","G245","G359", "G110","G249", "G011", "G298",
               "G097","G170", "G031"),
    moveX = c(0,0,0,0,0.2,0,0,0.1,0,0,0,0.1,0,0,0,-0.1,0,0,0,-0.2,0,0,0.3,0,0,0,0,0.2,-0.2, -0.3),
    moveY = c(0.1,0.15,0.1,-0.1,0,0.1,0.1,0,-0.2,0.2,0.1,-0.05,0.1,-0.3,-0.1,0,-0.1,0.2,-0.1,-0.2,0.2,0.2,-0.2,-0.1,-0.2,0.2,
              -0.2,-0.2, 0.1,0)
)

# pca bez kategorija
dataForPCA$textColor <- "black"
dataAverages <- dataForPCA


pcaBezKategorijaResult <- pcaBezKategorija(dataForPCA, visibleLabels, labelsShifting, "pcaBezKategorija")

# pca sa botanical type
blupsWithBtForPCA <- BLUPS_WITH_BT %>% dplyr::rename(Group = BT)
pcaBotanicalTypeResult <- pcaSakategorijamaBezLabela(blupsWithBtForPCA, 9, "pcaBotanicalType")


# pca sa clusterma
tmp <-  BLUPS_SCALED_WITH_MEMBERS %>% dplyr::rename(Group = MemberOf)

colnames(tmp) <-  c("Days to flower", "Height first pod", "Days to mature", "Plant height", 
                    "Pods per node", "Seeds PP","Number of BRA",
                                 "Pods per flower",  "Pod length", "Gen", "Group")
{
source("functions/analysisFunctions.r")
grafik12 <- pcaSakategorijamaBezLabela(tmp, 9, "pcaSaClusterima12", drugaosa = 2); grafik12
grafik13 <- pcaSakategorijamaBezLabela(tmp, 9, "pcaSaClusterima13", drugaosa = 3)
mergedPlot <- ggpubr::ggarrange(grafik12, grafik13,
                                labels = c("A", "B"),
                                font.label = list(size = 8, color = "black", face = "bold", family = NULL),
                                ncol = 2, 
                                nrow = 1, 
                                hjust = -1,
                                vjust = 3,
                                legend = "bottom",
                                common.legend = TRUE
) + 
    theme(plot.background = element_rect(fill = "white", colour = "white"))

mergedPlot

saveGGplot2(dataPlot = mergedPlot,
            folder = "pca",
            fileName = "pca-merged",
            isWide = TRUE,
            aspect_ratio = 0.52)
}

rm(mainGenotypes, dataForPCA, x, visibleLabels, labelsShifting, pcaSaClusterima,
   pcaBotanicalTypeResult, pcaBezKategorijaResult, tmp, blups_sca, blupsWithBtForPCA)
rm(mergedPlot, grafik12, grafik13, dataAverages)


