

dir.create(file.path(REZULTATI_FOLDER, "deskriptivna"), showWarnings = FALSE)
bot_type_means <- DATA_FILTER_BOT_TYPE %>% 
    dplyr::select(Environment, BotanicalType, 
                  DaysToFlower, DaysToMature, PlantHeight, 
                  Branching, HeightFirstPod,PodsPerFlower,
                  PodsPerNode, PodLenght, SeedsPerPod,) %>% 
    group_by(Environment, BotanicalType) %>% 
    dplyr::summarise(
        DaysToFlowerMean = mean(DaysToFlower, na.rm = TRUE) ,
        DaysToMatureMean = mean(DaysToMature, na.rm = TRUE),
        PlantHeightMean = mean(PlantHeight, na.rm = TRUE),
        BranchingMean = mean(Branching, na.rm = TRUE),
        HeightFirstPodMean = mean(HeightFirstPod, na.rm = TRUE),
        PodsPerFlowerMean = mean(PodsPerFlower, na.rm = TRUE),
        PodsPerNodeMean = mean(PodsPerNode, na.rm = TRUE),
        PodLenghtMean = mean(PodLenght, na.rm = TRUE),
        SeedsPerPodMean = mean(SeedsPerPod, na.rm = TRUE),
    )

bot_type_sd <- DATA_FILTER_BOT_TYPE %>% 
    dplyr::select(Environment, BotanicalType, 
                  DaysToFlower, DaysToMature,  PlantHeight, Branching,
                  HeightFirstPod, PodsPerFlower, PodsPerNode, 
                  PodLenght,SeedsPerPod) %>% 
    group_by(Environment, BotanicalType) %>% 
    dplyr::summarise(
        DaysToFlowerSD = sd(DaysToFlower, na.rm = TRUE) ,
        DaysToMatureSD = sd(DaysToMature, na.rm = TRUE),
        PlantHeightSD = sd(PlantHeight, na.rm = TRUE),
        BranchingSD = sd(Branching, na.rm = TRUE),
        HeightFirstPodSD = sd(HeightFirstPod, na.rm = TRUE),
        PodsPerFlowerSD = sd(PodsPerFlower, na.rm = TRUE),
        PodsPerNodeSD = sd(PodsPerNode, na.rm = TRUE),
        PodLenghtSD = sd(PodLenght, na.rm = TRUE),
        SeedsPerPodSD = sd(SeedsPerPod, na.rm = TRUE),
    )


num_genotypes_per_location <- DATA %>% 
    dplyr::select(Environment, Genotype) %>% 
    group_by(Environment, Genotype) %>% dplyr::summarise(gen_occurrences = n())

num_repetitons_per_location <- num_genotypes_per_location %>% dplyr::select(Environment, gen_occurrences) %>% 
    dplyr::group_by(Environment, gen_occurrences) %>% 
    dplyr::summarise(repetitons = n())

dir.create(file.path(REZULTATI_FOLDER, "deskriptivna"), showWarnings = FALSE)
writeToExcelFile("deskriptivna/deskriptivnaPoOsobinama", "BotType Mean SD", 
                 list(bot_type_means, bot_type_sd), writeRowNames = FALSE)
writeToExcelFile("deskriptivna/deskriptivnaPoOsobinama", "GEN_PER_LOC", 
                 list(num_repetitons_per_location, num_genotypes_per_location), writeRowNames = FALSE)




AG18 <- DATA %>% filter(DATA$Environment == "AG18")
AG19 <- DATA %>% filter(DATA$Environment == "AG19")
BO18 <- DATA %>% filter(DATA$Environment == "BO18")
BO19 <- DATA %>% filter(DATA$Environment == "BO19")
GU18 <- DATA %>% filter(DATA$Environment == "GU18")
GU19 <- DATA %>% filter(DATA$Environment == "GU19")
IFC18 <- DATA %>% filter(DATA$Environment == "IFC18")
IFC19 <- DATA %>% filter(DATA$Environment == "IFC19")
IFC20 <- DATA %>% filter(DATA$Environment == "IFC20")

cols_for_desc <- c( "DaysToFlower", "DaysToMature", "PlantHeight", "Branching",  
                    "HeightFirstPod", "PodsPerFlower", "PodsPerNode", 
                    "PodLenght", "SeedsPerPod")
AG18_desk <- deskriptivnaStatistika(AG18, cols_for_desc)
AG19_desk <- deskriptivnaStatistika(AG19, cols_for_desc)
BO18_desk <- deskriptivnaStatistika(BO18, cols_for_desc)
BO19_desk <- deskriptivnaStatistika(BO19, cols_for_desc)
GU18_desk <- deskriptivnaStatistika(GU18, cols_for_desc)
GU19_desk <- deskriptivnaStatistika(GU19, cols_for_desc)
IFC18_desk <- deskriptivnaStatistika(IFC18, cols_for_desc)
IFC19_desk <- deskriptivnaStatistika(IFC19, cols_for_desc)
IFC20_desk <- deskriptivnaStatistika(IFC20, cols_for_desc)




writeToExcelFile("deskriptivna/deskriptivnaPoLokalitima", "AG18", list(AG18_desk), writeRowNames = FALSE)
writeToExcelFile("deskriptivna/deskriptivnaPoLokalitima", "AG19", list(AG19_desk), writeRowNames = FALSE)
writeToExcelFile("deskriptivna/deskriptivnaPoLokalitima", "BO18", list(BO18_desk), writeRowNames = FALSE)
writeToExcelFile("deskriptivna/deskriptivnaPoLokalitima", "BO19", list(BO19_desk), writeRowNames = FALSE)
writeToExcelFile("deskriptivna/deskriptivnaPoLokalitima", "GU18", list(GU18_desk), writeRowNames = FALSE)
writeToExcelFile("deskriptivna/deskriptivnaPoLokalitima", "GU19", list(GU19_desk), writeRowNames = FALSE)
writeToExcelFile("deskriptivna/deskriptivnaPoLokalitima", "IFC18", list(IFC18_desk), writeRowNames = FALSE)
writeToExcelFile("deskriptivna/deskriptivnaPoLokalitima", "IFC19", list(IFC19_desk), writeRowNames = FALSE)
writeToExcelFile("deskriptivna/deskriptivnaPoLokalitima", "IFC20", list(IFC20_desk), writeRowNames = FALSE)


# deskriptivna po osobinama
cols <- c("AG18", "AG19", "BO18", "BO19","GU18", "GU19", "IFC18", "IFC19", "IFC20")
osobine <- cols_for_desc
for(i in 1:length(osobine))
{
    osobinaStr <- osobine[i]
    osobinaWide <- DATA_GEN_AVE %>% dplyr::select(Environment, Genotype, !!osobinaStr)%>% spread(Environment, !!osobinaStr)
    osobina_desk <- deskriptivnaStatistika(osobinaWide, cols)
    writeToExcelFile("deskriptivna/deskriptivnaPoOsobinama", osobinaStr, list(osobina_desk), writeRowNames = FALSE)
}




rm(cols_for_desc, bot_type_means, bot_type_sd, cols, osobine, osobinaStr, i, osobinaWide, osobina_desk,
   AG18, AG19, BO18, BO19, GU18, GU19, IFC18, IFC19, IFC20, 
        AG18_desk, AG19_desk, BO18_desk, BO19_desk, GU18_desk, GU19_desk, IFC18_desk, IFC19_desk, IFC20_desk,
   num_genotypes_per_location, num_repetitons_per_location)



