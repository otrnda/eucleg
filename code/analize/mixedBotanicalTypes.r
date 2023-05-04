
# BOTANICAL TYPES
require(broom.mixed)
require(report)
myMixedModel <- function(dataColumn, dataName)
{
    # dataColumn <- DATA_FILTER_BOT_TYPE$DaysToMature
    # dataName <- "DaysToMature"
    mixedModel <- lmerTest::lmer(dataColumn ~ BotanicalType + (1|Environment) + (1|Genotype), 
                                 data = DATA_FILTER_BOT_TYPE, 
                                 REML = TRUE)
    
    report <- report::report(mixedModel)
    print(report)
    summary(mixedModel)
    modelTidy <- broom.mixed::tidy(mixedModel)
    modelGlance <- broom.mixed::glance(mixedModel)
    rez <- modelTidy %>% 
        dplyr::select(effect, term, estimate, std.error, p.value) %>% 
        dplyr::rename(!!dataName := term) %>% 
        head(4)
    return (rez)
}

rezDaysToFlower <- myMixedModel(DATA_FILTER_BOT_TYPE$DaysToFlower, "DaysToFlower")
rezDaysToMature <- myMixedModel(DATA_FILTER_BOT_TYPE$DaysToMatur, "DaysToMature")
rezPlantHeight <- myMixedModel(DATA_FILTER_BOT_TYPE$PlantHeight, "PlantHeight")
rezBranching <- myMixedModel(DATA_FILTER_BOT_TYPE$Branching, "Branching")
rezHeightFirstPod <- myMixedModel(DATA_FILTER_BOT_TYPE$HeightFirstPod, "HeightFirstPod")
rezPodsPerFlower <- myMixedModel(DATA_FILTER_BOT_TYPE$PodsPerFlower, "PodsPerFlower")
rezPodsPerNode <- myMixedModel(DATA_FILTER_BOT_TYPE$PodsPerNode, "PodsPerNode")
rezPodLenght <- myMixedModel(DATA_FILTER_BOT_TYPE$PodLenght, "PodLenght")
rezSeedsPerPod <- myMixedModel(DATA_FILTER_BOT_TYPE$SeedsPerPod, "SeedsPerPod")

all <- list(rezDaysToFlower, rezDaysToMature,  rezPlantHeight, rezBranching,
            rezHeightFirstPod,  rezPodsPerFlower, rezPodsPerNode,
            rezPodLenght, rezSeedsPerPod)

dir.create(file.path(REZULTATI_FOLDER, "mixed-model"), showWarnings = FALSE)
writeToExcelFile(fileName = "mixed-model/mixedBotanical", "mixedBotanical", all, writeRowNames = FALSE)


rm(rezDaysToFlower, rezDaysToMature,  rezPlantHeight, rezBranching,
   rezHeightFirstPod,  rezPodsPerFlower, rezPodsPerNode,
   rezPodLenght, rezSeedsPerPod, all)


#summary(M1)
#residuals <- resid(M0)
#hist(residuals)
#anova(M0, M_NULL)
#lme4::fixef(M0)
#lme4::ranef(M1)







