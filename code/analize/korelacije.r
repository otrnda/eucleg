

require(Hmisc)
require(corrplot)
require(ggcorrplot)

korelacijaLokaliteta <- function(redniBrojOsobine, data)
{
    dir.create(file.path(REZULTATI_FOLDER, "korelacije"), showWarnings = FALSE)
    #redniBrojOsobine <- 7
    #data = DATA$PodsPerNode
    
    fileNameToSave <- paste("0", redniBrojOsobine, " ", VAR_NAMES[redniBrojOsobine], sep = "")
    
    longDataOneVar <- data.frame(
        "Environment" = DATA$Environment, 
        "Genotype" = DATA$Genotype, 
        "Values" = data)
    
    
    longDataOneVarMeans <- longDataOneVar %>% 
        dplyr::group_by(Environment, Genotype) %>% 
        dplyr::summarise(ValuesMean = mean(Values, na.rm = TRUE))
    

    wideDF <- longDataOneVarMeans %>%
        pivot_wider(names_from = Environment, values_from = ValuesMean)
    
    wide <- wideDF %>% 
        dplyr::select(-Genotype) %>% 
        as.matrix()
    
    corrRez <- Hmisc::rcorr(wide, type = "pearson")
    
    r_values <- corrRez$r
    p_values <- replace(corrRez$P, is.na(corrRez$P), 0)
    
    plot <- ggcorrplot::ggcorrplot(r_values,
               method = "square",
               ggtheme = ggplot2::theme_minimal,
               title = VAR_NAMES[redniBrojOsobine],
               type = "lower",
               outline.color = "white",
               colors = c("#EE9988", "#FFFFFF", "#77AADD"),
               lab = TRUE,
               lab_size = 2,
               p.mat = p_values,
               insig = "blank",
               show.diag = TRUE,
               show.legend = FALSE,
               sig.level = 0.01,
               ) +
        theme_bw() +
        xlab("") + 
        ylab("") + 
        theme(
            plot.title = element_text(size = 7),
            axis.text.x = element_text(size = 5, color = "black"),  
            axis.text.y = element_text(size = 5, color = "black"),
            legend.position = "none"
            )+
        theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0))
    writeToExcelFile("korelacije/korelacije-po-osobinama-posebno", 
                    fileNameToSave, list(r_values, p_values), writeRowNames = TRUE)
    return(plot)
    
}

# KORELACIJE LOKALITETA PO SVAKOJ OSOBINI POSEBNO
p1 <- korelacijaLokaliteta(redniBrojOsobine = 1, data = DATA$DaysToFlower)
p2 <- korelacijaLokaliteta(redniBrojOsobine = 2, data = DATA$DaysToMature)
p3 <- korelacijaLokaliteta(redniBrojOsobine = 3, data = DATA$PlantHeight)
p4 <- korelacijaLokaliteta(redniBrojOsobine = 4, data = DATA$Branching)
p5 <- korelacijaLokaliteta(redniBrojOsobine = 5, data = DATA$HeightFirstPod)
p6 <- korelacijaLokaliteta(redniBrojOsobine = 6, data = DATA$PodsPerFlower)
p7 <- korelacijaLokaliteta(redniBrojOsobine = 7, data = DATA$PodsPerNode)
p8 <- korelacijaLokaliteta(redniBrojOsobine = 8, data = DATA$PodLenght)
p9 <- korelacijaLokaliteta(redniBrojOsobine = 9, data = DATA$SeedsPerPod)


mergedPlot <- ggpubr::ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9,
                                labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I"),
                                font.label = list(size = 8, color = "black", face = "bold", 
                                                  family = NULL),
                                ncol = 3, 
                                nrow = 3, 
                                hjust = -1,
                                vjust = 2,
                                common.legend = FALSE
) + theme(plot.background = element_rect(fill = "white", colour = "white"))

mergedPlot
saveGGplot2(dataPlot = mergedPlot,
            folder = "korelacije",
            fileName = "korelacije-po-osobinama-posebno",
            isWide = TRUE,
            aspect_ratio = 1)






korelacijeOsobina <- function()
{
    dir.create(file.path(REZULTATI_FOLDER, "korelacije"), showWarnings = FALSE)
    allVariables <- cbind(DATA$DaysToFlower, DATA$DaysToMature,  DATA$PlantHeight, DATA$Branching,
                          DATA$HeightFirstPod, DATA$PodsPerFlower, DATA$PodsPerNode,
                          DATA$PodLenght, DATA$SeedsPerPod)
    
    colnames(allVariables) <- VAR_NAMES_SHORT
    
    corrRez <- rcorr(allVariables, type = "pearson")
    
    # rovi
    r_values <- corrRez$r
    p_values <- replace(corrRez$P, is.na(corrRez$P), 0)
    
    plot <- ggcorrplot(r_values,
                       method = "square",
                       hc.order = TRUE,
                       legend.title = NULL,
                       # hc.method = "hclust",
                       ggtheme = ggplot2::theme_minimal,
                       type = "lower",
                       outline.color = "white",
                       colors = c("#EE9988", "#FFFFFF", "#77AADD"),
                       lab = TRUE,
                       lab_size = 4,
                       p.mat = p_values,
                       sig.level = 0.01,
                       insig = "blank",
                       show.diag = TRUE,
                       show.legend = TRUE,
    ) +
        theme_bw() +
        xlab("") + 
        ylab("") + 
        theme(
            plot.title = element_text(size = 10),
            axis.text.x = element_text(size = 9, color = "black"),  
            axis.text.y = element_text(size = 9, color = "black"),
            legend.position = "none"
        ) 
    writeToExcelFile("korelacije/korelacija-osobina", 
                     "korelacija osobina", list(r_values, p_values), writeRowNames = TRUE)
    return(plot)
}

p10 <- korelacijeOsobina()
p10
saveGGplot2(dataPlot = p10,
            folder = "korelacije",
            fileName = "korelacija-osobina",
            isWide = TRUE,
            aspect_ratio = 0.75)
    
rm(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, mergedPlot)


# OVO JE ZA PANEL ALI IMA SUVISE LOKALITETA PA SE NE RADI
#panel.scat <- function(x, y){
#    points(x,y, pch = 21, cex = 1.1, col = "black", bg = "lightblue", lwd = 1.1)
#}
#panel.hist <- function(x, ...)
#{
#    usr <- par("usr"); on.exit(par(usr))
#    par(usr = c(usr[1:2], 0, 1.8))
#    h <- hist(x, plot = FALSE)
#    breaks <- h$breaks; nB <- length(breaks)
#    y <- h$counts; y <- y/max(y)
#    rect(breaks[-nB], 0, breaks[-1], y, col = "red", ...)
#}

#panel.cor <- function(x, y, cex.cor = 1.8, method = "pearson", ...) 
#{
#    options(warn = -1)                                      # Turn of warnings (e.g. tied ranks)
#    usr <- par("usr"); on.exit(par(usr))                    # Saves current "usr" and resets on exit
#    par(usr = c(0, 1, 0, 1))                                # Set plot size to 1 x 1
#    r <- cor(x, y, use = "pairwise.complete.obs")          
#    p <- cor.test(x, y, method = method)$p.val              # p-value
#    n <- sum(complete.cases(x, y))                          # How many data pairs
#    txt <- round(r, digits = 3) 
#    zvezde = ""
#    if(p < 0.05){zvezde <- " *"}
#    if(p < 0.01){zvezde <- " **"}
#    txtAll <- paste0(zvezde, '\n', txt )                     # Make panel text
#    text(0.5, 0.5, txtAll, cex = cex.cor, ...)              # Place panel text
#    options(warn = 0)                                       # Reset warning
#}
#pdf(file = paste(current_trial_name,".pdf"))
#pairs(wide[,1:2],lower.panel = panel.scat, diag.panel = panel.hist, upper.panel = panel.cor) 
#dev.off()








