
require(viridis)
require(dendsort)
require(dendextend)
require(gplots)
require(clv)

dir.create(file.path(REZULTATI_FOLDER, "clusters"), showWarnings = FALSE)
blups_sca <- as.data.frame(scale(as.matrix(BLUPS)))

colnames(blups_sca) <- VAR_NAMES_SHORT

row_colors <- c("magenta", "#008080", "orange", "red3", "green2", "blue", "black")
row_dend <- as.dendrogram(hclust(dist(blups_sca, method = "euclidean"), method = "complete"))
row_dend <- set(row_dend, "branches_lwd", 1)
row_dend <- dendextend::color_branches(row_dend, k = 7, col = row_colors, 
                            groupLabels = c("A", "B", "C", "D", "E", "F", "G")) 
plot(row_dend)


pheno_colors <- c("red3", "green2", "orange")
pheno_dend <- as.dendrogram(hclust(as.dist(1-cor(blups_sca, method = "pearson")), method = "complete"))
pheno_dend <- set(pheno_dend, "branches_lwd", 2)
pheno_dend <- dendsort(dendextend::color_branches(pheno_dend, k = 3, col = pheno_colors))
plot(pheno_dend)



tiff( res = 300, width = 185, height = 185, units = "mm", 
      file = paste(REZULTATI_FOLDER, "clusters/dendrogramHeatmap.tiff", sep = "") , type = "cairo")
heatmap.2(x = as.matrix(blups_sca), 
          Rowv = row_dend, 
          Colv = pheno_dend, 
          col = colorpanel(50, "magenta", "white", "green3"), 
          scale = "none", 
          density.info = "none",
          trace = "none",
          labRow = "",
          colsep = c(4,6),
          rowsep = c(14, 20, 46, 65, 93, 164, 220),
          sepcolor = "#666666",
          sepwidth = c(0.01,0.3),
)
dev.off() 

cut <- cutree(row_dend, k = 7)
row_dend_info <- clv::cls.scatt.data(blups_sca, cut)




BLUPS_SCALED_WITH_MEMBERS <- blups_sca %>% 
    dplyr::select(DTF, HFP, DTM, PH, PPN, SPP, NOB, PPF, PL) %>% 
    cbind("Genotype" = rownames(BLUPS)) %>% cbind("MemberOf" = as.vector(cut))


    
BLUPS_SCALED_WITH_MEMBERS$MemberOf[BLUPS_SCALED_WITH_MEMBERS$MemberOf == "1"] <- "C"
BLUPS_SCALED_WITH_MEMBERS$MemberOf[BLUPS_SCALED_WITH_MEMBERS$MemberOf == "2"] <- "D"
BLUPS_SCALED_WITH_MEMBERS$MemberOf[BLUPS_SCALED_WITH_MEMBERS$MemberOf == "3"] <- "A"
BLUPS_SCALED_WITH_MEMBERS$MemberOf[BLUPS_SCALED_WITH_MEMBERS$MemberOf == "4"] <- "B"
BLUPS_SCALED_WITH_MEMBERS$MemberOf[BLUPS_SCALED_WITH_MEMBERS$MemberOf == "5"] <- "E"
BLUPS_SCALED_WITH_MEMBERS$MemberOf[BLUPS_SCALED_WITH_MEMBERS$MemberOf == "6"] <- "G"
BLUPS_SCALED_WITH_MEMBERS$MemberOf[BLUPS_SCALED_WITH_MEMBERS$MemberOf == "7"] <- "F"


blups_scaled_all <- BLUPS_SCALED_WITH_MEMBERS %>% dplyr::left_join(GENOTYPE_BT,  by = c('Genotype' = 'GE'))


averages_per_cluster <- blups_scaled_all %>% 
    dplyr::group_by(MemberOf) %>% 
    dplyr::summarise(across(DTF:PL, mean, na.rm = TRUE))


count_per_cluster <- blups_scaled_all %>% 
    dplyr::group_by(MemberOf) %>% 
    dplyr::summarise(n = n())

count_per_bt <- blups_scaled_all %>% 
    dplyr::group_by(BT) %>% 
    dplyr::summarise(n = n())

count_per_cluster_bt <- blups_scaled_all %>% 
    dplyr::group_by(MemberOf, BT) %>% 
    dplyr::summarise(n = n())


writeToExcelFile("clusters/clustering-info", "cluster_members", list(blups_scaled_all), writeRowNames = TRUE)
writeToExcelFile("clusters/clustering-info", "averages_per_cluster", list(averages_per_cluster), writeRowNames = FALSE)
writeToExcelFile("clusters/clustering-info", "count_per_cluster_bt", list(count_per_cluster_bt), writeRowNames = FALSE)
writeToExcelFile("clusters/clustering-info", "average-distance", 
                 list(row_dend_info$intercls.average, row_dend_info$cluster.size), 
                 writeRowNames = FALSE)

writeToExcelFile("clusters/clustering-info", "average-intra", 
                 list(row_dend_info$intracls.average, row_dend_info$cluster.size), 
                 writeRowNames = FALSE)


rm(averages_per_cluster, blups_sca, count_per_cluster, cut,
   pheno_dend, row_dend, row_colors, pheno_colors,
   count_per_cluster_bt, count_per_bt, blups_scaled_all, row_dend_info)


