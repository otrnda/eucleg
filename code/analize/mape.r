


require(sf)
require(rnaturalearth)
require(eurostat)
#require(countrycode)
#require(ggrepel)





mapeCreator <- function()
{
    world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf")
    
    genotype_info <- readExcelFajl(1, c(rep("character", 7)), 'genotype-origin.xlsx')
    
    country_count <- genotype_info %>%
        dplyr::select(Origin) %>% group_by(Origin) %>% 
        dplyr::summarise(count = n()) %>% 
        arrange(desc(count)) %>% mutate(Visited = TRUE) %>% na.omit()
    
    
    country_colored <- world %>% 
        dplyr::select(geometry, name, iso_a3_eh) %>% 
        left_join(country_count, by = c("iso_a3_eh" = "Origin")) %>%
        na.omit() %>% 
        filter(Visited == TRUE) %>% 
        arrange(desc(count))
    
    
    country_labeled <- country_colored %>% 
        dplyr::mutate(labels = c(paste(country_colored$iso_a3_eh, country_colored$count, sep = " "))) %>% 
        head(12)
    
    worldMap <- world %>% 
        filter(admin != "Antarctica") %>% 
        sf::st_transform(crs = "+proj=robin") %>% 
        ggplot() + 
        theme_bw() +
        geom_sf(color = "white") +
        geom_sf(data = country_colored, aes(fill = Visited)) +
        scale_fill_manual(values = BLUE_BACK) + 
        ggrepel::geom_label_repel(
            data = country_labeled,
            aes(label = labels, geometry = geometry),
            stat = "sf_coordinates",
            min.segment.length = 0,
            max.overlaps = 140,
            box.padding = 0.1,
            label.padding = 0.1,
            label.size = 0.1,
            size = 1.3,
            fontface = 'bold'
        ) +
        theme(plot.title = element_text(face = "bold"),
              axis.text.x = element_blank(),
              legend.position = "none",
              axis.ticks.x = element_blank()
        )+
        xlab("") + 
        ylab("")
    
    saveGGplot2(dataPlot = worldMap, fileName = "maps/mapWorld", isWide = FALSE, aspect_ratio = 0.5)
    
    
    SHP_0 <- eurostat::get_eurostat_geospatial(resolution = 10, 
                                     nuts_level = 0, 
                                     year = 2021)
    locations <- data.frame(ID = c("AG", "IFC", "BO", "GU"),
                            east = c(-6.36, 21.20, 23.30, 3.81), 
                            nort = c(37.46, 43.58, 60.49, 50.98))
    europeMap <- SHP_0 %>% 
        ggplot() +
        geom_sf() +
        geom_point(data = locations, 
                   aes(x = east, y = nort),
                   col = BLUE, size=4)+
        geom_label(label = "AG", 
                   x = -6.36, y = 37.46 + 3, 
                   label.padding = unit(0.2, "lines"), 
                   size = 3
        )+
        geom_label(label = "IFC", 
                   x = 21.20, y = 43.58 + 3,
                   label.padding = unit(0.2, "lines"), 
                   size = 3
        )+
        geom_label(label = "BO", x = 23.30, y = 60.49 + 3,
                   label.padding = unit(0.2, "lines"), 
                   size = 3
        )+
        geom_label(label = "GU", x = 3.81, y = 50.98 + 3,
                   label.padding = unit(0.2, "lines"), 
                   size = 3
        )+
        scale_x_continuous(limits = c(-10, 35)) +
        scale_y_continuous(limits = c(35, 65)) + 
        xlab(" ") + ylab(" ") +
        theme_bw()
    
    
    saveGGplot2(dataPlot = europeMap, fileName = "maps/mapEurope", isWide = FALSE, aspect_ratio = 1)    
}
dir.create(file.path(REZULTATI_FOLDER, "maps"), showWarnings = FALSE)
mapeCreator()


