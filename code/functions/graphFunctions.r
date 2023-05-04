





#' Pavi ggplot heatmap i snima ih u rezultate. 
#' Ulaz je fajl sa svim podacima i niz sa rednim brojevima kolona koje treba ctati.
#'
#' @param df cela matrica sa lokalitetima
#' @param columnNumbers  na primer c(7,9)
#'
#' @return
#' @export
#'
#' @examples
heatmapCustom <- function(df, dataName, columnNumbers)
{
    numRedova = nrow(df)
    
    for(columnNumber in columnNumbers)
    {
        dataFrameZaHeat <- as.data.frame(matrix(nrow = numRedova, ncol = 3))
        for(redniBrojReda in c(1:numRedova))
        {
            dataFrameZaHeat[redniBrojReda,1] <- as.double(df$Row[redniBrojReda])
            dataFrameZaHeat[redniBrojReda,2] <- as.double(df$Column[redniBrojReda])
            dataFrameZaHeat[redniBrojReda,3] <- as.double(df[redniBrojReda,columnNumber])
        }
        colName = colnames(df)[columnNumber]
        grafik <- ggplot(dataFrameZaHeat, aes(V1, V2, fill= V3)) + geom_tile() + xlab(colName)
        saveGGplot(grafik, paste(colName, "heat", dataName, sep = "-"))
    }
}

boxPlotCustom <- function(df, dataName)
{
    for (colName in colnames(df))
    {
        cl = class(df[[colName]])
        dataSet = df[[colName]]
        if(cl == "integer" || cl == "numeric")
        {
            grafik <- ggplot(df, aes(x = Location, y = dataSet)) + geom_boxplot() + ylab(colName)
            saveGGplot(grafik, paste(colName, "box", dataName, sep = "-"))
        }
    }
}


histNumericColumns <- function(df, dataName)
{
    for (colName in colnames(df))
    {
        cl = class(df[[colName]])
        dataSet = df[[colName]]
        if(cl == "integer")
        {
            grafik <- ggplot(data = df, aes(dataSet)) + geom_histogram( stat = "count") + ggtitle(paste(dataName, colName, sep = " :: "))
            saveGGplot(grafik, paste(colName, "hist", dataName, sep = "-"))
        }
        if(cl == "numeric")
        {
            grafik <- ggplot(data = df, aes(dataSet)) + geom_histogram() + ggtitle(paste(dataName, colName, sep = " :: "))
            saveGGplot(grafik, paste(colName, "hist", dataName, sep = "-"))
        }
    }
}


scaterNumericColumns <- function(df, dataName)
{
    for (colName in colnames(df))
    {
        cl = class(df[[colName]])
        dataSet = df[[colName]]
        if(cl == "integer" || cl == "numeric")
        {
            # writeLines(colName)
            grafik = ggplot(df, aes(x = seq_along(dataSet), y = dataSet)) + geom_point()+ ggtitle(paste(dataName, colName, sep = " :: "))
            saveGGplot(grafik, paste(colName, "scater", dataName, sep = "-"))
        }
    }
    return (grafik)
}




# **************** STARO ************************

#' Boxplot sa kategrorijama. Gtafik se snima u fajl categorisedBoxPlot. Folder je eksterna promenljiva.
#'
#' @param data long format. Na x - osi se nalazeu Measurment, y - osa je Value a kategroja se zove Year
#'
#' @return NULL
boxplotCategorised <- function(data)
{
    brojRedova = 2
    xlabel = ""
    ylabel = ""
    boja1 = "#4682B4"
    boja2 = "#B47846"
    
    
    grafik <- ggplot(data, aes(x = Measurment, y = Value, fill = Year)) + 
        geom_boxplot(width = 0.5, position =  position_dodge(width=0.7)) +
        xlab(xlabel)+ ylab(ylabel)+
        facet_wrap(~Measurment, nrow = brojRedova, scale="free", shrink = TRUE, strip.position = "bottom") +
        theme( axis.text.x = element_blank(), axis.ticks.y = element_blank() ) +
        expand_limits(y = 0) +
        scale_fill_manual(values = c(boja1, boja2))
    
    saveGGplot(grafik, "categorisedBoxPlot")
    return(grafik)
}






# library(gridExtra)
# grid.arrange(cpPlot, cfPlot, cfatPlot, AA, PhC, FC, nrow = 2)


