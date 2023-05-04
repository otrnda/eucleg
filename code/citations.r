

citationInfo <- function(packageName)
{
    print (utils::packageVersion(packageName))
    print (citation(packageName))
}
citation()
citationInfo("lme4")
citationInfo("lmerTest")
citationInfo("Hmisc")
citationInfo("ggcorrplot")
citationInfo("dendextend")
citationInfo("gplots")
citationInfo("clv")
citationInfo("rnaturalearth")
citationInfo("eurostat")

 
