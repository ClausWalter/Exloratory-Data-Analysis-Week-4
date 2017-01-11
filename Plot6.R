Plot6 <- function(){
  
  ## Preparations: load necessary libraries, get/set paths, load, unzip and filter data:
  
  library(ggplot2)
  library(cowplot )
  
  path<-getwd()
  print("Data loading and unzipping")
  url <-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  filename<- "exdata%2Fdata%2FNEI_data.zip"
  filenameNpath<-file.path(path, filename)
  if (!file.exists(path)) {dir.create(path)}
  if (!file.exists(filenameNpath)){download.file(url, destfile = filenameNpath, mode = "wb")}
  unzip(file.path(path, filename), files = NULL, list = FALSE, overwrite = TRUE, junkpaths = FALSE, exdir = ".", unzip = "internal", setTimes = FALSE)
  
  ## Taking data over into internal tables (as per assignment recommendations given by R. Peng):
  
  print("Take over data into internal tables")
  if(!exists("SCC")){SCC <- readRDS("Source_Classification_Code.rds")}
  if(!exists("NEI")){NEI <- readRDS("summarySCC_PM25.rds")}
  if(!exists("NEI_SCC")) {NEI_SCC <- merge(NEI, SCC)}
  if(!exists("NEI_Balti_Motor_LA")){
    NEI_Balti_Motor_LA<-subset(NEI_SCC, fips %in% c("24510", "06037") & type == "ON-ROAD")
  }
  NEIByYear_aggregates <- aggregate(Emissions ~ year + fips, data = NEI_Balti_Motor_LA, sum)
  
  ## Calculating change percentage per city relative to 1999 (for each year measured)
  
  for(i in c(1:4)){ NEIByYear_aggregates$Pct_Change[i]<-NEIByYear_aggregates[i,3]*100/NEIByYear_aggregates[1,3]-100}
  for(i in c(5:8)){ NEIByYear_aggregates$Pct_Change[i]<-NEIByYear_aggregates[i,3]*100/NEIByYear_aggregates[5,3]-100}
  
  ## Compare emissions from motor vehicle sources in Baltimore City (fips == "24510") to Los Angeles (fips == "06037"). Which had the greater change over time?if(!exists("NEI_SCC")) {NEI_SCC <- merge(NEI, SCC)}
  ## For the fun of it, I calculate not only the absolute change, but also the relative change per year relative to 1999. Both graphs are shown as facets
  ## side-by-side. In the other plots, I used line-charts only. Here, I wanted to also try out bar charts. Remark on the data used:
  ## as for Plot5, I hope I used the right criterion to identify the correct data for emmissions caused by vehicles (type == "ON-ROAD"). With grep, any
  ## string in full-text search would have worked. Tried it, took a very long time on my PC. So I went for that solution.
  ## Plotting starts (left side of the picture: absolute changes per city, right side: percentage changes per city relative to 1999, for each year measured)
  
  print("Starting Plotting") 
  
  qp<-qplot(year, Pct_Change, data=NEIByYear_aggregates, colour = fips, geom=c("line", "point"), 
    main="Percent Change of PM2.5 Motor Vehicle Emmissions\nrelative to 1999 Values\nfor fips 24510 == Baltimore, fips 06037 == LA") + 
    scale_x_continuous(breaks = sequence(1999:2008))
  gg<-ggplot(NEIByYear_aggregates, aes(x=year, y=Emissions, fill=fips, label = Emissions)) + 
    geom_bar(stat="identity", position="dodge") + 
    ggtitle("Total PM2.5 Motor Vehicle Emmissions\nfor fips 24510 == Baltimore, fips 06037 == LA") + 
    scale_x_continuous(breaks = c(1999, 2002, 2005, 2008))
  print(plot_grid(gg, qp, align='h'))
  dev.copy(png, file=file.path(path, "Plot6.png"), width=1280, height=640)
  dev.off()
  
  ## Now all should be finished
  print("Finished!")
  }