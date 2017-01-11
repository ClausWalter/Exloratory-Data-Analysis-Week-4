Plot5 <- function(){
  
  ## Preparations: load necessary libraries, get/set paths, load, unzip and filter data:
  
  library(ggplot2)
  
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

  ## How have emissions from motor vehicle sources chaned from 1999 - 2008 in Baltimore City?
  ## Here, I am again not entirely sure which the appropriate data identification was, analyzing the sCC table. I chose type == "ON-ROAD",
  ## hoping that this identifies the right data. However, using grep similarly to the other assignment would have worked. 
  
  if(!exists("NEI_SCC")) {NEI_SCC <- merge(NEI, SCC)}
  if(!exists("NEI_Balti_Motor")){
    NEI_Balti_Motor<-subset(NEI, fips == "24510" & type == "ON-ROAD")
  }
  NEIByYear_aggregates <- aggregate(Emissions ~ year, data = NEI_Balti_Motor, sum)
  
  ## Calculating change percentage per city relative to 1999 (for each year measured)
  
  for(i in c(1:4)){ NEIByYear_aggregates$Pct_Change[i]<-NEIByYear_aggregates[i,2]*100/NEIByYear_aggregates[1,2]-100}
  
  print("Starting Plotting") 
  
  qp<-qplot(year, Pct_Change, data=NEIByYear_aggregates, geom=c("line", "point"), 
            main="Percent Change of PM2.5 Motor Vehicle Emmissions\nrelative to 1999 Values\nfor fips 24510 == Baltimore", col="red") + 
    scale_x_continuous(breaks = sequence(1999:2008)) +
    theme(legend.position = "none") +
    geom_text(aes(label=round(Pct_Change, 2)), vjust=1.5, colour="black")
  gg<-ggplot(NEIByYear_aggregates, aes(x=year, y=Emissions, label = Emissions)) + 
    geom_bar(stat="identity", position="dodge", fill = "red") + 
    ggtitle("Total PM2.5 Motor Vehicle Emmissions\nfor fips 24510 == Baltimore") + 
    scale_x_continuous(breaks = c(1999, 2002, 2005, 2008)) +
    theme(legend.position = "none") +
    geom_text(aes(label=round(Emissions, 2)), vjust=1.5, colour="white")
  print(plot_grid(gg, qp, align='h'))
  dev.copy(png, file=file.path(path, "Plot5.png"), width=1280, height=640)
  dev.off()
  
  ## Now all should be finished
  print("Finished!")
  }