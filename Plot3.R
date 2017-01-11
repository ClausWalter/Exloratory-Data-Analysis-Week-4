Plot3 <- function(){
  
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
  
  ## Of the four emission types (pint, nonpint, onroad, nonroad), which have seen a decrease for 1999-2008 for Baltimore? Use ggplot2.
  ## Creating subset tables per type:
  
  NEIByYear_point <- aggregate(Emissions ~ year, data = subset(NEI, subset= (fips == "24510" & type == "POINT")), sum)
  NEIByYear_point$type <- c("POINT")
  NEIByYear_nonpoint <- aggregate(Emissions ~ year, data = subset(NEI, subset= (fips == "24510" & type == "NONPOINT")), sum)
  NEIByYear_nonpoint$type <- c("NONPOINT")
  NEIByYear_onroad <- aggregate(Emissions ~ year, data = subset(NEI, subset= (fips == "24510" & type == "ON-ROAD")), sum)
  NEIByYear_onroad$type <- c("ON-ROAD")
  NEIByYear_nonroad <- aggregate(Emissions ~ year, data = subset(NEI, subset= (fips == "24510" & type == "NON-ROAD")), sum)
  NEIByYear_nonroad$type <- c("NON-ROAD")
  NEIByYear_aggregates <- rbind(NEIByYear_point, NEIByYear_nonpoint, NEIByYear_onroad, NEIByYear_nonroad )
  
  print("Starting Plotting")
  
  print(qplot(year, Emissions, data=NEIByYear_aggregates, colour = type, geom=c("line", "point"), main="Total PM2.5 Emissions in Baltimore per Type", ylab = "Emissions in tons") + scale_x_continuous(breaks = sequence(1999:2008)))
  dev.copy(png, file=file.path(path, "Plot3.png"), width=640, height=640)
  dev.off()
  
  ## Now all should be finished
  print("Finished!")
  }