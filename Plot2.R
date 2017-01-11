Plot2 <- function(){
  
  ## Preparations: load necessary libraries, get/set paths, load, unzip and filter data:
  
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

  ## Total PM 2.5 emissions for Baltimore (fips==24510) from 1999 to 2008

  NEIByYear <- aggregate(Emissions ~ year, data = subset(NEI, fips == "24510"), sum)
  NEIByYear$Emissions<-NEIByYear$Emissions/1000
  
  print("Starting Plotting")
  
  with(NEIByYear, plot(year, Emissions, type = "h", main = "Total PM2.5 Emissions over Years in Baltimore \n fips==24510", xlab="Years", ylab = "Emissions in kilotons", xlim=c(1999, 2008), ylim=c(0, 4), col = "blue"))
  with(NEIByYear, lines(year, Emissions, type = "b", col = "red"))
  axis(1, at=seq(1999, 2008, by=1))
  dev.copy(png, file=file.path(path, "Plot2.png"), width=640, height=640)
  dev.off()
  
  ## Now all should be finished
  print("Finished!")

  }