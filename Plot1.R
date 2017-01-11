Plot1 <- function(){
  
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

  ## Total emissions from PM2.5 in the US over 1999, 2002, 2005 and 2008 in megatons (division by 1000000)

  NEIByYear <- aggregate(Emissions ~ year, NEI, sum)
  NEIByYear$Emissions<-NEIByYear$Emissions/1000000
  
  print("Starting Plotting")
        
  with(NEIByYear, plot(year, Emissions, type = "h", main = "Total PM2.5 Emissions over Years in the US", xlab="Years", ylab = "Emissions in megatons", xlim=c(1999, 2008), ylim=c(0, 8), col = "blue"))
  with(NEIByYear, lines(year, Emissions, type = "b", col = "red"))
  axis(1, at=seq(1999, 2008, by=1))
  dev.copy(png, file=file.path(path, "Plot1.png"), width=640, height=640)
  dev.off()
  
  ## Now all should be finished
  print("Finished!")
}