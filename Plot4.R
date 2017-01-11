Plot4 <- function(){
  
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

  ## US coal-combustion-caused emissions:

  ## I analyzed the raw data and am not entirely sure if picking any row which contains the string "coal" is actually what
  ## we need to select, but I had to take the assumption that in the end, this is what we are supposed to analyze. Any other
  ## string would also do, but unfortunately, this part of the task description was not entirely clear to me.
  ## With the below data selections, I identify the rows containing "coal" first, then I transfer the according documents (based on the search result) into
  ## object "NEI_coal". So NEI_coal_cases" is just an intermediate step.
  
  if(!exists("NEI_SCC")) {NEI_SCC <- merge(NEI, SCC)}
  if(!exists("NEI_coal_cases")) {NEI_coal_cases <- grepl("coal", NEI_SCC$Short.Name, ignore.case=TRUE)}
  if(!exists("NEI_coal")) {NEI_coal <- NEI_SCC[NEI_coal_cases, ]}
  
  ##Aggregation over all entries:
  NEIByYear_aggregates <- aggregate(Emissions ~ year, data = NEI_coal, sum)
  
  print("Starting Plotting")
  
  print(qplot(year, Emissions, data=NEIByYear_aggregates, geom=c("line", "point"), ylab="Emissions in tons", ylim=c(0,700000), legend = FALSE, main="Total US Coal-related PM2.5 Emissions 1999-2008", col = "red") + 
        scale_x_continuous(breaks = sequence(1999:2008)) +
        theme(legend.position = "none"))
  dev.copy(png, file=file.path(path, "Plot4.png"), width=640, height=640)
  dev.off()
  
  ## Now all should be finished
  print("Finished!")
  }