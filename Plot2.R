library(dplyr)
library(stringr)

##FUNCTIONS DEFINITIONS
#Read Data Function
readData <- function(path, fileName) {
  
  file <- paste(path,"//", fileName, sep = "")
  dat <- read.table(file, header = TRUE, sep = ";")
  dat
}

#Save Graph as PNG Image Function
saveGraphAsPNG <- function(path, fileName) {
  dev.copy(png, file = paste(path,"//", fileName,".png", sep = ""), width = 480, height = 480) 
  dev.off()
}
################

##LOAD DATA SET 
rootPath <- "D://Arquivos de Projetos//[R] Codes//Exploring_Data_Analysis"
flName <- "household_power_consumption.txt"
startDate <- "1/2/2007"
endDate <-  "2/2/2007"

powerConsumptionTable <- readData(rootPath, flName)
aux <- filter(powerConsumptionTable, Date == startDate | Date == endDate)

y <- transform(aux, Global_active_power   = as.numeric(as.character(Global_active_power)),
               Global_reactive_power = as.numeric(as.character(Global_reactive_power)),
               Voltage               = as.numeric(as.character(Voltage)),
               Global_intensity      = as.numeric(as.character(Global_intensity)),
               Sub_metering_1        = as.numeric(as.character(Sub_metering_1)),
               Sub_metering_2        = as.numeric(as.character(Sub_metering_2)))


####################################################

##Plotting 2 - Global Active Power (kilowatts) x Date/Time

plot(strptime(paste(y$Date," ",y$Time), format= "%d/%m/%Y %H:%M:%S"), 
     y$Global_active_power, 
     type="l",
     ylab = "Global Active Power (kilowatts)",
     xlab = "")

saveGraphAsPNG(rootPath, "Plot2")
