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

par(mfrow = c(2, 2))
with(y, 
{
  plot(strptime(paste(Date," ",Time), format= "%d/%m/%Y %H:%M:%S"), 
       y$Global_active_power, 
       type="l",
       cex.lab = 0.7,
       ylab = "Global Active Power (kilowatts)",
       xlab = ""
      )
  plot(strptime(paste(Date," ",Time), format= "%d/%m/%Y %H:%M:%S"), 
       Voltage, 
       type="l",
       cex.lab = 0.7,
       ylab = "Global Active Power (kilowatts)",
       xlab = "datetime"
      )
  
  plot(strptime(paste(y$Date," ",y$Time), format= "%d/%m/%Y %H:%M:%S"), 
       y$Sub_metering_1, 
       type="l",
       cex.lab = 0.7,
       ylab = "Energy Sub Metering",
       xlab = "",
       col = "black")
  lines(strptime(paste(y$Date," ",y$Time), format= "%d/%m/%Y %H:%M:%S"), 
        y$Sub_metering_2,
        col = "red")
  lines(strptime(paste(y$Date," ",y$Time), format= "%d/%m/%Y %H:%M:%S"), 
        y$Sub_metering_3,
        col = "blue")
  legend("topright", 
         lty = 1,
         xjust = 0.5,
         cex = 0.2,
         col = c("black", "red", "blue"), 
         legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
        )
  
  plot(strptime(paste(Date," ",Time), format= "%d/%m/%Y %H:%M:%S"), 
       Global_reactive_power, 
       type="l",
       cex.lab = 0.7,
       ylab = "Global_reactive_power",
       xlab = "datetime"
  )
}
)

saveGraphAsPNG(rootPath, "Plot4")


