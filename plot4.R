cullfat <- function(){
    
      df <- read.table("./household_power_consumption.txt", 
                       header = TRUE,  
                       sep = ";",
                       colClasses = "character",
                       na.strings = "?")
      
      df.sub <- subset(df, Date == "1/2/2007" | Date == "2/2/2007")
      
      
      write.csv(df.sub, file = "data.csv")
      
      rm(df)
}

makeTimestamp <- function(){
      
      # Creates a nicely formatted "timestamp field to replace "Date" and "Time"
      
      df <- read.csv("./data.csv")
      
      df$timestamp <- paste(df$Date, df$Time)
      
      # remove unnecesary columns
      df$Date <- NULL
      df$Time <- NULL
      
      #View(df)
      
      write.csv(df, file = "data1.csv")
}
         
plot4 <- function(){
      
      # cullfat() #run once to create smaller csv file
      # makeTimestamp() # run once to format Date-Time
      
      df1 <- read.csv("./data1.csv")
            
      #################### PNG
      
      png(filename = "plot4.png",
          width = 480, height = 480, units = "px", pointsize = 12,
          bg = "white",  res = NA,
          type = "quartz")
      
      #################### Plot     
      
      df1$DateTime <- as.POSIXct(df1$timestamp, format="%d/%m/%Y %H:%M:%S")
      daterange = c(as.POSIXlt(min(df1$DateTime)), as.POSIXlt(max(df1$DateTime)))
            
      par(mfrow=c(2,2),                        
            p1.y <- df1$Global_active_power,         
            p2.y <- df1$Voltage,
            p3.x <- df1$timestamp,
            p3.y1 <- df1$Sub_metering_1,
            p3.y2 <- df1$Sub_metering_2,
            p3.y3 <- df1$Sub_metering_3,
            p4.y <- df1$Global_reactive_power
      )
      
      #p1
      
      plot(p1.y ~ DateTime, df1, xaxt = "n", 
           type = "l", 
           ylab = "Global Active Power (kilowatts)")
      axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="day"), format="%a")
      
      #p2
      
      plot(p2.y ~ DateTime, df1, xaxt = "n", 
           type = "l",
           ylab = "Voltage")      
      axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="day"), format="%a")
            
      #p3

      plot_range <- range(0, p3.y1, p3.y2, p3.y3)
      
      plot(p3.y1 ~ DateTime, df1, type="l", ylim = plot_range, xaxt = "n",
           ylab = "Energy sub metering")
      lines(p3.y2 ~ DateTime, df1, type="l", lty=1, col="red")
      lines(p3.y3 ~ DateTime, df1, type="l", lty=1, col="blue")
      
      axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="day"), format="%a")
      
      #p4
      
      plot(p4.y ~ DateTime, df1, type = "l", xaxt = "n",
           ylab = "Global reactive Power")
      
      axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="day"), format="%a")
  
      #################### End Plots
      
      dev.off()
     
}