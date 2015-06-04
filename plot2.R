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
         
plot2 <- function(){
      
      # cullfat() #run once to create smaller csv file
      # makeTimestamp() # run once to format Date-Time
      
      #################### PNG
      
      png(filename = "plot2.png",
      width = 480, height = 480, units = "px", pointsize = 12,
      bg = "white",  res = NA,
      type = "quartz")
      
      #################### Plot
      
      df1 <- read.csv("./data1.csv")    
      
      df1$DateTime <- as.POSIXct(df1$timestamp, format="%d/%m/%Y %H:%M:%S")
      daterange=c(as.POSIXlt(min(df1$DateTime)), as.POSIXlt(max(df1$DateTime)))
      
      y <- df1$Global_active_power
      
      plot(y ~ DateTime, df1, xaxt = "n", 
           type = "l", 
           ylab = "Global Active Power (kilowatts)")
      
      
      axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="day"), format="%a")
      
      
      #################### End Plot

      dev.off()
     
}