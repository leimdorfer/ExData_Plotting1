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

plot1 <- function(){
      
      # cullfat() #run once to create smaller csv file
      # makeTimestamp() # run once to format Date-Time     
      
      df <- read.csv("./data.csv")
      
      df.sub <- df[,"Global_active_power"]
      
      png(filename = "plot1.png",
          width = 480, height = 480, units = "px", pointsize = 12,
          bg = "white",  res = NA,
          type = "quartz")
      
      
      hist(df.sub,
           breaks = 12,
           freq = TRUE,
           main = "Global Active Power",
           xlab = "Global Active Power (kilowatts)",
           col = "red", border = "black")

      dev.off()
     
}