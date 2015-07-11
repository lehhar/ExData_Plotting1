plot4 <- function(){
        hpc <- read.table("household_power_consumption.txt", sep = ";", header = TRUE)  # read the source data and store it in variable hpc
        hpc$timestamp = strptime(paste(hpc$Date, hpc$Time), "%d/%m/%Y %H:%M:%S")        # add a column which combines date and time
        hpc$used = (hpc$timestamp > "2007-02-01" & hpc$timestamp < "2007-02-03")        # mark the columns which will be analyzed
        hpc1 <- hpc[hpc$used == TRUE,]                                                  # delete unwanted dates and store result in varialbe hpc1
        hpc1$na = is.na(hpc1$timestamp)                                                        # add a column which marks na-values
        hpc2 <- hpc1[hpc1$na == FALSE,]                                                 # delete rows which have invalid timestamps and store result in variable hpc2 - continue to plot the variable hpc2
        hpc2$Global_active_power = as.numeric(hpc2$Global_active_power)                  # format column
        hpc2$Global_reactive_power = as.numeric(hpc2$Global_reactive_power)              # format column
        hpc2$Voltage = as.numeric(hpc2$Voltage)                                          # format column
        hpc2$Global_intensity = as.numeric(hpc2$Global_intensity)                        # format column
        hpc2$Sub_metering_1 = as.numeric(hpc2$Sub_metering_1)                            # format column
        hpc2$Sub_metering_2 = as.numeric(hpc2$Sub_metering_2)                            # format column
        hpc2$Sub_metering_3 = as.numeric(hpc2$Sub_metering_3)                            # format column
        hpc2$timestamp = as.POSIXct(hpc2$timestamp)                                      # format column
        
        png(file = "plot4.png", width = 480, height = 480)
        
        par(mfrow = c(2,2))                                                             # to plot 4 charts, 2 rows and 2 columns
        
        plot(hpc2$timestamp, hpc2$Global_active_power, type="l", ylab = "Global Active Power (kilowatts)", xlab = """)
        
        plot(hpc2$timestamp, hpc2$Voltage, type="l", ylab = "Voltage", xlab = "")
        
        
        x <- hpc2$timestamp
        y1 <- hpc2$Sub_metering_1
        y2 <- hpc2$Sub_metering_2
        y3 <- hpc2$Sub_metering_3
        
        plot(x, y1, type="l", col = "black", ylab = "Energy sub metering", xlab = "")
        lines(x, y2, col="red")
        lines(x, y3, col="blue")
        
        legend("topright", , lty=1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), cex = 1)
        
        plot(hpc2$timestamp, hpc2$Global_reactive_power, type="l", ylab = "Global_reactive_power (kilowatts)", xlab = "")
        
        dev.off()
}