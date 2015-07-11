plot1 <- function(){
        hpc <- read.table("household_power_consumption.txt", sep = ";", header = TRUE)  # read the source data and store it in variable hpc
        hpc$timestamp = strptime(paste(hpc$Date, hpc$Time), "%d/%m/%Y %H:%M:%S")        # add a column which combines date and time
        hpc$used = (hpc$timestamp > "2007-02-01" & hpc$timestamp < "2007-02-03")        # mark the columns which will be analyzed
        hpc1 <- hpc[hpc$used == TRUE,]                                                  # delete unwanted dates and store result in varialbe hpc1
        hpc1$na = is.na(hpc1$timestamp)        	                                        # add a column which marks na-values
        hpc2 <- hpc1[hpc1$na == FALSE,]                                                 # delete rows which have invalid timestamps and store result in variable hpc2 - continue to plot the variable hpc2
        hpc2$Global_active_power = as.numeric(hpc2$Global_active_power)                  # format column
        hpc2$Global_reactive_power = as.numeric(hpc2$Global_reactive_power)              # format column
        hpc2$Voltage = as.numeric(hpc2$Voltage)                                          # format column
        hpc2$Global_intensity = as.numeric(hpc2$Global_intensity)                        # format column
        hpc2$Sub_metering_1 = as.numeric(hpc2$Sub_metering_1)                            # format column
        hpc2$Sub_metering_2 = as.numeric(hpc2$Sub_metering_2)                            # format column
        hpc2$Sub_metering_3 = as.numeric(hpc2$Sub_metering_3)                            # format column
        hpc2$timestamp = as.POSIXct(hpc2$timestamp)                                     # format column
        
        png(file = "plot1.png")
        hist(hpc2$Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)", width = 480, height = 480)
        dev.off()
}