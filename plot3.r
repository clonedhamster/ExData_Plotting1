library("data.table")

read.my.data <- function(filename ) {
  # read the data set and remove ?
  #dataset <- fread(filename, na.strings=c('?'), colClasses=c(1="integer"))
  filename <- 'household_power_consumption.txt'
  dataset <- fread(filename, na.strings=c('NA'), colClasses=c("character"))
  
  # Dates according to assignment
  # Find the first entry of 2007-02-01
  start.date <- grep('^1/2/2007', dataset$Date)[1]
  # Find the last entry of 2007-02-02
  end.date <- tail(grep('^2/2/2007', dataset$Date), n = 1)
  # Subset the data 
  subsetted.data <- as.data.frame(dataset[start.date:end.date,])
  # clean out what's not needed
  remove(dataset)
  
  # Typecaste Date and Time column to posix time class
  datedata <- paste(subsetted.data$Date, subsetted.data$Time, sep = '-')
  subsetted.data$Date <- as.POSIXct(datedata, format = '%d/%m/%Y-%H:%M:%S')
  
  for(i in seq(from = 3, to = 9)) {
    subsetted.data[, i] <- as.numeric(subsetted.data[, i])
  }
  
  
  subsetted.data <- cbind(Date = subsetted.data$Date, subsetted.data[, 3:9])
  return(subsetted.data)
}


data <- read.my.data('household_power_consumption.txt')

# write png file
png(file = "plot3.png", bg = "transparent", width = 480, height = 480)
# create the plot
plot(data$Date, data$Sub_metering_1, type = 'l', xlab = '', ylab = 'Energy sub metering')
lines(data$Date, data$Sub_metering_2, col = 2)
lines(data$Date, data$Sub_metering_3, col = 4)
legend('topright', c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'), col = c(1,2,4), lwd = 1)
# close the device
dev.off()
