# read data
household_power_consumption <- read.csv("~/Rall/household_power_consumption.txt", 
                                        sep=";", na.strings="?")
hpc <- read.csv.sql("~/Rall/household_power_consumption.txt", sql = "select * from file where Date >= '2007-02-01'", 
                    header= TRUE, sep=";", 
                    colClasses=c(as.Date,as.Date, "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric"))
# aqui muestro una fecha y hora convertidas
print(strptime(household_power_consumption$Time[[1]],"%T"))
print(strptime(household_power_consumption$Date[[1]],"%d/%m/%Y"))
print(as.Date(household_power_consumption$Date[[1]],"%d/%m/%Y"))
hist(household_power_consumption$Global_active_power, col="red",
     main="Global Active Power", 
     xlab= "Global Active Power (Kilowatts)")
