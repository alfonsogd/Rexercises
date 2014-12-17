## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
########### Subset para pruebas
#s1 <- NEI[sample(nrow(NEI), 1000, replace = FALSE),]
#Emissions <- c(mean(s1[s1$year == 1999,4],trim=0.1),mean(s1[s1$year == 2002,4],trim=0.1),
#               mean(s1[s1$year == 2005,4],trim=0.1),mean(s1[s1$year == 2008,4],trim=0.1))
#years <- c(1999,2002,2005,2008)
#Emybyyear <- data.frame(years, Emissions)
#modelito <- lm(Emissions ~ years, data = Emybyyear)
############## Plots para subset
#par(mfrow =c(1,2), oma = c(0,0,2,0))
#with(s1,{ plot(year, Emissions, ylim = c(0,200), main="Every fip by year", xlab="Year", ylab="Emissions in tons")
#          mtext("Emissions by year in the USA", outer = TRUE)})
#plot (years,Emissions,main="Mean of emissions by year", xlab="year", ylab="Mean in tons",type="l")
#abline(modelito, col="blue")
#legend("topright", pch = "-", col =("blue"),legend = ("tendency"))
########### Base completa, 1a pregunta: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
########### Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of 
########### the years 1999, 2002, 2005, and 2008.
Emissions <- c(mean(NEI[NEI$year == 1999,4],trim=0.1),mean(NEI[NEI$year == 2002,4],trim=0.1),
               mean(NEI[NEI$year == 2005,4],trim=0.1),mean(NEI[NEI$year == 2008,4],trim=0.1))
years <- c(1999,2002,2005,2008)
Emybyyear <- data.frame(years, Emissions)
modelito <- lm(Emissions ~ years, data = Emybyyear)
########### Plots para base completa
par(mfrow =c(1,2), oma = c(0,0,2,0))
with(NEI,{ plot(year, Emissions, ylim = c(0,100000), main="Every fip by year", xlab="Year", ylab="Emissions in tons")
          mtext("Emissions by year in the USA", outer = TRUE)})
plot (years,Emissions,main="Mean of emissions by year", xlab="year", ylab="Mean in tons",type="l")
abline(modelito, col="blue")
legend("topright", pch = "-", col =("blue"),legend = ("tendency"))
######### 2a pregunta: Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == 24510) from 1999 to 2008? 
######### Use the base plotting system to make a plot answering this question
Baltimore <- NEI[NEI$fips=="24510",]
Emissions <- c(mean(s1[s1$year == 1999,4],trim=0.1),mean(s1[s1$year == 2002,4],trim=0.1),
               mean(s1[s1$year == 2005,4],trim=0.1),mean(s1[s1$year == 2008,4],trim=0.1))
years <- c(1999,2002,2005,2008)
Emybyyear <- data.frame(years, Emissions)
modelBal <- lm(Emissions ~ years, data = Emybyyear)
############## Plots para subset
par(mfrow =c(1,2), oma = c(0,0,2,0))
with(Baltimore,{ plot(year, Emissions, ylim = c(0,200), main="Every fip by year", xlab="Year", ylab="Emissions in tons")
          mtext("Emissions by year in Baltimore, Maryland", outer = TRUE)})
plot (years,Emissions,main="Mean of emissions by year", xlab="year", ylab="Mean in tons",type="l")
abline(modelBal, col="red")
legend("topright", pch = "-", col =("red"),legend = ("tendency"))