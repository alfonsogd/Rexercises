## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
s1 <- NEI[sample(nrow(NEI), 1000, replace = FALSE),]
#with (NEI, plot(as.factor(year), Emissions, ylim = c(0,100000), main="Emissions by year"))
with (s1, plot(as.factor(year), Emissions, ylim = c(0,200), main="Emissions by year"))
#m1  = lm(Emissions ~ year, data = NEI)
m1  = lm(year ~ Emissions, data = s1)
#abline(m1, col="blue")
#abline(m1, col="blue")
#with (s1, plot(Emissions ~ as.factor(year), type="h"))
years <- c(1999,2002,2005,2008)
Emmissions <- c(sum(s1[s1$year == 1999,4]),sum(s1[s1$year == 2002,4]),
                sum(s1[s1$year == 2005,4]),sum(s1[s1$year == 2008,4]))
Emybyyear <- data.frame(years, Emmissions)
#yylabels <-c(1999,2002,2005,2008)
#valemiss <- c(sum(NEI[NEI$year == 1999,4]),sum(NEI[NEI$year == 2002,4]),
#              sum(NEI[NEI$year == 2005,4]),sum(NEI[NEI$year == 2008,4]))
#plot (yylabels,valemiss,main="Emissions in the USA", xlab="year", ylab="Emissions in tons",type="l")
plot (years,Emmissions,main="Emissions in the USA", xlab="year", ylab="Emissions in tons",type="l")
