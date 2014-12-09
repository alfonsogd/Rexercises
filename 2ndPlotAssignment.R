## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
#s1 <- sample(NEI, size=1000, replace=FALSE)
#boxplot(Emissions ~ year, s1, ylab="Emissions", xlab="year") 
#with (s1, plot(year, Emissions, main="Emissions by year"))
#with (s1, plot(Emissions ~ as.factor(year), type="h"))
yylabels <-c(1999,2002,2005,2008)
valemiss <- c(sum(NEI[NEI$year == 1999,4]),sum(NEI[NEI$year == 2002,4]),
              sum(NEI[NEI$year == 2005,4]),sum(NEI[NEI$year == 2008,4]))
plot (yylabels,valemiss,main="Emissions in the USA", xlab="year", ylab="Emissions in tons",type="l")
