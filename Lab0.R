# Lab 0
source("http://www.openintro.org/stat/data/present.R")
present
dim(present)
names(present)
table(present$year)
present$girls
plot (x = present$year, y = present$girls, type="l")
present$boys + present$girls
plot (present$year, present$boys + present$girls, type = "l")
?max
mayor <- max(present$boys + present$girls)
present$year[present$boys+present$girls==mayor]
plot (present$year, present$boys / (present$boys + present$girls), type ="l")
abline(lm(present$boys / (present$boys + present$girls, present$year)))
