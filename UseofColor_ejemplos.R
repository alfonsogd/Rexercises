#### Ejercicios con colores 
library(grDevices)
library(graphics)
library(RColorBrewer)
filled.contour(volcano, color.palette = terrain.colors, asp = 1)
title(main = "volcano data: filled contour map")
filled.contour(volcano, color.palette = heat.colors, asp = 1)
filled.contour(volcano, color.palette = topo.colors, asp = 1)

cols <- brewer.pal(3,"BuGn")
pal <- colorRampPalette(cols)
image(volcano, col = pal(650))
