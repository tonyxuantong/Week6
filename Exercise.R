library(sp)
library(rgdal)
library(rgeos)
library(maptools)

N_railways <- readOGR("netherlands-railways-shape",
                            "railways")
N_place <- readOGR("netherlands-places-shape",
                            "places")

prj_new <- CRS("+proj=longlat +datum=WGS84")
N_railways_2 <- spTransform(N_railways, CRS("+init=epsg:28992"))
N_place_2 <- spTransform(N_place, CRS("+init=epsg:28992"))

head(N_railways_2@data)
head(N_place_2@data)
industrial_railway <- N_railways_2[N_railways_2$type == "industrial"]
jpeg("industrial_railway.jpg")
plot(industrial_railway, lwd = 1, main = "Industrial Railways in Netherland")

buffzone <- gBuffer(industrial_railway, width=1000, byid=TRUE)
plot(buffzone, add = TRUE, lty = 3, lwd = 2, col = "green")


N_intersection <- gIntersection(N_place_2, buffzone)
plot(N_intersection, add = TRUE, col = "red")
jpeg("places_railway_intersects.jpg")
dev.off()


