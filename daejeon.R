#Adatok beolvasasa
setwd("C:/users/kochd/Desktop/WiP Szakdoga/Adatok")
daejeon <- read.csv2("daejeon_r.csv")

daejeon[daejeon$Heating.type == 0,]$Heating.type <- "Gas"
daejeon[daejeon$Heating.type == 1,]$Heating.type <- "Other"
daejeon$Heating.type <- as.factor(daejeon$Heating.type)

#Abrak
boxplot(daejeon)
hist(daejeon$Condominium.price, 100, xlim=c(0, 60000))


#Teszt modell
fit1 <- lm(Condominium.price ~ Size.of.unit + Floor + Construction.year + Network.distance.to.nearest.subway.station + Number.of.households +
             Heating.type + Number.of.top.university.entrants + Number.of.high.schools + Network.distance.to.the.CBD + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront
           + Number.of.bus.stops + Population + Sex.ratio + Medium.age + Young.population.ratio + Old.population.ratio + Population.density + Ratio.of.adults.with.higher.degrees, data = daejeon)

fit2 <- lm(log(Condominium.price) ~ Size.of.unit + Floor + Construction.year + log(Network.distance.to.nearest.subway.station) + Number.of.households + 
             Heating.type + Number.of.high.schools + Network.distance.to.the.CBD + log(Network.distance.to.nearest.waterfront)
           + Number.of.bus.stops, data = daejeon)

fit3 <- lm(Condominium.price ~ Size.of.unit + Floor + Construction.year + Network.distance.to.nearest.subway.station + Number.of.households + 
             Heating.type + Number.of.high.schools + Network.distance.to.the.CBD + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront
           + Number.of.bus.stops + Population + Young.population.ratio + Old.population.ratio + Population.density + Ratio.of.adults.with.higher.degrees, data = daejeon)

summary(fit1)
summary(fit2)
summary(fit3)

#Multikollienaritas
install.packages("corrplot")
library("corrplot")
corrplot(cor(daejeon), method = "number")
corrplot(daejeon_es_keruletek[4:21],method = "number" )

install.packages("olsrr")
library("olsrr")
ols_vif_tol(regresszio)

#Heteroszkedaszticitas
install.packages("lmtest")
library("lmtest")
lmtest::bptest(fit2)

res <- resid(fit2)
plot(fitted(fit2), res)
ols_plot_resid_qq(fit2)
hist(res, 100)

#Terbeli autokorrelacio
library(sf)
library(dplyr)
library(ggplot2)
library(sp)
library(spatstat)
library(spdep)
library(ncf)

shape <- st_read("C:/Users/kochd/Desktop/WiP Szakdoga/Adatok/Shape files/Daejeon_condominiums.shp")
clean_shape <- st_read("C:/Users/kochd/Desktop/WiP Szakdoga/DaejeonMoran.shp")

coord <- data.frame(cbind(clean_shape$Longitude, clean_shape$Latitude))
colnames(coord) <- c("x", "y")

w <- 1/as.matrix(dist(coordinates(coord)))
diag(w) <- 0
nb <- tri2nb(coord)
daejeon.geoms <- st_as_sf(coord, coords = c("x", "y"))

moran.test(log(clean_shape$Condominiu), nb2listwdist(nb, daejeon.geoms, type = "idw", style = "W"))

#Hedonikus regresszio - adatok 
install.packages("tidyverse")
library(tidyverse)

setwd("C:/Users/kochd/Desktop/WiP Szakdoga/Adatok/Vegleges_shape/Daejeon")
kerulet_nevek <- read.csv2("kerulet_vektor.csv")
daejeon_es_keruletek <- read.csv2("Daejeon_fofajl_es_kerulet.csv")

daejeon_es_keruletek$Heating.type <- as.factor(daejeon_es_keruletek$Heating.type)
daejeon_es_keruletek$Network.distance.to.nearest.greenspace <- log(daejeon_es_keruletek$Network.distance.to.nearest.greenspace)
daejeon_es_keruletek$Network.distance.to.nearest.waterfront <- log(daejeon_es_keruletek$Network.distance.to.nearest.waterfront)
daejeon_es_keruletek$Network.distance.to.nearest.subway.station <- log(daejeon_es_keruletek$Network.distance.to.nearest.subway.station)
daejeon_es_keruletek[sapply(daejeon_es_keruletek, is.infinite)] <- 0

regresszio <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools +Construction.year + Number.of.households +
                   Heating.type + Number.of.top.university.entrants + Number.of.bus.stops + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront
                 + Parking.space.per.household  + Population.density,
                 data = subset(daejeon_es_keruletek, Kerulet == as.vector(kerulet_nevek %>% slice (1))))
reg1_coef_daejeon <- data.frame(regresszio$coefficients)
write.csv2(reg1_coef_daejeon, "reg1_coef_daejeon.csv")

regresszio2 <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools +Construction.year + Number.of.households +
                    Heating.type + Number.of.top.university.entrants + Number.of.bus.stops + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront
                  + Parking.space.per.household  + Population.density,
                  data = subset(daejeon_es_keruletek, Kerulet == as.vector(kerulet_nevek %>% slice (2))))
reg2_coef_daejeon <- data.frame(regresszio2$coefficients)
write.csv2(reg2_coef_daejeon, "reg2_coef_daejeon.csv")

regresszio3 <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools +Construction.year + Number.of.households +
                    Heating.type + Number.of.top.university.entrants + Number.of.bus.stops + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront
                  + Parking.space.per.household  + Population.density,
                  data = subset(daejeon_es_keruletek, Kerulet == as.vector(kerulet_nevek %>% slice (3))))
reg3_coef_daejeon <- data.frame(regresszio3$coefficients)
write.csv2(reg3_coef_daejeon, "reg3_coef_daejeon.csv")

regresszio4 <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools +Construction.year + Number.of.households +
                    Heating.type + Number.of.top.university.entrants + Number.of.bus.stops + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront
                  + Parking.space.per.household  + Population.density,
                  data = subset(daejeon_es_keruletek, Kerulet == as.vector(kerulet_nevek %>% slice (4))))

reg4_coef_daejeon <- data.frame(regresszio4$coefficients)
write.csv2(reg4_coef_daejeon, "reg4_coef_daejeon.csv")

regresszio5 <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools +Construction.year + Number.of.households +
                    Heating.type + Number.of.top.university.entrants + Number.of.bus.stops + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront
                  + Parking.space.per.household  + Population.density,
                  data = subset(daejeon_es_keruletek, Kerulet == as.vector(kerulet_nevek %>% slice (5))))

reg5_coef_daejeon <- data.frame(regresszio5$coefficients)
write.csv2(reg5_coef_daejeon, "reg5_coef_daejeon.csv")

#GWR
install.packages("spgwr")
install.packages("ggplot2")
library(spgwr)
library(ggplot2)

setwd("C:/users/kochd/Desktop/WiP Szakdoga/Adatok")
daejeon_r2 <- read.csv2("daejeon_r2.csv")

daejeon_r2$Heating.type <- as.factor(daejeon_r2$Heating.type)
daejeon_r2$Network.distance.to.nearest.greenspace <- log(daejeon_r2$Network.distance.to.nearest.greenspace)
daejeon_r2$Network.distance.to.nearest.waterfront <- log(daejeon_r2$Network.distance.to.nearest.waterfront)
daejeon_r2$Network.distance.to.nearest.subway.station <- log(daejeon_r2$Network.distance.to.nearest.subway.station)
daejeon_r2[sapply(daejeon_r2, is.infinite)] <- 0

coord <- data.frame(cbind(daejeon_r2$Longitude,daejeon_r2$Latitude))
koord_daejeon <- SpatialPointsDataFrame(coord, data =daejeon_r2)

install.packages("GWmodel")
library(GWmodel)

gwrBandwidth2 <-bw.gwr(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools +Construction.year + Number.of.households +
                         Heating.type + Number.of.top.university.entrants + Number.of.bus.stops + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront
                       + Parking.space.per.household  + Population.density, data = koord_daejeon, approach = "AIC", kernel = "bisquare", adaptive =TRUE, longlat=TRUE, parallel.method = "omp")


GWR_2 <- gwr.basic(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools +Construction.year + Number.of.households +
                     Heating.type + Number.of.top.university.entrants + Number.of.bus.stops + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront
                   + Parking.space.per.household  + Population.density, data = koord_daejeon, bw = gwrBandwidth2, kernel = "bisquare", adaptive = TRUE, longlat = TRUE, parallel.method = "omp")

gwr2 <- as.data.frame(GWR_2$SDF)
write.csv2(gwr2, "daejeon_GWR_estimates.csv")

#Referencia globalis OLS
global2 <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools +Construction.year + Number.of.households +
                Heating.type + Number.of.top.university.entrants + Number.of.bus.stops + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront
              + Parking.space.per.household  + Population.density, data = daejeon_r2)

summary(global2)
global2_write <- as.data.frame(global2$coefficients)
write.csv2(global2_write, "daejeon_global_OLS.csv")







































































#
#Shape file exportalasa
#Vegre es az eredetei daejeon shp osszekombinalasa qgis-be koordinatak alapjan
#Daejeon polygon fajl es a kombinalt pont fajl koordinata alapon valo eggyesitese
#
library(sf)
#library(tidyverse)
#library(mapview)
#library(rgdal)
#library(dplyr)
#
#
setwd("C:/users/kochd/Desktop/WiP Szakdoga/Adatok/")
points <- read.csv2("pontok_daegu.csv") %>%
  st_as_sf(coords = c(2,3),
          crs = 5181)

points$Condominium.price <- as.integer(points$Condominium.price)

st_write(points, "daegu_arindex.shp")

















