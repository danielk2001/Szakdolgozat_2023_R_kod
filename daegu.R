#Adatok beolvasasa
setwd("C:/users/kochd/Desktop/WiP Szakdoga/Adatok")
daegu <- read.csv2("daegu_r.csv")

daegu[daegu$Heating.type == 0,]$Heating.type <- "Gas"
daegu[daegu$Heating.type == 1,]$Heating.type <- "Other"
daegu$Heating.type <- as.factor(daegu$Heating.type)

daegu$Network.distance.to.nearest.greenspace <- log(daegu$Network.distance.to.nearest.greenspace)
daegu$Network.distance.to.nearest.waterfront <- log(daegu$Network.distance.to.nearest.waterfront)
daegu$Network.distance.to.nearest.subway.station <- log(daegu$Network.distance.to.nearest.subway.station)
daegu$Network.distance.to.the.CBD <- log(daegu$Network.distance.to.the.CBD)
daegu[sapply(daegu, is.infinite)] <- 0


#Abrak
boxplot(daegu)

#Teszt modell
fit1 <- lm(Condominium.price ~ Size.of.unit + Floor + Construction.year + Network.distance.to.nearest.subway.station + Number.of.households +
              Highest.floor + Parking.space.per.household + Heating.type + Number.of.top.university.entrants + Number.of.high.schools + Network.distance.to.the.CBD + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront
           + Number.of.bus.stops + Population.density + Ratio.of.adults.with.higher.degrees, data = daegu)

fit2 <- lm(log(Condominium.price) ~ Size.of.unit + Floor + Construction.year + Network.distance.to.nearest.subway.station + Number.of.households +
             Highest.floor + Parking.space.per.household + Heating.type + Number.of.top.university.entrants + Number.of.high.schools + Network.distance.to.the.CBD + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront
           + Number.of.bus.stops + Population.density + Ratio.of.adults.with.higher.degrees, data = daegu)

summary(fit1)
summary(fit2)

#Multikollienaritas
install.packages("corrplot")
library("corrplot")
corrplot(cor(daegu), method = "number")

install.packages("olsrr")
library("olsrr")
ols_vif_tol(fit2)

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

shape <- st_read("C:/Users/kochd/Desktop/WiP Szakdoga/Adatok/Shape files/Daegu_condominiums.shp")
clean_shape <- st_read("C:/Users/kochd/Desktop/WiP Szakdoga/Adatok/Moran´s I/Daegu/MoranI_daegu.shp")

coord <- data.frame(cbind(clean_shape$Longitude, clean_shape$Latitude))
colnames(coord) <- c("x", "y")

w <- 1/as.matrix(dist(coordinates(coord)))
diag(w) <- 0
nb <- tri2nb(coord)
daegu.geoms <- st_as_sf(coord, coords = c("x", "y"))

moran.test(log(clean_shape$Condominiu), nb2listwdist(nb, daegu.geoms, type = "idw", style = "W"))

#Hedonikus regresszio - adatok 
install.packages("tidyverse")
library(tidyverse)

setwd("C:/Users/kochd/Desktop/WiP Szakdoga/Adatok/Vegleges_shape/Daegu")
kerulet_nevek <- read.csv2("Kerulet_vektor_daegu_jo.csv")
fofajl_es_keruleteik_daegu <- read.csv2("fofajl_es_keruleteik_daegu.csv")

fofajl_es_keruleteik_daegu$Heating.type <- as.factor(fofajl_es_keruleteik_daegu$Heating.type)
fofajl_es_keruleteik_daegu$Network.distance.to.nearest.greenspace <- log(fofajl_es_keruleteik_daegu$Network.distance.to.nearest.greenspace)
fofajl_es_keruleteik_daegu$Network.distance.to.nearest.waterfront <- log(fofajl_es_keruleteik_daegu$Network.distance.to.nearest.waterfront)
fofajl_es_keruleteik_daegu$Network.distance.to.nearest.subway.station <- log(fofajl_es_keruleteik_daegu$Network.distance.to.nearest.subway.station)
fofajl_es_keruleteik_daegu[sapply(fofajl_es_keruleteik_daegu, is.infinite)] <- 0

regresszio <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + 
                   Heating.type + Number.of.top.university.entrants + Number.of.bus.stops + Network.distance.to.the.CBD + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.subway.station
                 + Parking.space.per.household  + Population.density,
   data = subset(fofajl_es_keruleteik_daegu, Kerulet == as.vector(kerulet_nevek %>% slice (1))))
  
reg1_coef_daegu <- data.frame(regresszio$coefficients)
write.csv2(reg1_coef_daegu, "reg1_coef_daegu.csv")

regresszio2 <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + 
                    Heating.type + Number.of.top.university.entrants + Number.of.bus.stops + Network.distance.to.the.CBD + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.subway.station
                  + Parking.space.per.household  + Population.density,
                 data = subset(fofajl_es_keruleteik_daegu, Kerulet == as.vector(kerulet_nevek %>% slice (2))))

reg2_coef_daegu <- data.frame(regresszio2$coefficients)
write.csv2(reg2_coef_daegu, "reg2_coef_daegu.csv")

regresszio3 <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + 
                    Heating.type + Number.of.top.university.entrants + Number.of.bus.stops + Network.distance.to.the.CBD + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.subway.station
                  + Parking.space.per.household,
                 data = subset(fofajl_es_keruleteik_daegu, Kerulet == as.vector(kerulet_nevek %>% slice (3))))

reg3_coef_daegu <- data.frame(regresszio3$coefficients)
write.csv2(reg3_coef_daegu, "reg3_coef_daegu.csv")

regresszio4 <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + 
                    Heating.type + Number.of.top.university.entrants + Number.of.bus.stops + Network.distance.to.the.CBD + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.subway.station
                  + Parking.space.per.household  + Population.density,
                 data = subset(fofajl_es_keruleteik_daegu, Kerulet == as.vector(kerulet_nevek %>% slice (4))))

reg4_coef_daegu <- data.frame(regresszio4$coefficients)
write.csv2(reg4_coef_daegu, "reg4_coef_daegu.csv")

regresszio5 <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + 
                    Heating.type + Number.of.top.university.entrants + Number.of.bus.stops + Network.distance.to.the.CBD + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.subway.station
                  + Parking.space.per.household + Population.density,
                 data = subset(fofajl_es_keruleteik_daegu, Kerulet == as.vector(kerulet_nevek %>% slice (5))))

reg5_coef_daegu <- data.frame(regresszio5$coefficients)
write.csv2(reg5_coef_daegu, "reg5_coef_daegu.csv")

regresszio6 <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + 
                    Heating.type + Number.of.top.university.entrants + Number.of.bus.stops + Network.distance.to.the.CBD + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.subway.station
                  + Parking.space.per.household  + Population.density,
                 data = subset(fofajl_es_keruleteik_daegu, Kerulet == as.vector(kerulet_nevek %>% slice (6))))

reg6_coef_daegu <- data.frame(regresszio6$coefficients)
write.csv2(reg6_coef_daegu, "reg6_coef_daegu.csv")

regresszio7 <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year +  Number.of.households + 
                   Number.of.top.university.entrants + Number.of.bus.stops + Network.distance.to.the.CBD + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.subway.station
                  + Parking.space.per.household  + Population.density,
                 data = subset(fofajl_es_keruleteik_daegu, Kerulet == as.vector(kerulet_nevek %>% slice (7))))

reg7_coef_daegu <- data.frame(regresszio7$coefficients)
write.csv2(reg7_coef_daegu, "reg7_coef_daegu.csv")

regresszio8 <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + 
                    Heating.type + Number.of.top.university.entrants + Number.of.bus.stops + Network.distance.to.the.CBD + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.subway.station
                  + Parking.space.per.household  + Population.density,
                 data = subset(fofajl_es_keruleteik_daegu, Kerulet == as.vector(kerulet_nevek %>% slice (8))))

reg8_coef_daegu <- data.frame(regresszio8$coefficients)
write.csv2(reg8_coef_daegu, "reg8_coef_daegu.csv")

#GWR
library(spgwr)
library(ggplot2)

daegu$Network.distance.to.nearest.greenspace <- log(daegu$Network.distance.to.nearest.greenspace)
daegu$Network.distance.to.nearest.waterfront <- log(daegu$Network.distance.to.nearest.waterfront)
daegu$Network.distance.to.nearest.subway.station <- log(daegu$Network.distance.to.nearest.subway.station)
daegu[sapply(daegu, is.infinite)] <- 0

coord <- data.frame(cbind(daegu$Longitude,daegu$Latitude))
koord2 <- SpatialPointsDataFrame(coord, data =daegu)

library(GWmodel)
gwrBandwidth <-bw.gwr(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + 
                        Heating.type + Number.of.top.university.entrants + Number.of.bus.stops + Network.distance.to.the.CBD + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.subway.station
                      + Parking.space.per.household  + Population.density, data = koord2, approach = "AIC", kernel = "bisquare", adaptive =TRUE, longlat=TRUE)

GWR_1 <- gwr.basic(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + 
                     Heating.type + Number.of.top.university.entrants + Number.of.bus.stops + Network.distance.to.the.CBD + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.subway.station
                   + Parking.space.per.household  + Population.density, data = koord2, bw = gwrBandwidth, kernel = "bisquare", adaptive = TRUE, longlat = TRUE, parallel.method = "omp")

gwr1 <- as.data.frame(GWR_1$SDF)
write.csv2(gwr1, "daegu_GWR_estimates.csv")

#Referencia globalis OLS
global1 <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + 
                Heating.type + Number.of.top.university.entrants + Number.of.bus.stops + Network.distance.to.the.CBD + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.subway.station
              + Parking.space.per.household  + Population.density, data = daegu)

summary(global1)
global1_write <- as.data.frame(global1$coefficients)
write.csv2(global1_write, "daegu_global_OLS.csv")




