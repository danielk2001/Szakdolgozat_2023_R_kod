#Adatok beolvasasa
setwd("C:/users/kochd/Desktop/WiP Szakdoga/Adatok")
gwangju <- read.csv2("gwangju_r.csv")

gwangju[gwangju$Heating.type == 0,]$Heating.type <- "Gas"
gwangju[gwangju$Heating.type == 1,]$Heating.type <- "Other"
gwangju$Heating.type <- as.factor(gwangju$Heating.type)

gwangju$Network.distance.to.nearest.greenspace <- log(gwangju$Network.distance.to.nearest.greenspace)
gwangju$Network.distance.to.nearest.waterfront <- log(gwangju$Network.distance.to.nearest.waterfront)
gwangju$Network.distance.to.nearest.subway.station <- log(gwangju$Network.distance.to.nearest.subway.station)
gwangju$Network.distance.to.the.CBD <- log(gwangju$Network.distance.to.the.CBD)
gwangju[sapply(gwangju, is.infinite)] <- 0

#Abrak
boxplot(gwangju)
hist(gwangju$Condominium.price, 100, xlim=c(0, 60000))

#Teszt modell
fit1 <- lm(Condominium.price ~ Size.of.unit + Floor + Construction.year + Network.distance.to.nearest.subway.station + Number.of.households +
              Highest.floor + Parking.space.per.household + Heating.type + Number.of.top.university.entrants + Number.of.high.schools + Network.distance.to.the.CBD + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront
           + Number.of.bus.stops + Population.density + Ratio.of.adults.with.higher.degrees, data = gwangju)

fit2 <- lm(log(Condominium.price) ~ Size.of.unit + Floor + Construction.year + Network.distance.to.nearest.subway.station + Number.of.households +
             Highest.floor + Parking.space.per.household + Heating.type + Number.of.top.university.entrants + Number.of.high.schools + Network.distance.to.the.CBD + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront
           + Number.of.bus.stops + Population.density + Ratio.of.adults.with.higher.degrees, data = gwangju)

summary(fit1)
summary(fit2)

#Multikollienaritas
install.packages("corrplot")
library("corrplot")
corrplot(cor(gwangju), method = "number")

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

shape <- st_read("C:/Users/kochd/Desktop/WiP Szakdoga/Adatok/Shape files/Gwangju_condominiums.shp")
clean_shape2 <- st_read("C:/Users/kochd/Desktop/WiP Szakdoga/Adatok/Moran´s I/Gwangju/MoranI_gwangju.shp")
clean_shape <- clean_shape2[-503,]
coord <- data.frame(cbind(clean_shape$Longitude, clean_shape$Latitude))

colnames(coord) <- c("x", "y")

w <- 1/as.matrix(dist(coordinates(coord)))
diag(w) <- 0
nb <- tri2nb(coord)
gwangju.geoms <- st_as_sf(coord, coords = c("x", "y"))

moran.test(log(clean_shape$Condominiu), nb2listwdist(nb, gwangju.geoms, type = "idw", style = "W"))

#Hedonikus regresszio - adatok
install.packages("tidyverse")
library(tidyverse)

setwd("C:/Users/kochd/Desktop/WiP Szakdoga/Adatok/Vegleges_shape/gwangju")
kerulet_nevek <- read.csv2("Kerulet_vektor_gwangju.csv")
fofajl_es_keruleteik_gwangju <- read.csv2("fofajl_es_keruleteik_gwangju.csv")

fofajl_es_keruleteik_gwangju$Heating.type <- as.factor(fofajl_es_keruleteik_gwangju$Heating.type)
fofajl_es_keruleteik_gwangju$Network.distance.to.nearest.greenspace <- log(fofajl_es_keruleteik_gwangju$Network.distance.to.nearest.greenspace)
fofajl_es_keruleteik_gwangju$Network.distance.to.nearest.waterfront <- log(fofajl_es_keruleteik_gwangju$Network.distance.to.nearest.waterfront)
fofajl_es_keruleteik_gwangju$Network.distance.to.nearest.subway.station <- log(fofajl_es_keruleteik_gwangju$Network.distance.to.nearest.subway.station)
fofajl_es_keruleteik_gwangju[sapply(fofajl_es_keruleteik_gwangju, is.infinite)] <- 0

#Hedonikus regresszio - elemzes
regresszio <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + Number.of.top.university.entrants + 
                   Heating.type + Number.of.bus.stops + Network.distance.to.the.CBD + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront + Network.distance.to.nearest.subway.station
                 + Parking.space.per.household  + Population.density,
                 data = subset(fofajl_es_keruleteik_gwangju, Kerulet == as.vector(kerulet_nevek %>% slice (1))))

reg1_coef_gwangju <- data.frame(regresszio$coefficients)
write.csv2(reg1_coef_gwangju, "reg1_coef_gwangju.csv")

regresszio2 <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + Number.of.top.university.entrants + 
                     Number.of.bus.stops + Network.distance.to.the.CBD + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront + Network.distance.to.nearest.subway.station
                 + Parking.space.per.household  + Population.density,
                 data = subset(fofajl_es_keruleteik_gwangju, Kerulet == as.vector(kerulet_nevek %>% slice (2))))

reg2_coef_gwangju <- data.frame(regresszio2$coefficients)
write.csv2(reg2_coef_gwangju, "reg2_coef_gwangju.csv")

regresszio3 <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + 
                    Heating.type + Number.of.top.university.entrants + Number.of.bus.stops + Network.distance.to.the.CBD + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.subway.station
                  + Parking.space.per.household  + Population.density,
                  data = subset(fofajl_es_keruleteik_gwangju, Kerulet == as.vector(kerulet_nevek %>% slice (3))))

reg3_coef_gwangju <- data.frame(regresszio3$coefficients)
write.csv2(reg3_coef_gwangju, "reg3_coef_gwangju.csv")

regresszio4 <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + Number.of.top.university.entrants + 
                    Number.of.bus.stops + Network.distance.to.the.CBD + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront + Network.distance.to.nearest.subway.station
                 + Parking.space.per.household  + Population.density,
                 data = subset(fofajl_es_keruleteik_gwangju, Kerulet == as.vector(kerulet_nevek %>% slice (4))))

reg4_coef_gwangju <- data.frame(regresszio4$coefficients)
write.csv2(reg4_coef_gwangju, "reg4_coef_gwangju.csv")

regresszio5 <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + 
                    Heating.type + Number.of.top.university.entrants + Number.of.bus.stops + Network.distance.to.the.CBD + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.subway.station
                  + Parking.space.per.household  + Population.density,
                  data = subset(fofajl_es_keruleteik_gwangju, Kerulet == as.vector(kerulet_nevek %>% slice (5))))

reg5_coef_gwangju <- data.frame(regresszio5$coefficients)
write.csv2(reg5_coef_gwangju, "reg5_coef_gwangju.csv")


#GWR
library(spgwr)
library(ggplot2)

setwd("C:/users/kochd/Desktop/WiP Szakdoga/Adatok")
gwangju_r2 <- read.csv2("gwangju_r2.csv")

gwangju_r2$Heating.type <- as.factor(gwangju_r2$Heating.type)
gwangju_r2$Network.distance.to.nearest.greenspace <- log(gwangju_r2$Network.distance.to.nearest.greenspace)
gwangju_r2$Network.distance.to.nearest.waterfront <- log(gwangju_r2$Network.distance.to.nearest.waterfront)
gwangju_r2$Network.distance.to.nearest.subway.station <- log(gwangju_r2$Network.distance.to.nearest.subway.station)
gwangju_r2[sapply(gwangju_r2, is.infinite)] <- 0

coord <- data.frame(cbind(gwangju_r2$Longitude,gwangju_r2$Latitude))
gwangju_koord <- SpatialPointsDataFrame(coord, data =gwangju_r2)

library(GWmodel)
gwrBandwidth3 <-bw.gwr(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + 
                         Heating.type + Number.of.top.university.entrants + Number.of.bus.stops + Network.distance.to.the.CBD + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.subway.station
                       + Parking.space.per.household  + Population.density, data = gwangju_koord, approach = "AIC", kernel = "bisquare", adaptive =TRUE, longlat=TRUE)



GWR_3 <- gwr.basic(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + 
                     Heating.type + Number.of.top.university.entrants + Number.of.bus.stops + Network.distance.to.the.CBD + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.subway.station
                   + Parking.space.per.household  + Population.density, data = gwangju_koord, bw = gwrBandwidth3, kernel = "bisquare", adaptive = TRUE, longlat = TRUE, parallel.method = "omp")

gwr3 <- as.data.frame(GWR_3$SDF)
write.csv2(gwr3, "gwangju_GWR_estimates.csv")

#Referencia globalis OLS
global3 <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + 
                Heating.type + Number.of.top.university.entrants + Number.of.bus.stops + Network.distance.to.the.CBD + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.subway.station
              + Parking.space.per.household  + Population.density, data = gwangju_r2)

summary(global3)
global3_write <- as.data.frame(global3$coefficients)
write.csv2(global3_write, "gwangju_global_OLS.csv")








































