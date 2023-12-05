#Adatok beolvasasa
setwd("C:/users/kochd/Desktop/WiP Szakdoga/Adatok")
busan <- read.csv2("busan_r.csv")

busan[busan$Heating.type == 0,]$Heating.type <- "Gas"
busan[busan$Heating.type == 1,]$Heating.type <- "Other"
busan$Heating.type <- as.factor(busan$Heating.type)

busan$Network.distance.to.nearest.greenspace <- log(busan$Network.distance.to.nearest.greenspace)
busan$Network.distance.to.nearest.waterfront <- log(busan$Network.distance.to.nearest.waterfront)
busan$Network.distance.to.nearest.subway.station <- log(busan$Network.distance.to.nearest.subway.station)
busan$Network.distance.to.the.CBD <- log(busan$Network.distance.to.the.CBD)
busan[sapply(busan, is.infinite)] <- 0

#Abrak
boxplot(busan)
hist(busan$Condominium.price, 100, xlim=c(0, 60000))

#Teszt modell
fit1 <- lm(Condominium.price ~ Size.of.unit + Floor + Construction.year + Network.distance.to.nearest.subway.station + Number.of.households +
             Highest.floor + Parking.space.per.household + Heating.type + Number.of.top.university.entrants + Number.of.high.schools + Network.distance.to.the.CBD + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront
           + Number.of.bus.stops + Population.density + Ratio.of.adults.with.higher.degrees, data = busan)

fit2 <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Construction.year + Network.distance.to.nearest.subway.station + Number.of.households +
             Highest.floor + Parking.space.per.household + Heating.type + Number.of.top.university.entrants + Number.of.high.schools + Network.distance.to.the.CBD + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront
           + Number.of.bus.stops + Population.density, data = busan)

summary(fit1)
summary(fit2)

#Multikollienaritas
install.packages("corrplot")
library("corrplot")
corrplot(cor(busan), method = "number")

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

shape <- st_read("C:/Users/kochd/Desktop/WiP Szakdoga/Adatok/Shape files/Busan_condominiums.shp")
clean_shape <- st_read("C:/Users/kochd/Desktop/WiP Szakdoga/Adatok/Moran´s I/Busan/MoranI_busan2.shp")
coord <- data.frame(cbind(clean_shape$Longitude, clean_shape$Latitude))

colnames(coord) <- c("x", "y")

w <- 1/as.matrix(dist(coordinates(coord)))
diag(w) <- 0
nb <- tri2nb(coord)
busan.geoms <- st_as_sf(coord, coords = c("x", "y"))

moran.test(log(clean_shape$Condominiu), nb2listwdist(nb, busan.geoms, type = "idw", style = "W"))

#Hedonikus regresszio - adatok
install.packages("tidyverse")
library(tidyverse)

setwd("C:/Users/kochd/Desktop/WiP Szakdoga/Adatok/Vegleges_shape/Busan")
kerulet_nevek <- read.csv2("Kerulet_vektor_busan.csv")
fofajl_es_keruleteik_busan <- read.csv2("fofajl_es_keruleteik_busan.csv")

fofajl_es_keruleteik_busan$Heating.type <- as.factor(fofajl_es_keruleteik_busan$Heating.type)
fofajl_es_keruleteik_busan$Network.distance.to.nearest.greenspace <- log(fofajl_es_keruleteik_busan$Network.distance.to.nearest.greenspace)
fofajl_es_keruleteik_busan$Network.distance.to.nearest.waterfront <- log(fofajl_es_keruleteik_busan$Network.distance.to.nearest.waterfront)
fofajl_es_keruleteik_busan$Network.distance.to.nearest.subway.station <- log(fofajl_es_keruleteik_busan$Network.distance.to.nearest.subway.station)
fofajl_es_keruleteik_busan[sapply(fofajl_es_keruleteik_busan, is.infinite)] <- 0

#Hedonikus regressziok - elemzes

regresszio <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + Number.of.top.university.entrants + 
     Heating.type + Number.of.bus.stops + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront
   + Parking.space.per.household  + Population.density,
   data = subset(fofajl_es_keruleteik_busan, Kerulet == as.vector(kerulet_nevek %>% slice (1))))

reg1_coef_busan <- data.frame(regresszio$coefficients)
write.csv2(reg1_coef_busan, "reg1_coef_busan.csv")



regresszio2 <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + Number.of.top.university.entrants + 
                   Heating.type + Number.of.bus.stops + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront
                 + Parking.space.per.household  + Population.density,
                 data = subset(fofajl_es_keruleteik_busan, Kerulet == as.vector(kerulet_nevek %>% slice (2))))

reg2_coef_busan <- data.frame(regresszio2$coefficients)
write.csv2(reg2_coef_busan, "reg2_coef_busan.csv")



regresszio3 <- lm(log(Condominium.price) ~ Size.of.unit + Number.of.high.schools + Construction.year + Number.of.households + Number.of.top.university.entrants + 
                   Heating.type + Number.of.bus.stops + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront
                 + Parking.space.per.household  + Population.density,
                 data = subset(fofajl_es_keruleteik_busan, Kerulet == as.vector(kerulet_nevek %>% slice (3)))) 

reg3_coef_busan <- data.frame(regresszio3$coefficients)
write.csv2(reg3_coef_busan, "reg3_coef_busan.csv")



regresszio4 <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + Number.of.top.university.entrants + 
                    Number.of.bus.stops + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront
                 + Parking.space.per.household  + Population.density,
                 data = subset(fofajl_es_keruleteik_busan, Kerulet == as.vector(kerulet_nevek %>% slice (4))))

reg4_coef_busan <- data.frame(regresszio4$coefficients)
write.csv2(reg4_coef_busan, "reg4_coef_busan.csv")




regresszio5 <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + 
                   Number.of.bus.stops
                 + Parking.space.per.household  + Population.density,
                 data = subset(fofajl_es_keruleteik_busan, Kerulet == as.vector(kerulet_nevek %>% slice (5))))

reg5_coef_busan <- data.frame(regresszio5$coefficients)
write.csv2(reg5_coef_busan, "reg5_coef_busan.csv")




regresszio6 <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + Number.of.top.university.entrants + 
                   Heating.type + Number.of.bus.stops + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront
                 + Parking.space.per.household  + Population.density,
                 data = subset(fofajl_es_keruleteik_busan, Kerulet == as.vector(kerulet_nevek %>% slice (6))))

reg6_coef_busan <- data.frame(regresszio6$coefficients)
write.csv2(reg6_coef_busan, "reg6_coef_busan.csv")



regresszio7 <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + 
                    Heating.type + Number.of.bus.stops + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront
                  + Parking.space.per.household,
                  data = subset(fofajl_es_keruleteik_busan, Kerulet == as.vector(kerulet_nevek %>% slice (7))))

reg7_coef_busan <- data.frame(regresszio7$coefficients)
write.csv2(reg7_coef_busan, "reg7_coef_busan.csv")



regresszio8 <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + Number.of.top.university.entrants + 
                    Heating.type + Number.of.bus.stops + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront
                  + Parking.space.per.household  + Population.density,
                  data = subset(fofajl_es_keruleteik_busan, Kerulet == as.vector(kerulet_nevek %>% slice (8))))

reg8_coef_busan <- data.frame(regresszio8$coefficients)
write.csv2(reg8_coef_busan, "reg8_coef_busan.csv")




regresszio9 <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + Number.of.top.university.entrants + 
                     Number.of.bus.stops + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront
                  + Parking.space.per.household,
                  data = subset(fofajl_es_keruleteik_busan, Kerulet == as.vector(kerulet_nevek %>% slice (9))))

reg9_coef_busan <- data.frame(regresszio9$coefficients)
write.csv2(reg9_coef_busan, "reg9_coef_busan.csv")




regresszio10 <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + Number.of.top.university.entrants + 
                    Heating.type + Number.of.bus.stops + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront
                  + Parking.space.per.household  + Population.density,
                  data = subset(fofajl_es_keruleteik_busan, Kerulet == as.vector(kerulet_nevek %>% slice (10))))

reg10_coef_busan <- data.frame(regresszio10$coefficients)
write.csv2(reg10_coef_busan, "reg10_coef_busan.csv")




regresszio11<- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + Number.of.top.university.entrants + 
                    Heating.type + Number.of.bus.stops + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront
                  + Parking.space.per.household  + Population.density,
                  data = subset(fofajl_es_keruleteik_busan, Kerulet == as.vector(kerulet_nevek %>% slice (11))))

reg11_coef_busan <- data.frame(regresszio11$coefficients)
write.csv2(reg11_coef_busan, "reg11_coef_busan.csv")




regresszio12 <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + Number.of.top.university.entrants + 
                    Heating.type + Number.of.bus.stops + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront
                  + Parking.space.per.household  + Population.density,
                  data = subset(fofajl_es_keruleteik_busan, Kerulet == as.vector(kerulet_nevek %>% slice (12))))

reg12_coef_busan <- data.frame(regresszio12$coefficients)
write.csv2(reg12_coef_busan, "reg12_coef_busan.csv")




regresszio13 <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + Number.of.top.university.entrants + 
                    Heating.type + Number.of.bus.stops + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront
                  + Parking.space.per.household  + Population.density,
                  data = subset(fofajl_es_keruleteik_busan, Kerulet == as.vector(kerulet_nevek %>% slice (13))))

reg13_coef_busan <- data.frame(regresszio13$coefficients)
write.csv2(reg13_coef_busan, "reg13_coef_busan.csv")




regresszio14 <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + Number.of.top.university.entrants + 
                    Heating.type + Number.of.bus.stops + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront
                  + Parking.space.per.household  + Population.density,
                  data = subset(fofajl_es_keruleteik_busan, Kerulet == as.vector(kerulet_nevek %>% slice (14))))

reg14_coef_busan <- data.frame(regresszio14$coefficients)
write.csv2(reg14_coef_busan, "reg14_coef_busan.csv")




regresszio15 <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + Number.of.top.university.entrants + 
                     Heating.type + Number.of.bus.stops + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront
                   + Parking.space.per.household  + Population.density,
                   data = subset(fofajl_es_keruleteik_busan, Kerulet == as.vector(kerulet_nevek %>% slice (15))))

reg15_coef_busan <- data.frame(regresszio15$coefficients)
write.csv2(reg15_coef_busan, "reg15_coef_busan.csv")




regresszio16 <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + Number.of.top.university.entrants + 
                     Heating.type + Number.of.bus.stops + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront
                   + Parking.space.per.household  + Population.density,
                   data = subset(fofajl_es_keruleteik_busan, Kerulet == as.vector(kerulet_nevek %>% slice (16))))

reg16_coef_busan <- data.frame(regresszio16$coefficients)
write.csv2(reg16_coef_busan, "reg16_coef_busan.csv")

#GWR
library(spgwr)
library(ggplot2)

setwd("C:/users/kochd/Desktop/WiP Szakdoga/Adatok")
busan_r <- read.csv2("busan_r.csv")

busan_r$Heating.type <- as.factor(busan_r$Heating.type)
busan_r$Network.distance.to.nearest.greenspace <- log(busan_r$Network.distance.to.nearest.greenspace)
busan_r$Network.distance.to.nearest.waterfront <- log(busan_r$Network.distance.to.nearest.waterfront)
busan_r$Network.distance.to.nearest.subway.station <- log(busan_r$Network.distance.to.nearest.subway.station)
busan_r[sapply(busan_r, is.infinite)] <- 0

coord <- data.frame(cbind(busan_r$Longitude,busan_r$Latitude))
busan_koord <- SpatialPointsDataFrame(coord, data =busan_r)

library(GWmodel)
gwrBandwidth4 <-bw.gwr(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + Number.of.top.university.entrants + 
                         Heating.type + Number.of.bus.stops + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.waterfront
                       + Parking.space.per.household  + Population.density, data = busan_koord, approach = "AIC", kernel = "bisquare", adaptive =TRUE, longlat=TRUE)


GWR_4 <- gwr.basic(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + 
                     Heating.type + Number.of.top.university.entrants + Number.of.bus.stops + Network.distance.to.the.CBD + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.subway.station
                   + Parking.space.per.household  + Population.density, data = busan_koord, bw = gwrBandwidth4, kernel = "bisquare", adaptive = TRUE, longlat = TRUE, parallel.method = "omp")


gwr4 <- as.data.frame(GWR_4$SDF)
write.csv2(gwr4, "busan_GWR_estimates.csv")

#Referencia globalis OLS
global4 <- lm(log(Condominium.price) ~ Size.of.unit + Highest.floor + Number.of.high.schools + Construction.year + Number.of.households + 
                Heating.type + Number.of.top.university.entrants + Number.of.bus.stops + Network.distance.to.the.CBD + Network.distance.to.nearest.greenspace + Network.distance.to.nearest.subway.station
              + Parking.space.per.household  + Population.density, data = busan_r)

summary(global4)
global4_write <- as.data.frame(global4$coefficients)
write.csv2(global4_write, "busan_global_OLS.csv")

