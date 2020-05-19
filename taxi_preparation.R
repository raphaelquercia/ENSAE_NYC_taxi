###############################################################################################################################
# Packages
###############################################################################################################################library(dplyr)
library(readr)
library(jsonlite)
library(lubridate)
library(sf)
library(geojsonsf)
library(tidyr)
library(dplyr)

###############################################################################################################################
# chargement des données
################################################################################################################################ 
# Taxi train ==> https://www.kaggle.com/c/nyc-taxi-trip-duration
taxi <- read.csv2(file='data/train.csv', sep=",", dec=".")
# Shape file arrondissement et zip code ==> https://earthworks.stanford.edu/catalog/nyu_2451_34509
nycShapeFile <- geojson_sf('data/shape/nyu-2451-34509-geojson.json')
# Météo ==> package RIEM mais proxy EDF veut pas alors alors la source direct ==> https://mesonet.agron.iastate.edu/request/download.phtml
# Extraction manuelle des données météo sur les 4 aéroports -JRB, LGA, NYC, JFK- les plus proches de NYC
nycWeather <- read.csv2(file="data/NYC_weather.csv", sep=",", dec=".")


# TAXI############################################################################################################################### 
# Parsing date et split jusqu'à la granularité horaire
taxi <- mutate(taxi, 
                    pickup_datetime = ymd_hms(pickup_datetime),
                    dropoff_datetime = ymd_hms(dropoff_datetime),
                    year = year(pickup_datetime),
                    month = month(pickup_datetime),
                    day = day(pickup_datetime),
                    weekDay = wday(pickup_datetime),
                    pickupHour = hour(pickup_datetime),
                    dropoffHour = hour(dropoff_datetime))
########
# TAXI + zipcode############################################################################################################################### 
# Création objet geometry pickup dans jeu de données
taxi <- st_as_sf(taxi,
                       coords = c("pickup_longitude","pickup_latitude"),
                       agr = "constant",
                       dim = "XY",
                       crs=4326, # = WGS84
                       stringsAsFactors = F,
                       remove = F)
# Inner join sur la base des zipcode ==> Hypothèse 1 : on exclut les trajets dont départ hors NYC
taxi <- st_join(taxi, nycShapeFile, join = st_within,left=F) 
# Renommage
taxi <- taxi %>% 
  mutate (pickupBcode = bcode) %>% 
  mutate (pickupZcta = zcta) %>% 
  select(-bcode) %>%
  select(-zcta)

# A priori, on ne peut avoir qu'un objet Sf:geometry dans un dataframe
# Suppression de l'objet geometry
st_geometry(taxi) <- NULL
# Création objet geometry dropoff dans jeu de données
taxi <- st_as_sf(taxi,
                             coords = c("dropoff_longitude","dropoff_latitude"),
                             agr = "constant",
                             dim = "XY",
                             crs=4326, # = WGS84
                             stringsAsFactors = F,
                             remove = F)
# Inner join sur la base des zipcode ==> Hypothèse 2 : on exclut les trajets dont arrivée hors NYC
taxi <- st_join(taxi, nycShapeFile, join = st_within,left=F)
# Renommage
taxi <- taxi %>% 
  mutate (dropoffBcode = bcode) %>% 
  mutate (dropoffZcta = zcta) %>% 
  select(-bcode) %>%
  select(-zcta)

# Pour faire des tests rapides ==> https://www.coordonnees-gps.fr/

########
# TAXI + zipcode + distance####
# calcul et rajout distance
source("distance_computation.R")
taxi <- taxi %>% 
  mutate (distance = getDistanceFromLatLonInKm(dropoff_latitude,dropoff_longitude,pickup_latitude,pickup_longitude))
# Pour faire des tests rapides ==> https://www.coordonnees-gps.fr/

########
# TAXI + zipcode + distance + Météo####
# Parsing date 
nycWeather <- mutate(nycWeather, 
                  validDate = ymd_hm(valid),
                  wYear = year(validDate),
                  wMonth = month(validDate),
                  wDay = day(validDate),
                  wWeekDay = wday(validDate),
                  # On associe le timestamp de l'observation météo pour la prochaine jointure avec taxi
                  wHour = hour(round_date(validDate, unit = 'hour')))

# On ne retient que la température, vent et précipitations mais il existe d'autres variables potentiellement explicatives
# https://www.weather.gov/media/asos/aum-toc.pdf
aggWeather <- nycWeather %>% group_by(wYear, wMonth, wDay, wHour) %>%
  summarise(
    # on prend la valeur max observée (sur les 4 aéroports considérés) pour chacun des points horaires 
    maxTemp = max(tmpc),
    maxWind = max(sped),
    maxPrecip = max(p01m))

# ajout des données météo par jointure sur l'heure de départ
taxi <- left_join(taxi, aggWeather, by=c("year"="wYear","month"="wMonth","day"="wDay","pickupHour"="wHour"))

# Trafic / todo

# Events / todo : voir fichier Louis Pierre ?

# Sauvegarde du jeu de données
write.csv(taxi, 'taxiTrainDataSet.csv')
save.image(file = 'taxiTrainDataSet.rdata')

# Filtrage données / todo

# Train Model > test et évaluation / todo


