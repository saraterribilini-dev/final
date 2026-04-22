###############################################################################
# 1) PACKAGES
###############################################################################
library(rgbif)         # access to GBIF data
library(rnaturalearth) # country maps
library(ggplot2)       # graphics
library(rinat)         # access to iNaturalist data
library(raster)        # spatial extent management
library(dplyr)         # table manipulation
library(sf)       

sf_use_s2(FALSE)


### CUCULUS CANORUS ####
###############################################################################
# 2) USER PARAMETERS
###############################################################################

# Species of interest
myspecies <- "Cuculus canorus"

# Maximum number of GBIF records to download
gbif_limit <- 7000 #se ci mette troppo a caricare torna a 5000

# Time filtering period
date_start <- as.Date("2015-01-01")
date_end   <- as.Date("2026-12-31")

# Simplified geographic extent for Europe
xmin <- -22
xmax <- 57
ymin <- 30
ymax <- 70
#se voglio fare una carta super pulita posso provare a creare un raster delle dimensioni che voglio
#poi dopo il mio plot si ritaglierà sopra il mio raster e avrò una mappa più pulita
###############################################################################
# 3) BASE MAP: Europe
###############################################################################

# Download the outline of Europe
Europe <- ne_countries(
  scale = "medium",
  returnclass = "sf",
  continent = "Europe"
)

# Simple visualization of the map

#ggplot(data = Europe) + geom_sf(fill = "grey95", color = "black") + theme_classic()
  
#plot(st_geometry(Europe), xlim = c(-22, 57), ylim = c(30, 70))

###############################################################################
# 4) DOWNLOAD GBIF DATA
###############################################################################

# Download occurrences with coordinates
gbif_raw <- occ_data(
  scientificName = myspecies,
  hasCoordinate = TRUE,
  limit = gbif_limit
)

# Extract the main data table
gbif_occ <- gbif_raw$data
gbif_occ$continent
# Quick inspection
head(gbif_occ)
names(gbif_occ)

# Select occurrences located in Europe
gbif_Europe <- gbif_occ %>%
  filter(continent == "EUROPE") #europe in maiuscolo

# Check number of records
nrow(gbif_Europe)

# Quick base plot for checking
plot(
  gbif_Europe$decimalLongitude,
  gbif_Europe$decimalLatitude,
  pch = 16,
  col = "darkgreen",
  xlab = "Longitude",
  ylab = "Latitude",
  main = "GBIF occurrences in Europe")

# Map showing GBIF occurrences only
quartz()
t1 <- ggplot(data = Europe) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = gbif_Europe,
    aes(x = decimalLongitude, y = decimalLatitude),
    size = 1.5,
    shape = 21,
    fill = "darkgreen",
    color = "black"
  ) +
  theme_classic() + xlim(xmin,xmax) + ylim(ymin,ymax)+ 
  labs(title = "GBIF occurrences of Cuculus canorus")

print(t1)
###############################################################################
# 5) FORMAT GBIF DATA
###############################################################################

# Keep only the useful columns
# eventDate may contain date + time; as.Date() keeps only the date
data_gbif <- data.frame(
  species   = gbif_Europe$species,
  latitude  = gbif_Europe$decimalLatitude,
  longitude = gbif_Europe$decimalLongitude,
  date_obs  = as.Date(gbif_Europe$eventDate),
  source    = "gbif"
)

# Check structure
head(data_gbif)
str(data_gbif)

###############################################################################
# 6) DOWNLOAD iNaturalist DATA
###############################################################################

# Query iNaturalist for the same species in EUROPE
# place_id = "europe" usually works with rinat
inat_raw <- get_inat_obs(
  query = myspecies,
  place_id = "europe"
)

# Inspect the structure
head(inat_raw)
names(inat_raw)

# Map showing iNaturalist occurrences only
quartz()
t2 <- ggplot(data = Europe) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = inat_raw,
    aes(x = longitude, y = latitude),
    size = 1.5,
    shape = 21,
    fill = "darkred",
    color = "black"
  ) +
  theme_classic() + xlim(xmin,xmax) + ylim(ymin,ymax) +
  labs(title = "iNaturalist occurrences of Cuculus canorus")
print(t2)
###############################################################################
# 7) FORMAT iNaturalist DATA
###############################################################################

# In most rinat versions the observation date is stored in observed_on
# Convert it to Date format
data_inat <- data.frame(
  species   = inat_raw$scientific_name,
  latitude  = inat_raw$latitude,
  longitude = inat_raw$longitude,
  date_obs  = as.Date(inat_raw$observed_on),
  source    = "inat"
)

# Check structure
head(data_inat)
str(data_inat)

###############################################################################
# 8) MERGE THE TWO DATABASES
###############################################################################

# IMPORTANT:
# Here we want to STACK GBIF and iNaturalist observations.
# Therefore we use bind_rows() instead of merge().
matrix_full <- bind_rows(data_gbif, data_inat)

# Check results
head(matrix_full)
table(matrix_full$source, useNA = "ifany")
summary(matrix_full$date_obs)

###############################################################################
# 9) TIME FILTERING BETWEEN TWO DATES
###############################################################################

# Keep only observations within the selected time interval
matrix_full_date_sp1 <- matrix_full %>%
  filter(!is.na(date_obs)) %>%
  filter(date_obs >= date_start & date_obs <= date_end)

# Check results
head(matrix_full_date_sp1)
summary(matrix_full_date_sp1$date_obs)
table(matrix_full_date_sp1$source)

###############################################################################
# 10) MAP OF COMBINED DATA
###############################################################################
quartz()
t3 <- ggplot(data = Europe) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = matrix_full_date_sp1,
    aes(x = longitude, y = latitude, fill = source),
    size = 1.5,
    shape = 21,
    color = "black",
    alpha = 0.8
  ) +
  theme_classic() + xlim(xmin,xmax) + ylim(ymin,ymax)+
  labs(title = "Occurrences of Cuculus canorus in Europe")
print(t3)