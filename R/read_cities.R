read.cities <- function(infile="ne_10m_populated_places",country="BE"){

  require(maptools)
  PW <- readShapePoints(infile)

  PW <- PW[which(PW$ISO_A2==country),]
  city.name <- as.character(PW$NAME)
  city.lon <- PW$LONGITUDE
  city.lat <- PW$LATITUDE

  data.frame(name=city.name,lon=city.lon,lat=city.lat)
}

