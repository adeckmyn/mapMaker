read.worldmap <- function(database="ne_50m_admin_0_countries_lakes",
                          countries=NULL,namefield="name") {
  read.regions(database,countries,namefield)
}

read.admin1 <- function(database="ne_10m_admin_1_states_provinces",
                        countries="BE",namefield="name"){
  read.regions(database,countries,namefield)
}

read.regions <- function(database, countries,namefield){
# TODO : if two namefields are given: paste(..,sep=":")
#  require(maptools)
  if (is.character(database)) database <- readShapePoly(database)
  
  if (!is.null(countries)) {
    a2 <- database@data[[which(tolower(names(database))=="iso_a2")]]
    database <- database[a2 %in% countries,]
  }
  
  region.names <- as.character(database[[namefield]])
  nregions <- length(region.names)

  ngon <- vapply(1:nregions,FUN=function(i) length(database@polygons[[i]]@Polygons),FUN.VALUE=1)

  gon.names <- unlist(lapply(1:dim(database)[1], function(i) {
             if(ngon[i]==1) region.names[i] else paste(region.names[i],1:ngon[i],sep=":")}))

  allpoly <- lapply(database@polygons, function(x) lapply(x@Polygons, function(y) y@coords))
## allpoly is a list of lists of Nx2 matrices (not data frames)
## first flatten the list, then add NA to every row, then rbind and remove last NA
#  p1 <- do.call(c, allpoly)
#  p2 <- lapply(p1, function(x) rbind(x,c(NA,NA)))
#  p3 <- do.call(rbind,p2)
  result <- do.call(rbind, lapply(do.call(c,allpoly), function(x) rbind(x,c(NA,NA))))

  list(x=head(result[,1],-1),y=head(result[,2],-1),names=gon.names)
}

