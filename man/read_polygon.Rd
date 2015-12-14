\name{read_regions}
\alias{read_admins1}
\alias{read_worldmap}
\title{
  Read polygon data from a shape file.
}
\description{Extract a set of polygons from a shape file and return as a simple list.}
\usage{
read.worldmap <- function(database="data/ne_10m_admin_0_countries_lakes",
                          countries=NULL,namefield="NAME_LONG") 
read.admin1 <- function(database="data/ne_10m_admin_1_states_provinces",
                        countries="BE",namefield="woe_name")
read.regions <- function(database, countries,namefield)
}
\arguments{
  \item{database}{
    character string naming a geographical database, typically a shapefile, but it could be any format readable with \code{readShapePoly}. It may also be a \code{SpatialPolygonDataFrame}. }
  \item{countries}{
    character vector with 2-letter codes (ISO-3166) of countries to extract.
  }
  \item{namefield}{Which field to use for the polygon names.}
}
\details{
  \code{read.worldmap} and \code{read.admin1} are just wrappers with a different set of defaults. The output is a list object that can be used as a map database for the \code{map} function.
}
\value{
A list with three components:
\itemize{
  \item{x,y: longitude and latitude vectors. The polygons are separated by \code{NA} values.}
  \item{names: a character vector with length equal to the number of polygons in \code{x,y}.} 
}
}
# \examples{
#be <- read.admin1(country="BE")
#map(be)	# low resolution map of the world
#}
\seealso{
  \code{\link{maps:map}} }

