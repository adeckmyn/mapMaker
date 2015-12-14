# mapMaker
###Create maps for the R package 'maps'

This is a package that helps to create maps that can be used with the R package 'maps'. Typical use could be to create a map with internal (state, provincial) borders for a particular country. Most likely input at this stage is data from the Natural Earth project.

##Introduction

The 'maps' package comes with a number of maps installed, and the 'mapdata' packages adds several more. But sometimes people need e.g. a detailed map for a particular country, or an alternative version of a map. This package can help to make such data available for use with the 'maps' package.
In this early stage, the main purpose is to read data from shapefiles available from the Natural Earth project (http://www.naturalearthdata.com).

This package provides:

- a simple routine that reads shape files (as provided e.g. by Natural Earth) and exports a list of polygons that can be directly used in 'map()'
- a set of R routines that transforms and exports this set of polygons into the file based format. This can even include splitting into polylines.
- export to ascii files (like in the sources of 'maps' and 'mapdata') or directly in binary format (as in the installed packages).

##Formats

The data sets in 'maps' and 'mapdata' are distributed as ascii files, but installed in a particular binary format that allows for efficient reading of polygons and polylines.

However, the 'map()' function can also use a simpler map format which is not necessarily file based. A map database may also be a simple R list with three components:
- x,y: 2 vectors containing longitude and lattitude of a set of polygons (with NA separating the polygons)
- names: the names of the polygons

The output of map(...,fill=TRUE) is in fact such a list and can be used again:
> myworld <- map('world',fill=TRUE)
> map(myworld)

So the simplest way to use new data sets in 'maps' is as such a simple list of polygons. This is fine for many applications, but some of the possibilities of the 'maps' package are only available for the file based maps.

##HOW TO

a typical mapMaker session may look like this:
```R
# read provincial borders
be <- read.admin1(database="ne_10m_admin_1_states_provinces", countries="BE")
# you can use map() to see the result
map(be) 

```
####To create a polyline-based set of files:

1. Turn into an internal format, but still just polygons.  
`be1 <- map.make(be)`
2. Split the polygons into line segments consisting of 2 points.  
`be2 <- map.split(be1)`
3. Remove all dupplicate segments.  
`be3 <- map.dups(be2)`
4. Calculate the 'valence' of every point (number of segments it belongs to). This may take some time (for the complete 1:50 world map from Natural Earth: a bit over 1 minute on my PC).  
`val <- map.valence(be3)`
5. Shift the lines forming a polygon, until the polygon starts at a vertex (except islands, of course).  
`be4 <- map.shift.gon(be3,val)`
6. Merge all segments to polylines that begin and end at a vertex.  
`be5 <- map.merge.segments(be4,val)`
7. Export as a binary file (not yet completely implemented...).  
`map.export.bin(be5,file=paste(MY_MAP_PATH,"belgium",sep="/"))`
8. Make the map available to `maps`. This requires setting an environment variable and a local variable.  
`Sys.setenv("R_MY_MAP_PATH"=MY_MAP_PATH)`  
`belgiumMapEnv <- "R_MY_MAP_PATH"`
9. Finished!  
`map(database="belgium")`


## TO DO
- export directly to binary format (in progress)
- use the data sets provide by rnaturalearth, rather than downloading yourself
- Joining polygons (e.g. Russia) that have been split to opposite sides of the map
- Further editing of maps...

