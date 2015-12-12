# mapMaker
###Create maps for the R package 'maps'

This is a package that helps to create maps that can be used with the R package 'maps'. Typical use could be to create a map with internal (state, provincial) borders for a particular country. Most likely input at this stage is data from the Natural Earth project.

##Introduction

The 'maps' package comes with a number of maps installed, and the 'mapdata' packages adds several more. But sometimes people need e.g. a detailed map for a particular country, or an alternative version of a map. This package can help to make such data available for use with the 'maps' package.

In this early stage, the main purpose is to read data from shapefiles available from the Natural Earth project (http://www.naturalearthdata.com).

##Basics

The data sets in 'maps' and 'mapdata' are distributed as ascii files, but installed in a particular binary format that allows for efficient reading of polygons and polylines.

However, the 'map()' function can also use a simpler map format which is not necessarily file based. A map database may also be a simple R list with three components:
- x,y: 2 vectors containing longitude and lattitude of a set of polygons (with NA separating the polygons)
- names: the names of the polygons

The output of map(...,fill=TRUE) is in fact such a list and can be used again:
> myworld <- map('world',fill=TRUE)
> map(myworld)

So the simplest way to use new data sets in 'maps' is as such a simple list of polygons. This is fine for many applications, but some of the possibilities of the 'maps' package are only available for the file based maps.

This package provides:

- a simple routine that reads shape files (as provided e.g. by Natural Earth) and exports a list of polygons that can be directly used in 'map()'
- a set of R routines that transforms and exports this set of polygons into the file based format. This can even include splitting into polylines.
- export to ascii files (like in the sources of 'maps' and 'mapdata') or directly in binary format (as in the installed packages).

## TO DO
- export directly to binary format (in progress)
- explain how to make the exported maps accessible via environment variables
- use the data sets provide by rnaturalearth, rather than downloading yourself
- ...
