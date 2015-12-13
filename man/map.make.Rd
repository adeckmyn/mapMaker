\name{map.make}
\alias{map.make}
\alias{map.split}
\alias{map.dups}
\alias{map.shift.gon}
\alias{map.merge.segments}
\alias{map.LR}
\title{Turn a set of polygons into a polyline database}
\description{
  Turn a set of polygons (as returned by \code{map(...,fill=TRUE)} or \code{read.regions()}) into a polyline-based data set.
}
\usage{
map.make(map,scale=8)
map.split(ww)
map.dups(ww)
map.shift.gon(ww,valence=NULL)
map.merge.segments(ww,valence=NULL)
map.LR(ww)
}
\arguments{
\item{map}{A list with components \code{x,y,names}, as output from \code{map(fill=TRUE)} or \code{read.region()}.}
\item{ww}{A list with components \code{gon}, \code{line} and \code{names}, which is also the format of the output. See 'value' for details.}
\item{valence}{A numeric vector (output from \code{map.valence}). If NULL, it is computed inside the function. As this may take some time (e.g. about 1 minute for a world database) but is needed in two functions, it is convenient to calculate it separately.}
}
\value{ All these functions, except \code{map.valence}, return an indexed data set of a map. After every step, the details may be different but the format is the same:
\itemize{
  \item[names: the names of all the polygons.}
  \item{line: all the lines in the set, with indexes and some extra information:
  \itemize{
    \item{nline: the number of lines in the data set.}
    \item{x,y: the actual lines, separated by NA's.}
    \item{begin, end: vectors of length \code{nline} pointing at the first and last point of every line in the data vectors \code{x,y}.}
    \item{length: vector of length \code{nline} containing the length of every vector}
    \item{left, right: vectors of length \code{nline} conatining the number of the polygon at the left and right of the line. May be missing or incomplete at some intermediate stages.}
    \item{scale: the line data may be scaled and rounded to integer.}}}
  \item{gon: description of all the polygons:
  \itemize{
    \item{ngon: the number of polygons.}
    \item{length: vector of length \code{ngon}. The number of lines that every polygon conists of.}
    \item{data: all the lines that constitute the polygons. Negative values mean the line is followed in reverse direction. There is no NA separating the different polygons, so you need exact begin and end points.}
    \item{begin, end: vectors of length \code{ngon}: the begin and end point in the data vector.}}}
}
\details{This set of functions performs the various steps necessary to transform a set of polygons into a data set of polylines that may be used as input for \code{maps}. A necessary condition is that the polygons need to have exactly matching borders. If this is not the case, the result may be completely random.

See 'examples' for quick discussion of th evarious functions.

}
\examples{\dontrun{
# a typical transformation sequence:
be <- read.admin1(countries="BE") # read provincial borders
map(be) # you can use map() to see the result
be1 <- map.make(be) # turn into an internal format
be2 <- map.split(be1) # split the polylines into segments consisting of 2 points
be3 <- map.dups(be2)  # remove all dupplicate segments
val <- map.valence(be3) # calculate the 'valence' of every point (number of segments it belongs to)
be4 <- map.shift.gon(be3,val) # shift the lines forming a polygon, until the polygon starts at a vertex
be5 <- map.merge.segments(be4,val) # merge all segments to polylines that begin and end at a vertex
map.export.bin(be5,file=paste(MY_MAP_PATH,"belgium",sep="/")) # save as a binary database
Sys.setenv("R_MY_MAP_PATH"=MY_MAP_PATH) # map() uses environment variables to find data files
belgiumMapEnv <- "R_MY_MAP_PATH"
map(database="belgium") # see the final result
}}

\seealso{
  \code{\link{maps:map}}, \code{\link{read.region}}, \code{\link{map.valence}} }

