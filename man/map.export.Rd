\name{map.export}
\alias{map.export.ascii}
\alias{map.export.bin}
\title{Write map data out to file}
\description{
Write the internal representation of polylines and polygons to files. Either ascii (for portability, but needs to be 'compiled' for use by maps) or directly to binary.
}
\usage{
map.export.ascii(ww, outfile, scale=pi/180, ndec=10)
map.export.bin(ww,outfile, scale=pi/180)
}
\arguments{
\item{ww}{A list with components \code{gon}, \code{line} and \code{names}, the internal representation of mapMaker.}
\item{outfile}{Name of file(s) to write to. The extensions are added (see details).}
\item{scale}{Rescaling of the data. Usually, data is writen in radians.}
\item{ndec}{Number of decimals to write in the ascii file.}
}
\value{No return data. The data is written to files.}
\details{\itemize{
\item{The ascii version is written to five files with extensions '.line', '.linestats', '.gon', '.gonstats', '.name'. These files correspond to those in e.g. the source code of the 'maps' package. They are portable, but need compilation to binary before being usable. The maps package contains C code to do this at install time. }
\item{The binary code is in three files with extensions '.L', '.G', '.N'. This corresponds to the files in an installed library. It is not completely portable (e.g. endianness and compiler types influence the binary content.}}}
\seealso{
  \code{\link{maps:map}}, \code{\link{map.make}} }

