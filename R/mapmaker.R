# basic polygon map: $x, $y and $names
# here, we develop tools to turn that into a polyline map
# !! output from 'map' also has $range...


# step 1 : every polygon consists of one closed polyline
#          lines are rescaled to radians x 10^precision and rounded to int
# PROBLEM: at precision 8, some segments become a repeated point
#          which gives a (bogus) warning in map.dups for a "positive direction fit"
#          but precision 9 may give numbers > maxint=2^31
#          SO: use doubles, but fill them with (large) integers
# BUT: when writing to binary map format, they are reduced to 32 bit float!


map.clean <- function(ww, precision=1.E-8) {
  nx <- length(ww$x)
  cleanup <- .C("mapclean",x=as.numeric(ww$x),y=as.numeric(ww$y), len=as.integer(nx),
                x_out=numeric(nx),y_out=numeric(nx),len_out=integer(1),
                precision=as.numeric(precision),
                NAOK=TRUE)
  if (cleanup$len_out < nx) cat("mapclean removed",nx-cleanup$len_out,"points.\n")
#  data.frame(x=result$nx[1:result$nlen],y=result$ny[1:result$nlen])
  ww$x <- cleanup$x_out[1:cleanup$len_out]
  ww$y <- cleanup$y_out[1:cleanup$len_out]
  if (!is.null(ww$line)) ww$line <- line.parse(ww)
  ww
}

map.make <- function(map){
# make sure there is no trailing NA
  if (is.na(tail(map$x,1))) {
    map$x <- head(map$x,-1)
    map$y <- head(map$y,-1)
  }
  nline <- sum(is.na(map$x)) + 1
  ngon <- nline

  gondata <- rep(NA,2*ngon-1)
  gondata[seq(1,2*ngon,by=2)] <- 1:ngon

  ww <- list(x=map$x, y=map$y, gondata=gondata, names=map$names, gon=NA, line=NA)
  ww$gon  <- gon.parse(ww)
  ww$line <- line.parse(ww)

  ww
}

# step 2 : split al lines into segments 
map.split <- function(ww) {

  nline2 <- sum(ww$line$length - 1) 
  glen2 <- cumsum(ww$line$length-1)
 
  x2 <- rep(NA,3*nline2 -1)
  y2 <- rep(NA,3*nline2 -1)
  t1 <- c(1,which(is.na(ww$x)) + 1)
  t2 <- c(which(is.na(ww$x)) -1,length(ww$x))
  c1 <- setdiff(which(!is.na(ww$x)),t2)
  c2 <- setdiff(which(!is.na(ww$x)),t1)

  x2[seq(1,3*nline2,by=3)] <- ww$x[c1]
  x2[seq(2,3*nline2,by=3)] <- ww$x[c2]
  y2[seq(1,3*nline2,by=3)] <- ww$y[c1]
  y2[seq(2,3*nline2,by=3)] <- ww$y[c2]

  ww$x <- x2
  ww$y <- y2
  ww$line <- line.parse(ww)

  ngon <- length(ww$gon$begin)
  gdata <- 1:nline2
  ww$gondata <- insert.points(gdata, head(glen2,-1) + 1, NA)
  ww$gon <- gon.parse(ww)

  ww
}

# step 3 : remove duplicate segments
# the c code returns a vector of length nline
# value v[j] indicates:
# 0 : this segment is unique or the copy is to be removed
# i : identical to segment i (should never happen!)
#-i : reverse of segment i
map.dups <- function(ww){
  nline <- dim(ww$line)[1]
  ttt <- .C("mapdups",x=as.numeric(ww$x),
                      y=as.numeric(ww$y),
                      nx=as.integer(nline),
                      result=integer(nline),NAOK=TRUE)
  if (any(ttt$result>0)) warning("Some polygons have different winding!")
  renumber <- rep(NA,nline)
  nline2 <- sum(ttt$result == 0)
  renumber[ttt$result==0] <- 1:nline2
  renumber[ttt$result > 0] <- renumber[ttt$result[ttt$result>0]]
  renumber[ttt$result < 0] <- -renumber[abs(ttt$result[ttt$result<0])]

  off1 <- ww$line$begin[ttt$result==0]
  x2 <- rep(NA,3*nline2-1)
  y2 <- rep(NA,3*nline2-1)
  x2[seq(1,3*nline2,by=3)] <- ww$x[off1]
  x2[seq(2,3*nline2,by=3)] <- ww$x[(off1 + 1)]
  y2[seq(1,3*nline2,by=3)] <- ww$y[off1]
  y2[seq(2,3*nline2,by=3)] <- ww$y[(off1 + 1)]
  ww$x <- x2
  ww$y <- y2
  ww$line <- line.parse(ww)

  ww$gondata <- vapply(ww$gondata,function(i) if (is.na(i)) NA else sign(i)*renumber[abs(i)], FUN.VALUE=1)
  ww 
}

# step 4 : merge segments to polylines
# we follow every polygon around
# and merge the segments if the end point is not a vertex.
# at every merge, al lot of administration must be done
# we do it all in-line, we know the data can only "shrink"
# in a second phase, we check if the first and last line 
# in a polygon may be merged

# step 4a: calculate valence : how many times does a point appear in the full data set
# takes a while (for 'world' map a bit more than 1 minute)
map.valence <- function(ww) {
  .C("mapvalence", x=as.numeric(ww$x),
                   y=as.numeric(ww$y),
                   len=as.integer(length(ww$x)),
                   val=integer(length(ww$x)),NAOK=TRUE)$val
}

# step 4b : make sure every polygon starts to draw from a vertex (except islands)
map.shift.gon <- function(ww,valence=NULL){
  if (is.null(valence)) valence <- map.valence(ww)
# get the valence of the first point of every line (or segment)
# the last element is also needed, as it is the first of the reversed line
  bvals <- valence[ww$line$begin]
  evals <- valence[ww$line$end]
  gvals <- ifelse(ww$gondata>0,bvals[abs(ww$gondata)],evals[abs(ww$gondata)])
  gbvals <- gvals[ww$gon$begin]

# gon$data values may be NEGATIVE, in which case bval[] is an error
# if the value is negative: ww$line$end

  gcheck <- which(gbvals==2) # the gons that don't start with a vertex
  for (gg in gcheck){
    vert <- which(gvals[ww$gon$begin[gg]:ww$gon$end[gg]] > 2)
    if(length(vert) > 0 ){ # not an island
      bb <- ww$gon$begin[gg]
      ee <- ww$gon$end[gg]
      ll <- ww$gon$length[gg]
#      cat("gg=",gg,"bb=",bb,"ee=",ee,"ll=",ll,"vert[1]=",vert[1],"\n")
      ww$gondata[bb:ee] <- ww$gondata[bb:ee][c(vert[1]:ll,1:(vert[1]-1))]
    }
  }
  ww
}

# step 4c : merge segments to lines
map.merge.segments <- function(ww,valence=NULL) {
# valence 1 is impossible when all lines are still single segments
# when valence=2 you could merge, except if it's a closed loop
  if (is.null(valence)) valence <- map.valence(ww)
  if (any(na.omit(valence)==1)) warning("There appear to be loose points? If it's just Antarctica, that's OK.")

  xlen <- length(ww$x)
  nline <- dim(ww$line)[1]
  ngon  <- dim(ww$gon)[1]
  merging <- .C("mapmerge_seg",
                          x=as.numeric(ww$x),
                          y=as.numeric(ww$y),
                          xlen=as.integer(xlen),
                          valence=as.integer(valence),
                          linebuf=integer(nline),
                          gon=as.integer(ww$gondata[!is.na(ww$gondata)]),
                          gonlen=as.integer(ww$gon$length),
                          ngon=as.integer(ngon),
                          x_out=numeric(xlen),
                          y_out=numeric(xlen),
                          xlen_out=as.integer(0),
                          gon_out=integer(length(ww$gondata)),
                          gonlen_out=integer(ngon),
                          NAOK=TRUE)

  ww$x <- merging$x_out[1:merging$xlen_out]
  ww$y <- merging$y_out[1:merging$xlen_out]
  ww$line <- line.parse(ww) 

  gdata <- head(merging$gon_out,sum(merging$gonlen_out))
  ww$gondata <- insert.points(gdata, cumsum(merging$gonlen_out[-ngon])+1, NA)
  ww$gon <- gon.parse(ww)

  ww
}

map.LR <- function(ww) {
  nline <- dim(ww$line)[1]
  ww$line$left  <- vapply(1:nline, function(i) which.gon(ww,-i)[1],FUN.VALUE=1)
  ww$line$right <- vapply(1:nline, function(i) which.gon(ww, i)[1],FUN.VALUE=1)
  ww
}

###############################################################################

map.gon2line <- function(mapdb, precision=1.E-8, quiet=FALSE){
  if (!quiet) cat("Cleaning map data to precision", precision, ".\n")
  ww <- map.clean(mapdb, precision)
  if (!quiet) cat("Adding line & polygon indices.\n")
  ww <- map.make(ww)
  if (!quiet) cat("Splitting all polygons into line segments.\n")
  ww <- map.split(ww)
  if (!quiet) cat("Removing duplicate segments.\n")
  ww <- map.dups(ww)
  if (!quiet) cat("Calculating point valences.\n")
  val <- map.valence(ww)
  if (!quiet) cat("Shifting polygons to start at vertex.\n")
  ww <- map.shift.gon(ww, val)
  if (!quiet) cat("Merging segments to polylines.\n")
  ww <- map.merge.segments(ww, val)
  if (!quiet) cat("Fixing left/right polygon for lines.\n")
  ww <- map.LR(ww)
   
  ww 
}
