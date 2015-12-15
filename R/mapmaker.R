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
map.make <- function(map, rounding=9){
  nline <- sum(is.na(map$x)) + 1
  ngon <- nline
# make sure there is no trailing NA
  if (is.na(map$x[length(map$x)])) {
    map$x <- map$x[-length(map$x)]
    map$y <- map$y[-length(map$x)]
  }
  gondata <- rep(NA,2*ngon)
  gondata[seq(1,2*ngon,by=2)] <- 1:ngon
  gon <- list(length = rep(1,ngon),
              begin = seq(1,2*ngon-1,by=2),
              end   = seq(1,2*ngon-1,by=2),
              data   = gondata,
              ngon = ngon)
# You need to do some rounding later on!
# NE has a precision of 9 decimals
  map$x <- round(map$x, rounding)
  map$y <- round(map$y, rounding)
# remove duplicate points
# from a polygon dataset
# because they mess up the segment-splitting
  NX <- length(map$x)
  cleanup <- .C("mapclean",x=map$x,y=map$y, len=as.integer(NX),
                x_out=numeric(NX),y_out=numeric(NX),len_out=integer(1),
                NAOK=TRUE)
  if(cleanup$len_out < NX) cat("mapclean removed",NX-cleanup$len_out,"points.\n")
#  data.frame(x=result$nx[1:result$nlen],y=result$ny[1:result$nlen])
  x <- cleanup$x_out[1:cleanup$len_out]
  y <- cleanup$y_out[1:cleanup$len_out]
  linoffset <- c(1,which(is.na(x))+1)
  linlen <- c(linoffset[2:nline]-linoffset[1:(nline-1)] - 1,
              length(x)- linoffset[ngon] + 1)
  line <- list(begin = linoffset,
               end   = linoffset + linlen - 1,
               length = linlen,
               left = rep(0,nline),
               right = 1:nline,
               nline = nline)

  list(x=x, y=y, gon=gon, line=line, names=map$names)
}


# step 2 : split al lines into segments 
map.split <- function(ww) {
  ngon <- ww$gon$ngon
  gon2 <- list(ngon=ww$gon$ngon)
  gon2$length <- ww$line$length - 1
  gon2$begin <- c(0,cumsum(gon2$length[-ngon])) + 1
  gon2$end <- gon2$begin + gon2$length - 1
  gon2$data <- 1:sum(gon2$length)

# len=2 -> 1 line
#     3    2
#     4
  nline2 <- sum(ww$line$length - 1) 
  line2 <- list(nline=nline2)
  line2$length <- rep(2,nline2)
  line2$left <- rep(0,nline2)
  line2$right <- rep(1:ngon,times=gon2$length)
# a b c -> a b NA b c
# internal points b -> b NA b ==> 
 
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
  line2$begin <- seq(1,3*nline2,by=3)
  line2$end <- seq(2,3*nline2,by=3)

  list(x=x2, y=y2, gon=gon2, line=line2, names=ww$names)
}

# step 3 : remove duplicate segments
# the c code returns a vector of length nline
# value v[j] indicates:
# 0 : this segment is unique or the copy is to be removed
# i : identical to segment i (should never happen!)
#-i : reverse of segment i
map.dups <- function(ww){
  ttt <- .C("mapdups",x=as.numeric(ww$x),
                      y=as.numeric(ww$y),
                      nx=as.integer(ww$line$nline),
                      result=integer(ww$line$nline),NAOK=TRUE)
  if (any(ttt$result>0)) warning("Some polygons have different winding!")
  renumber <- rep(NA,ww$line$nline)
  nline2 <- sum(ttt$result == 0)
  renumber[ttt$result==0] <- 1:nline2
  renumber[ttt$result > 0] <- renumber[ttt$result[ttt$result>0]]
  renumber[ttt$result < 0] <- -renumber[abs(ttt$result[ttt$result<0])]
  line2 <- list(nline=nline2,
                begin=seq(1,3*nline2,by=3),
                end=seq(2,3*nline2,by=3),
                length=rep(2,nline2),
                right=ww$line$right[ttt$result==0])
  off1 <- ww$line$begin[ttt$result==0]
  x2 <- rep(NA,3*nline2-1)
  y2 <- rep(NA,3*nline2-1)
  x2[seq(1,3*nline2,by=3)] <- ww$x[off1]
  x2[seq(2,3*nline2,by=3)] <- ww$x[(off1 + 1)]
  y2[seq(1,3*nline2,by=3)] <- ww$y[off1]
  y2[seq(2,3*nline2,by=3)] <- ww$y[(off1 + 1)]

  gon2 <- ww$gon
  gon2$data <- vapply(gon2$data,function(i) renumber[i],FUN.VALUE=1)
  list(x=x2, y=y2, gon=gon2,line=line2,names=ww$names)
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
  bval <- valence[ww$line$begin]
  eval <- valence[ww$line$end]
  gval <- ifelse(ww$gon$data>0,bval[abs(ww$gon$data)],eval[abs(ww$gon$data)])
  gbval <- gval[ww$gon$begin]

# gon$data values may be NEGATIVE, in which case bval[] is an error
# if the value is negative: ww$line$end

  gcheck <- which(gbval==2) # the gons that don't start with a vertex
  for (gg in gcheck){
    vert <- which(gval[ww$gon$begin[gg]:ww$gon$end[gg]] > 2)
    if(length(vert) > 0 ){ # not an island
      bb <- ww$gon$begin[gg]
      ee <- ww$gon$end[gg]
      ll <- ww$gon$length[gg]
#      cat("gg=",gg,"bb=",bb,"ee=",ee,"ll=",ll,"vert[1]=",vert[1],"\n")
      ww$gon$data[bb:ee] <- ww$gon$data[bb:ee][c(vert[1]:ll,1:(vert[1]-1))]
    }
  }
  ww
}

# step 4c : merge segments to lines
map.merge.segments <- function(ww,valence=NULL) {
# valence 1 is impossible when all lines are still single segments
# when valence=2 you could merge, except if it's a closed loop
  if (is.null(valence)) valence <- map.valence(ww)
  if (any(valence==1)) warning("There appear to be loose points?")

  xlen <- length(ww$x)
  merging <- .C("mapmerge_seg",
                          x=as.numeric(ww$x),
                          y=as.numeric(ww$y),
                          xlen=as.integer(xlen),
                          valence=as.integer(valence),
                          linebuf=integer(ww$line$nline),
                          gon=as.integer(ww$gon$data),
                          gonlen=as.integer(ww$gon$length),
                          ngon=as.integer(ww$gon$ngon),
                          x_out=numeric(xlen),
                          y_out=numeric(xlen),
                          xlen_out=as.integer(0),
                          gon_out=integer(length(ww$gon$data)),
                          gonlen_out=integer(ww$gon$ngon),
                          NAOK=TRUE)
  x2 <- merging$x_out[1:merging$xlen_out]
  y2 <- merging$y_out[1:merging$xlen_out]
  line2 <- list(nline = sum(is.na(x2)) + 1,
                begin = c(1,which(is.na(x2))+1),
                end = c(which(is.na(x2))-1,length(x2)))
  line2$length = line2$end - line2$begin + 1
  
  
  gon2 <- list(ngon=ww$gon$ngon,
               length=merging$gonlen_out,
               begin=c(1,cumsum(merging$gonlen_out)[-ww$gon$ngon]+1),
               end=cumsum(merging$gonlen_out),
               data=merging$gon_out[1:sum(merging$gonlen_out)])
  ww2 <- list(x=x2, y=y2, line=line2,gon=gon2,names=ww$names)

  map.LR(ww2)
}

###############################################################################

# to which polygon does a given line belong?
which.gon <- function(ll,ww){
  pplist <- which(ww$gon$data == ll)
  if (length(pplist)==0) return(0)
# vapply wil give an error if there are two matching polygons
# that should never happen, but if the orientation of some polygons is inconsistent?...
  vapply(pplist, function(pp) which( ww$gon$begin <= pp & ww$gon$end >= pp),
         FUN.VALUE=1)
}

map.LR <- function(ww) {
  ww$line$left  <- vapply(1:ww$line$nline, function(ll) which.gon(-ll,ww)[1],FUN.VALUE=1)
  ww$line$right <- vapply(1:ww$line$nline, function(ll) which.gon( ll,ww)[1],FUN.VALUE=1)
  ww
}



