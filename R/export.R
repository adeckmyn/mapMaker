### OUTPUT: routines to create the new source files 
map.export <- function(ww, outfile, type="bin", ...) {
  if (type=="bin") map.export.bin(ww,outfile,...)
  else map.export.ascii(ww,outfile,...)
}

map.export.ascii <- function(ww, outfile, scale=pi/180, ndec=10) {
# line data
  lfile <- paste(outfile,'.line',sep='')
  lx <- round(ww$x * scale,ndec)
  ly <- round(ww$y * scale,ndec)
  system(paste('rm -f',lfile))
  for(loc in 1:ww$line$nline){
    write(paste(ww$line$left[loc],ww$line$right[loc]),file=lfile,append=TRUE)
    write(rbind('',format(
            rbind(lx[ww$line$begin[loc]:ww$line$end[loc]],
                  ly[ww$line$begin[loc]:ww$line$end[loc]]),
            nsmall=ndec)),
          file=lfile,append=TRUE,ncolumns=3)

    write('EOR', file=lfile,append=TRUE)
  }
# linestat
  lsfile <- paste(outfile,'.linestats',sep='')
  system(paste('rm -f',lsfile))
  write(paste(ww$line$nline,max(ww$line$length)),file=lsfile,append=TRUE)

# gon
  ind <- 1:ww$gon$ngon
  gfile <- paste(outfile,'.gon',sep='')
  system(paste('rm -f',gfile))
  for(loc in 1:ww$gon$ngon){
### for exact match: 1 blank before the numbers
    write(paste('',ww$gon$data[ww$gon$begin[loc]:ww$gon$end[loc]]),
          file=gfile,append=TRUE,ncolumns=1)
    write('EOR', file=gfile,append=TRUE)
  }
# gonstat
  gsfile <- paste(outfile,'.gonstats',sep='')
  system(paste('rm -f',gsfile))
  write(paste(ww$gon$ngon,max(ww$gon$length)),file=gsfile,append=TRUE)

## names
  nfile <- paste(outfile,'.name',sep='')
  system(paste('rm -f',nfile))
  write.table(cbind(ww$names,seq_along(ww$names)),sep='\t',
              quote=FALSE,col.names=FALSE,row.names=FALSE,
              file=nfile)
}

##########################################
# write directly to 'maps' binary format #
##########################################

# the original C code is much more efficient, but this routine runs
# without compilation and without first writing to ASCII
### R can not write "unsigned"!
### So we have to assume some limitations:
### line length must be < 2^15 rather than < 2^16 
###        (OK for 1:10 world map: just over 2^14 = 16384, worldHires has ~19000 )
### file size must be < 2^31 (OK)
### UNFORTUNATELY: the original C code writes the struct directly -> compiler-dependent padding...
map.export.bin <- function(ww, outfile, scale=pi/180){
  type_settings <- .C("mapsizes",result=integer(4))$result
  names(type_settings)=c("char","short","int","float")
#  cat("platform:",type_settings,"\n")

  nline <- ww$line$nline
  ngon <- ww$gon$ngon
  type_settings <- as.list(type_settings)

#############
# LINE DATA #
#############
  lfile <- paste(outfile,'.L',sep='')
  system(paste('rm -f',lfile))

  lx <- ww$x *scale
  ly <- ww$y *scale

# SW and NE limits of every line element:
  line.limits <- data.frame(
                   W = vapply(1:nline,function(ll) min(lx[ww$line$begin[ll]:ww$line$end[ll]]),FUN.VALUE=1),
                   S = vapply(1:nline,function(ll) min(ly[ww$line$begin[ll]:ww$line$end[ll]]),FUN.VALUE=1),
                   E = vapply(1:nline,function(ll) max(lx[ww$line$begin[ll]:ww$line$end[ll]]),FUN.VALUE=1),
                   N = vapply(1:nline,function(ll) max(ly[ww$line$begin[ll]:ww$line$end[ll]]),FUN.VALUE=1)
                 )

  lf <- file(lfile, open="wb")
# header part
  writeBin(as.integer(2),lf,size=type_settings$int) # 2=line type "sphere"
  writeBin(as.integer(ww$line$nline),lf,size=type_settings$int)
# for every line: offset, npair, left & right polygon, SW & NE limits
  offset <- 2*type_settings$int + nline * (type_settings$int + 4*type_settings$short + 4*type_settings$float)
  if (any(ww$line$length >= 2^(8*type_settings$short - 1))) stop("Line length too long: R can not write unsigned short.")
  for(i in 1:nline){
    writeBin(as.integer(offset),lf,size=type_settings$int)
    writeBin(as.integer(ww$line$length[i]),lf,size=type_settings$short)
    writeBin(as.integer(ww$line$left[i]),lf,size=type_settings$short)
    writeBin(as.integer(ww$line$right[i]),lf,size=type_settings$short)
    writeBin(as.integer(0),lf,size=type_settings$short) # padding: a hack
    writeBin(as.numeric(line.limits[i,]),lf,size=type_settings$float)
    offset <- offset + 2*ww$line$length[i]*type_settings$float
  }
# xy data
  xy <- rbind(lx,ly)[,!is.na(lx)]
  writeBin(as.numeric(xy),lf,size=type_settings$float)
  close(lf)

################
# POLYGON DATA #
################
  gfile <- paste(outfile,'.G',sep='')
  system(paste('rm -f',gfile))

# SW and NE limits of every polygon (calculate from the line element limits OR reconstitute whole polygon)
  gon.limits <- data.frame(
                  W = vapply(1:ngon, function(gg) min(line.limits$W[which(1:nline %in% 
                                             abs(ww$gon$data[ww$gon$begin[gg]:ww$gon$end[gg]]))]),FUN.VALUE=1),
                  S = vapply(1:ngon, function(gg) min(line.limits$S[which(1:nline %in% 
                                             abs(ww$gon$data[ww$gon$begin[gg]:ww$gon$end[gg]]))]),FUN.VALUE=1),
                  E = vapply(1:ngon, function(gg) max(line.limits$E[which(1:nline %in% 
                                             abs(ww$gon$data[ww$gon$begin[gg]:ww$gon$end[gg]]))]),FUN.VALUE=1),
                  N = vapply(1:ngon, function(gg) max(line.limits$N[which(1:nline %in% 
                                             abs(ww$gon$data[ww$gon$begin[gg]:ww$gon$end[gg]]))]),FUN.VALUE=1)
                 )
  gf <- file(gfile, open="wb")

# header
  writeBin(as.integer(ww$gon$ngon),gf,size=type_settings$short)
  offset <- type_settings$short + ngon * (type_settings$int + 4*type_settings$char + 4*type_settings$float)
  for (for i in 1:ngon) {
    writeBin(as.integer(offset),gf,size=type_settings$int)
    writeBin(as.integer(ww$gon$length[i]),gf,size=type_settings$char)
    writeBin(integer(3),gf,size=type_settings$char) #padding
    writeBin(as.numeric(gon.limits[i,]),gf,size=type_settings$float)
    offset <- offset + ww$gon$length[i] * type_settings$int
  } 
# data 
  writeBin(as.integer(ww$gon$data),gf,size=type_settings$int)
  close(gf)

#########
# NAMES # (identical to ascii version!)
#########
  nfile <- paste(outfile,'.N',sep='')
  system(paste('rm -f',nfile))
  write.table(cbind(ww$names,seq_along(ww$names)),sep='\t',
              quote=FALSE,col.names=FALSE,row.names=FALSE,
              file=nfile)

}

