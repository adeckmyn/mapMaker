### OUTPUT: routines to create the new source files 

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


# write directly to 'maps' binary format
# the original C code is much more efficient, but this routine runs
# without compilation and without first writing to ASCII
map.export.bin <- function(ww,outfile, scale=pi/180){
  type_settings <- .C(maptypes,result=integer(4))$result
  names(type_settings)=c("char","short","int","float")
  cat("platform:",type_settings,"\n")

  nline <- ww$line$nline
  ngon <- ww$gon$ngon

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

  ff <- file(lfile, open="wb")
# header part
  writeBin(as.integer(2),ff,size=type_settings$int) # 2=line type "sphere"
  writeBin(as.integer(ww$line$nline),ff,size=type_settings$int)
# for every line: offset, npair, left & right polygon, SW & NE limits
  offset <- 2*type_settings$int + nline * (type_settings$int + 3*type_settings$short + 4*type_settings$float)
  for(i in 1:nline){
    writeBin(as.integer(offset),ff,size=type_settings$int,signed=FALSE)
    writeBin(as.integer(ww$line$length[i]),ff,size=type_settings$short, signed=FALSE)
    writeBin(as.integer(ww$line$left[i]),ff,size=type_settings$short, signed=FALSE)
    writeBin(as.integer(ww$line$right[i]),ff,size=type_settings$short, signed=FALSE)
    writeBin(line.limits[i,],ff,size=type_settings$float)
    offset <- offset + 2*ww$line$length[i]*type_settings$float
  }
# xy data
  xy <- cbind(lx,ly)[!is.na(lx),]
  writeBin(xy,ff,length=type_settings$float)
# linestat
  system(paste('rm -f',lsfile))
  write(paste(ww$line$nline,max(ww$line$length)),file=lsfile,append=TRUE)
  close(ff)

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
  ff <- file(gfile, open="wb")

# header
  writeBin(as.integer(ww$gon$ngon),ff,size=type_settings$short)
  offset <- type_settings$short + ngon * (type_settings$int + + type_settings$char + 4*type_settings$float)
  for (gg in 1:ngon) {
    writeBin(as.integer(offset),size=type_settings$int, signed=FALSE)
    writeBin(as.integer(ww$gon$length[gg]),size=type_settings$char, signed=FALSE)
    writeBin(gon.limits[i,],ff,size=type_settings$float)
    offset <- offset + ww$gon$length[gg] * type_settings$int
  } 
# data 
  write(as.integer(ww$gon$data),ff,size=type_settings$int)
  close(ff)

#########
# NAMES # (identical to ascii version!)
#########
  nfile <- paste(outfile,'.N',sep='')
  system(paste('rm -f',nfile))
  write.table(cbind(ww$names,seq_along(ww$names)),sep='\t',
              quote=FALSE,col.names=FALSE,row.names=FALSE,
              file=nfile)

}

