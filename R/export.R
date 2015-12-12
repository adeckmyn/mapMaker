### OUTPUT: routines to create the new source files 

map.export.ascii <- function(ww, outfile, ndec=NULL) {
# line data
  lfile <- paste(outfile,'.line',sep='')
  lsfile <- paste(outfile,'.linestats',sep='')
  if (is.null(ndec)) ndec <- ww$line$scale
  lx <- round(ww$line$x / 10^ww$line$scale,ndec)
  ly <- round(ww$line$y / 10^ww$line$scale,ndec)
  system(paste('rm -f',lfile,lsfile))
  for(loc in 1:ww$line$nlines){
    write(paste(ww$line$left[loc],ww$line$right[loc]),file=lfile,append=TRUE)
    write(rbind('',format(
            rbind(lx[ww$line$begin[loc]:ww$line$end[loc]],
                  ly[ww$line$begin[loc]:ww$line$end[loc]]),
            nsmall=ndec)),
          file=lfile,append=TRUE,ncolumns=3)

    write('EOR', file=lfile,append=TRUE)
  }
# linestat
  system(paste('rm -f',lsfile))
  write(paste(ww$line$nlines,max(ww$line$length)),file=lsfile,append=TRUE)

# gon & name
  ind <- 1:ww$gon$ngons
  gfile <- paste(outfile,'.gon',sep='')
  gsfile <- paste(outfile,'.gonstats',sep='')
  nfile <- paste(outfile,'.name',sep='')
  system(paste('rm -f',gfile,nfile,gsfile))
  for(loc in 1:ww$gon$ngons){
### for exact match: 1 blank before the numbers
    write(paste('',ww$gon$data[ww$gon$begin[loc]:ww$gon$end[loc]]),
          file=gfile,append=TRUE,ncolumns=1)
    write('EOR', file=gfile,append=TRUE)
    write(paste(ww$gon$name[loc],loc,sep='\t'),file=nfile,append=TRUE)
  }
  write(paste(ww$gon$ngons,max(ww$gon$length)),file=gsfile,append=TRUE)
  
}


# write directly to 'maps' binary format
map.export.bin <- function(ww,filename){
  type_settings <- .C(maptypes,result=integer(4))$result
  names(type_settings)=c("char","short","int","float")
# line data
  lfile <- paste(outfile,'.L',sep='')
  if (is.null(ndec)) ndec <- ww$line$scale
  lx <- ww$line$x / 10^ww$line$scale
  ly <- ww$line$y / 10^ww$line$scale
  system(paste('rm -f',lfile))
  ff <- open(lfile, open="wb")
# header part
  writeBin(ff,as.integer(ww$line$nline),length=type_settings$short)

# xy data
  xy <- cbind(ww$line$x,ww$line$y)[!is.na(ww$line$x),]
  writeBin(ff,xy,length=type_settings$float)
# linestat
  system(paste('rm -f',lsfile))
  write(paste(ww$line$nlines,max(ww$line$length)),file=lsfile,append=TRUE)

## polygon data
  ind <- 1:ww$gon$ngons
  gfile <- paste(outfile,'.G',sep='')
  system(paste('rm -f',gfile))

## names (identical to ascii version!)
  nfile <- paste(outfile,'.N',sep='')
  system(paste('rm -f',nfile))
  write.table(cbind(ww$gon$name,seq_along(ww$gon$name)),sep='\t',
              quote=FALSE,col.names=FALSE,row.names=FALSE,
              file=nfile)
}

