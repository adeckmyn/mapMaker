### INPUT: read source data (either ASCII or BINARY format)

map.read.ascii <- function(infile, scale=180/pi){
### read line data
  data <- read.table(paste(infile,'.line',sep=''),
                     col.names=c('x','y'),na.strings='EOR',fill=TRUE)
  nline <- sum(is.na(data$x))
# first element of every line identifies the polygons it separates
  gonloc <- c(1,(which(is.na(data$x))+1))[1:nline]
# the actual line elements: (drop the last NA)
  x <- head(data$x[-gonloc],-1)*scale
  y <- head(data$y[-gonloc],-1)*scale
  line <- list(nline=nline,
               left=data$x[gonloc],
               right=data$y[gonloc],
               begin=c(1,which(is.na(x))+1),
               end=c(which(is.na(x))-1,length(x)))
  line$length <- line$end - line$begin + 1
### polygon data
  data <- scan(paste(infile,'.gon',sep=''),na.strings='EOR',quiet=TRUE)
# drop the last NA
  data <- head(data,-1)
  gon <- list(ngon=sum(is.na(data))+1,
              begin=c(1,which(is.na(data))+1),
              end=c(which(is.na(data))-1,length(data)),
              data=data)
  gon$length <- gon$end - gon$begin + 1

### names
  names <- read.table(paste(infile,'.name',sep=''),sep='\t',quote="",
                   stringsAsFactors=FALSE,
                   col.names=c('name','index'))$name

  list(x=x, y=y, line=line, gon=gon, names=names)
}

map.read.bin <- function(infile, scale=180/pi) {
  type_settings <- .C("mapsizes",result=integer(4))$result
  names(type_settings) <- c("char","short","int","float")
  type_settings <- as.list(type_settings)

# line data
  lfile <- paste(infile,".L",sep="")
  lf <- file(lfile,open="rb")
  on.exit(try(close(lf),silent=TRUE))
  seek(lf,0)
  maptype <- readBin(lf,"int",size=type_settings$int,n=1) # ==2 for sphere
  nline   <- readBin(lf,"int",size=type_settings$int,n=1)
#  cat("type=",maptype, "nline=",nline,"\n")
  line.header <- matrix(NA,ncol=9,nrow=nline)
  for(i in 1:nline) {
    line.header[i,1] <- readBin(lf,"int",size=type_settings$int,n=1) # signed=F does not work for int
    line.header[i,2] <- readBin(lf,"int",size=type_settings$short,n=1,signed=FALSE)
    line.header[i,3] <- readBin(lf,"int",size=type_settings$short,n=1,signed=FALSE)
    line.header[i,4] <- readBin(lf,"int",size=type_settings$short,n=1,signed=FALSE)
    line.header[i,5] <- readBin(lf,"int",size=type_settings$short,n=1,signed=FALSE) # just padding
    line.header[i,6] <- readBin(lf,"double",size=type_settings$float,n=1)
    line.header[i,7] <- readBin(lf,"double",size=type_settings$float,n=1)
    line.header[i,8] <- readBin(lf,"double",size=type_settings$float,n=1)
    line.header[i,9] <- readBin(lf,"double",size=type_settings$float,n=1)
  }
  line <- as.data.frame(line.header)
  names(line) <- c("offset","length","left","right","padding","W","S","E","N")
  xy <- matrix(readBin(lf,"double",size=type_settings$float,n=2*sum(line$length)),
               ncol=2,byrow=TRUE)
## TODO: split xy by NA's
  close(lf)

 
# gon data
  gfile <- paste(infile,".G",sep="")
  gf <- file(gfile,open="rb")
  on.exit(try(close(gf),silent=TRUE))
  seek(gf,0)

  ngon <- readBin(gf,"int",size=type_settings$short,n=1)
  gon.header <- matrix(NA,ncol=8,nrow=ngon)
  for(i in 1:ngon) {
    gon.header[i,1] <- readBin(gf,"int",size=type_settings$int,n=1)
    gon.header[i,2] <- readBin(gf,"int",size=type_settings$char,n=1,signed=FALSE)
    gon.header[i,3] <- readBin(gf,"int",size=type_settings$char,n=1) # just padding
    gon.header[i,4] <- readBin(gf,"int",size=type_settings$short,n=1) # just padding
    gon.header[i,5] <- readBin(gf,"double",size=type_settings$float,n=1)
    gon.header[i,6] <- readBin(gf,"double",size=type_settings$float,n=1)
    gon.header[i,7] <- readBin(gf,"double",size=type_settings$float,n=1)
    gon.header[i,8] <- readBin(gf,"double",size=type_settings$float,n=1)
  }
  gon <- as.data.frame(gon.header)
  names(gon) <- c("offset","length","padding1","padding2","W","S","E","N")
  gondata <- readBin(gf,"int",size=type_settings$int,n=sum(gon$length))
  close(gf)

### names
  names <- read.table(paste(infile,'.N',sep=''),sep='\t',quote="",
                   stringsAsFactors=FALSE,
                   col.names=c('name','index'))$name

  list(line=line,gon=gon,xy=xy,data=gondata,names=names)
}

