insert.points <- function(x, i, y) {
# insert points y at locations i in vector x
  if (max(i) > length(x)) stop("Vector x too short.")
  if (length(i) != length(y)) {
    if (length(y)==1) y <- rep(y,length(i))
    else if (length(y)<length(i)) y <- y[seq_along(i)]
    else stop("Vectors don't match.")
  }
  ni <- length(i)
  si <- order(i)
  i <- c(i[si],length(x)+1)
  y <- y[si]

  x1 <- c(head(x,i[1]-1), 
          lapply(1:ni, function(j) c(y[j], x[i[j]:(i[(j + 1)] - 1)])) )
  unlist(x1)
}

############################

line.parse <- function(ww) {
  line <- data.frame(begin = c(1,which(is.na(ww$x))+1),
               end = c(which(is.na(ww$x))-1, length(ww$x)))
  line$length <- line$end - line$begin + 1

  line
}

gon.parse <- function(ww) {
  gon <- data.frame(begin = c(1,which(is.na(ww$gondata)) + 1),
              end   = c(which(is.na(ww$gondata)) - 1, length(ww$gondata)))
  gon$length <- gon$end - gon$begin + 1
  gon
}

#######################3

get.line <- function(ww, i) {
  plist <- if (i > 0)  ww$line$begin[i]:ww$line$end[i] else ww$line$end[abs(i)]:ww$line$begin[abs(i)]
  data.frame(x = ww$x[plist],
             y = ww$y[plist])
}

get.gon <- function(ww, i) {
  linelist <- ww$gon$begin[i]:ww$gon$end[i]
  ww$gondata[linelist]
#  data.frame(line=ww$gon$data[linelist],
#             left =ifelse(linelist>0,ww$line$left[linelist],ww$line$right[abs(linelist)]),
#             right=ifelse(linelist<0,ww$line$left[abs(linelist)],ww$line$right[linelist]))
}

get.fullgon <- function(ww, i){
  llist <- get.gon(ww, i)
  nl <- length(llist)
  data <- lapply(1:nl, function(j) if (j < nl) head(get.line(ww,llist[j]),-1) else get.line(ww,llist[j]) )
  do.call(rbind, data)
}
  

########################

# to which polygon(s) does a given line belong?
which.gon <- function(ww, i){
  pplist <- which(ww$gondata == i)
  if (length(pplist)==0) return(0)
  vapply(pplist, function(pp) which( ww$gon$begin <= pp & ww$gon$end >= pp),
         FUN.VALUE=1)
}

# to which line does a given point belong?
which.line <- function(ww, i){
  which(ww$line$begin <= i & ww$line$end >= i)
}

#########################

gon.remove <- function(ww, i){
  ilen <- ww$gon$length[i]
  ww$gondata <- c(head(ww$gondata,ww$gon$begin[i]-1), tail(ww$gondata, -(ww$gon$end[i]+1)))
  ww$gon <- gon.parse(ww)
  ww
}

gon.change <- function(ww, i, data){
  len0 <- ww$gon$length[i]
  len1 <- length(data)
  if (len0==len1) {
    ww$gondata[ww$gon$begin[i]:ww$gon$end[i]] <- data
  } else {
    ww$gondata <- c(head(ww$gondata,ww$gon$begin[i]-1),data,tail(ww$gondata,-ww$gon$end[i]))
    ww$gon <- gon.parse(ww)
  }
  ww
}

line.remove <- function(ww, i){
  if (any(abs(ww$gondata) == i)) stop(paste("Line",i,"is still in use."))
  ilen <- ww$line$length[i] + 1 # include NA

  ww$x <- c(head(ww$x,ww$line$begin[i]-1),tail(ww$x,-(ww$line$end[i]+1)))
  ww$y <- c(head(ww$y,ww$line$begin[i]-1),tail(ww$y,-(ww$line$end[i]+1)))
  ww$line <- line.parse(ww)
 
  ww
}  

line.change <- function(ww, i, data){
  len0 <- ww$line$length[i]
  len1 <- dim(data)[1]
  if (len0==len1) {
    ww$x[ww$line$begin[i]:ww$line$end[i]] <- data[,1]
    ww$y[ww$line$begin[i]:ww$line$end[i]] <- data[,2]
  } else {
    ww$x <- c(head(ww$x, ww$line$begin[i]-1),data[,1],tail(ww$x,-ww$line$end[i]))
    ww$y <- c(head(ww$y, ww$line$begin[i]-1),data[,2],tail(ww$y,-ww$line$end[i]))
    ww$line <- line.parse(ww)
  }
  ww
}

#############################

line.split <- function(ww, p) {
  if (is.na(ww$x[p])) stop("Can't split at NA.")
  pline <- which.line(ww, p)

  ww$x <- c(head(ww$x,p),NA,tail(ww$x,p-1))
  ww$y <- c(head(ww$y,p),NA,tail(ww$y,p-1))
  ww$line <- line.parse(ww)
  
  iind1 <- which(ww$gondata > pline)
  ww$gondata[iind1] <- ww$gondata[iind1] + 1

  iind2 <- which(ww$gondata < -pline)
  ww$gondata[iind2] <- ww$gondata[iind2] - 1

  ww$gondata <- insert.points(ww$data, which(ww$gondata == pline)+1, pline+1) 
  ww$gondata <- insert.points(ww$data, which(ww$gondata == -pline), -pline-1) 

  ww$gon <- parse.gon(ww)

  ww
}
