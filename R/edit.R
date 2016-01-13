#' Insert a list of points into a vector at given positions
#' 
#' @param x A vector
#' @param i A vector of indices where to insert new values
#' @param y A vector of values to be inserted. Either length 1 or the same length as i.
#' @return A vector with the length(i) points inserted.
#' 
insert.points <- function(x, i, y) {
# insert points y at locations i in vector x
  if (length(i)==0) return(x)
  if (max(i) > length(x)) stop("Vector x too short.")
  if (length(i) != length(y)) {
    if (length(y)==1) y <- rep(y,length(i))
    else if (length(y)<length(i)) y <- y[seq_along(i)]
    else stop("Vectors don't match.")
  }
  ni <- length(i)
  si <- order(i)
  index <- c(i[si],length(x)+1)
  y <- y[si]

  x1 <- c(head(x,index[1]-1), 
          lapply(1:ni, function(j) c(y[j], x[index[j]:(index[(j + 1)] - 1)])) )
  unlist(x1)
}

############################
#' Parse all the lines (co-ordinates) of a map and build an index
#' 
#' @param ww A map object
#' 
line.parse <- function(ww) {
  nline <- sum(is.na(ww$x)) + 1
  line <- data.frame(begin = c(1,which(is.na(ww$x))+1),
               end = c(which(is.na(ww$x))-1, length(ww$x)))
  line$length <- line$end - line$begin + 1
## keep line.parse independent from gon ?
  if (!is.null(ww$gon)) {
    line$left  <- vapply(1:nline, function(i) which.gon(ww,-i)[1],FUN.VALUE=1)
    line$right <- vapply(1:nline, function(i) which.gon(ww, i)[1],FUN.VALUE=1)
  }
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
  ww$names <- ww$names[-i]
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
# this changes line numbers, so gondata MUST be adapted!
  i <- abs(i)
  if (any(abs(na.omit(ww$gondata)) == abs(i))) stop(paste("Line",i,"is still in use."))
  ilen <- ww$line$length[i] + 1 # include NA

  ww$x <- c(head(ww$x,ww$line$begin[i]-1),tail(ww$x,-(ww$line$end[i]+1)))
  ww$y <- c(head(ww$y,ww$line$begin[i]-1),tail(ww$y,-(ww$line$end[i]+1)))
  ww$line <- line.parse(ww)
  ww$gondata[which(ww$gondata > i )] <- ww$gondata[which(ww$gondata >  i)] - 1
  ww$gondata[which(ww$gondata < -i)] <- ww$gondata[which(ww$gondata < -i)] + 1
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

  ww$x <- c(head(ww$x,p),NA,tail(ww$x,-(p-1)))
  ww$y <- c(head(ww$y,p),NA,tail(ww$y,-(p-1)))
  ww$line <- line.parse(ww)
  
  iind1 <- which(ww$gondata > pline)
  ww$gondata[iind1] <- ww$gondata[iind1] + 1

  iind2 <- which(ww$gondata < -pline)
  ww$gondata[iind2] <- ww$gondata[iind2] - 1

  ww$gondata <- insert.points(ww$gondata, which(ww$gondata == pline)+1, pline+1) 
  iind3 <- which(ww$gondata == -pline)
  if (length(iind3)>0) ww$gondata <- insert.points(ww$gondata, iind3, -pline-1) 

  ww$gon <- gon.parse(ww)

  ww
}

###

gon.check <- function(ww, i){
  gon <- get.gon(ww, i)
  for(j in gon){
    lin <- get.line(ww,j)
    print(c(lin$x[1],lin$y[1]),20)
    print(c(tail(lin$x,1),tail(lin$y,1)),20)
  }
}

###

line.append <- function(ww, data) {
  ww$x <- c(ww$x, NA, data[,1])
  ww$y <- c(ww$y, NA, data[,2])
  ww$line <- line.parse(ww)
  ww
}

gon.append <- function(ww, data, name) {
  ww$names <- c(ww$names,name)
  ww$gondata <- c(ww$gondata, NA, data)
  ww$gon <- gon.parse(ww)
  ww
}

