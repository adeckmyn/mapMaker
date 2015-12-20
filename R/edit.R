get.line <- function(ww, i) {
  data.frame(x = ww$x[ww$line$begin[i]:ww$line$end[i]],
             y = ww$y[ww$line$begin[i]:ww$line$end[i]])
}

get.gon <- function(ww, i) {
  linelist <- ww$gon$begin[i]:ww$gon$end[i]
  ww$gon$data[linelist]
#  data.frame(line=ww$gon$data[linelist],
#             left =ifelse(linelist>0,ww$line$left[linelist],ww$line$right[abs(linelist)]),
#             right=ifelse(linelist<0,ww$line$left[abs(linelist)],ww$line$right[linelist]))
}

########################

# to which polygon does a given line belong?
which.gon <- function(ll,ww){
  pplist <- which(ww$gon$data == ll)
  if (length(pplist)==0) return(0)
# vapply wil give an error if there are two matching polygons
# that should never happen, but if the orientation of some polygons is inconsistent?...
  vapply(pplist, function(pp) which( ww$gon$begin <= pp & ww$gon$end >= pp),
         FUN.VALUE=1)
}

# to which line does a given point belong?
which.line <- function(pp,ww){
  which(ww$line$begin <= pp & ww$line$end >= pp)
}





#########################

gon.remove <- function(ww, i){
  ww$gon$data <- ww$gon$data[-(ww$gon$begin[i]:ww$gon$end[i])]
  if (i==1) drop.index <- 2:ww$gon$ngon
  else if (i==ww$gon$ngon) drop.index <- 1:(ww$gon$ngon-1)
  else  drop.index <- c(1:(i-1),(i+1):ww$gon$ngon)
  ww$gon$begin  <- ww$gon$begin[drop.index]
  ww$gon$end    <- ww$gon$end[drop.index]
  ww$gon$length <- ww$gon$lenght[drop.index]
  ww$gon$ngon <- ww$gon$ngon-1
  ww
}

gon.change <- function(ww, i, data){
  len0 <- ww$gon$length[i]
  len1 <- length(data)
  ngon <- ww$gon$ngon
  if (len0==len1) {
    ww$gon$data[ww$gon$begin[i]:ww$gon$end[i]] <- data
  } else {
    ww$gon$data <- c(head(ww$gon$data,ww$gon$begin[i]-1),data,tail(ww$gon$data,-ww$gon$end[i]))
    if (i < ngon) ww$gon$begin[(i+1):ngon] <-  ww$gon$begin[(i+1):ngon] + (len1-len0)
    ww$gon$end[i:ngon] <- ww$gon$end[i:ngon] + (len1-len0)
    ww$gon$length[i] <- len1
  }
  ww
}

line.change <- function(ww, i, data){
  len0 <- ww$line$length[i]
  len1 <- dim(data)[1]
  nline <- ww$line$nline
  if (len0==len1) {
    ww$x[ww$line$begin[i]:ww$line$end[i]] <- data[,1]
    ww$y[ww$line$begin[i]:ww$line$end[i]] <- data[,2]
  } else {
    ww$x <- c(head(ww$x, ww$line$begin[i]-1),data[,1],tail(ww$x,-ww$line$end[i]))
    ww$y <- c(head(ww$y, ww$line$begin[i]-1),data[,2],tail(ww$y,-ww$line$end[i]))
    if (i < nline) ww$line$begin[(i+1):nline] <-  ww$line$begin[(i+1):nline] + (len1-len0)
    ww$line$end[i:nline] <- ww$line$end[i:nline] + (len1-len0)
    ww$line$length[i] <- len1
  }
  ww
}




#############################

find.antarctica <- function(ww){
  i <- which(ww$y < -85)[1]
  which(ww$line$begin <= i & ww$line$end >= i)
}

###############################

