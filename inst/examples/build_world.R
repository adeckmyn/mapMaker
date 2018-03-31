library(mapMaker)
infile <- "~/code/NaturalEarth/v3.1.0/data/ne_50m_admin_0_countries"
infile.lakes <- "~/code/NaturalEarth/v3.1.0/data/ne_50m_admin_0_countries_lakes"
outfile <- "~/code/NaturalEarth/build.world50/w50nl"
outfile2 <- "~/code/NaturalEarth/build.world50/w50nl2"
Sys.setenv("MYMAPS"=paste0(dirname(outfile),"/"))
assign(paste0(basename(outfile),"MapEnv"),"MYMAPS")
assign(paste0(basename(outfile2),"MapEnv"),"MYMAPS")

######################
w1 <- readShapePoly(infile)
ww <- map.make(SpatialPolygons2map(w1))

### Antarctica: remove fake points & don't close polygon
find.antarctica <- function(ww){
  i <- which(ww$y < -85)[1]
  which(ww$line$begin <= i & ww$line$end >= i)
}

p121 <- get.line(ww,121)
p121 <- p121[c(2070:2805,2:1804),]
ww <- line.change(ww,121,p121)

### Recombine split polygons

remove.line.gon <- function(ww,i){
# this  assumes every lines is a polygon 
# or just run gon.remove & line.remove
  ww$x <- c(head(ww$x,ww$line$begin[i]-1),tail(ww$x,-(ww$line$end[i]+1)))
  ww$y <- c(head(ww$y,ww$line$begin[i]-1),tail(ww$y,-(ww$line$end[i]+1)))
  ww$names <- ww$names[-i]
  map.make(ww)
}

w1 <- unique(unlist(lapply(which(ww$x < -179.99),function(i) which.line(ww, i))))
e1 <- unique(unlist(lapply(which(ww$x >  179.99),function(i) which.line(ww, i))))
# print(ww$names[w1])
# print(ww$names[e1])
w1 <- w1[-1]  # not 121=Antarctica
e1 <- e1[-1]

for(l in w1) assign(paste("w",l,sep=""),get.line(ww,l))
for(l in e1) assign(paste("e",l,sep=""),get.line(ww,l))

## MANUAL: combine polygons, remove superfluous polygon from names list & data
## for both Fiji and Russia: most logical to move west part by +360

# Fiji: line/poly 537:557
# (539=540)-(541=542)-(543 = 544)
w540$x <- w540$x + 360
w542$x <- w542$x + 360
w544$x <- w544$x + 360

# plot(rbind(w552,e551))
# points(w552,type="o",col=2)
p539 <- rbind(e539[1:5,],w540[7:11,],e539[1,])
ww <- line.change(ww,539,p539)

p541 <- rbind(e541[1:4,],w542[4:7,],e541[1,])
ww <- line.change(ww,541,p541)

p543 <- rbind(w544[2,],e543[2:56,],w544[c(5:7,2),])
ww <- line.change(ww,543,p543)

# Russia: 1271=1314(main), 1282 = 1280
w1271$x <- w1271$x + 360
w1282$x <- w1282$x + 360

p1280 <- rbind(e1280[1:11,],w1282[c(23:31,2:20),],e1280[14:17,]) 
ww <- line.change(ww,1280,p1280)

p1314 <- rbind(e1314[1:633,],w1271[c(337:338,2:321),],e1314[636:4574,])
ww <- line.change(ww,1314,p1314)

# now remove superfluous polygons
for (i in rev(sort(w1))) ww <- remove.line.gon(ww,i)


### FIX BUGS in w50 data:
# (tracked by looking for line$length==2)
# some of these may not be actual bugs
# The China/India border is very contested
# but for 'maps' I need to have lines that fit exactly.
# I don't want 'no-one's land' because it screws up internal=FALSE plots.`

# BE/NL border bug
g1=187 #BE
g2=1061 #NL
p1 <- get.line(ww,g1)
p2 <- get.line(ww,g2)
p1[29:30,] <- p2[119:118,]
p2[120:118,] <- p1[29:31,]

ww <- line.change(ww, g1, p1)
ww <- line.change(ww, g2, p2)

# AF/IR : Iran has 1 extra point in boundary (remove, or add to Afghanistan?)
g1 <- 847 #IQ
g2 <- 2 #AF
p1 <- get.line(ww, g1)
p2 <- get.line(ww, g2)
p1 <- p1[-235,]
ww <- line.change(ww, g1, p1)

# China/India : many errors
# maybe easier to correct by pasting the whole border from one to the other
g1 <- 430 # CH
p1 <- get.line(ww,g1)
g2 <- 839 # IN
p2 <- get.line(ww,g2)
p2 <- rbind(p1[1692:1574,],p2[51:1287,])
ww <- line.change(ww, g2, p2)

# Indonesia:89/Papua New G:
g1 <- 780
g2 <- 1195
p1 <- get.line(ww,g1)
p2 <- get.line(ww,g2)
p2 <- p2[-(278:279),]
ww <- line.change(ww,g2, p2)

# create line database
ww0 <- ww
ww <- map.gon2line(ww0,precision=1E-8)
ww <- map.LR(ww)
ww1 <- ww


### split meridian crossings (necessary for world2)

# insert break points at 0
i <- which(head(ww$x,-1) * tail(ww$x,-1) < 0)
xi <- rep(0,length(i))
yi <- ww$y[i] + (ww$y[(i+1)]-ww$y[i])/(ww$x[(i+1)]-ww$x[i]) * (xi-ww$x[i])

ww$x <- insert.points(ww$x, i+1, 0)
ww$y <- insert.points(ww$y, i+1, yi)
ww$line <- line.parse(ww)
# split lines at 0
splits <- which(ww$x==0)
for (i in rev(sort(splits))) ww <- line.split(ww, i)

### A few additions:
# NL: Ijsselmeer & Zuid-Flevoland
wl1 <- readShapePoly(infile.lakes)
wlakes <- SpatialPolygons2map(wl1)
#wlakes <- map.make(read.worldmap(infile.lakes))
ijssel <- get.line(wlakes,1078)
flevo <- get.line(wlakes,1074)
ww <- line.append(ww,ijssel)
nl <- dim(ww$line)[1]
ww <- gon.append(ww,nl,"Netherlands:IJsselmeer")
ww <- line.append(ww,flevo)
nl <- dim(ww$line)[1]
ww <- gon.append(ww,nl,"Netherlands:Zuid-Flevoland")

# Some very small 'enclaves' don't have an associated 'hole'
# as a result, e.g. Vatican appears even with 'interior=FALSE'
# Solution: have the 'Vatican' polgon twice (in reverse order)
# but we don't cal this 'italy:hole' or anything, just 'Vatican'
clist <- c("Vatican")
for (cc in clist) {
  ll <- get.gon(ww,which(ww$names==cc))
  ww <- gon.append(ww,-ll,cc)
}

ww <- map.LR(ww)
map.export.bin(ww, outfile)
map.export.ascii(ww, outfile, ndec=8)

##############
### world2 ###
##############
ww2 <- ww

zzz=which(ww$x==0)
zerolines=vapply(zzz,function(x) which.line(ww,x),1)
zerogons1 <- vapply(zerolines, function(x) which.gon(ww,x),1)
zerogons2 <- vapply(zerolines, function(x) which.gon(ww,-x),1)
zerogons=unique(c(zerogons1,zerogons2))

# print(ww$names[zerogons])
# Antarctica : reverse order of lines and add a closure (?)
# the "closure line" is best added manually at the end
pgon <- 121
ant0 <- get.gon(ww,pgon)
ww <- gon.change(ww, pgon, rev(ant0))

# Burkina Faso
pgon <- 189
bf <- get.gon(ww,pgon)
bf.w <- bf[c(8,1:3)]
bf.e <- bf[4:7]
bf.w.close <- rbind(tail(get.line(ww,bf[3]),1),head(get.line(ww,bf[8]),1))
bf.e.close <- rbind(tail(get.line(ww,bf[7]),1),head(get.line(ww,bf[4]),1))
bf.e.close$x[] <- 360

ww <- line.append(ww, bf.e.close)
ww <- gon.change(ww, pgon, c(bf.e, dim(ww$line)[1]))

ww <- line.append(ww, bf.w.close)
ww <- gon.append(ww, c(bf.w, dim(ww$line)[1]), name=paste0(ww$names[pgon],":west"))

# Algeria
pgon <- 486
al <- get.gon(ww,pgon)
al.w <- al[c(10,1:4)]
al.e <- al[5:9]
al.w.close <- rbind(tail(get.line(ww,al[4]),1),head(get.line(ww,al[10]),1))
al.e.close <- rbind(tail(get.line(ww,al[9]),1),head(get.line(ww,al[5]),1))
al.e.close$x[] <- 360

ww <- line.append(ww, al.e.close)
ww <- gon.change(ww, pgon, c(al.e, dim(ww$line)[1]))

ww <- line.append(ww, al.w.close)
ww <- gon.append(ww, c(al.w, dim(ww$line)[1]), name=paste0(ww$names[pgon],":west"))

# Spain
pgon <- 511
gon0 <- get.gon(ww,pgon)
# plot(get.fullgon(ww,pgon),type='l')
# lines(c(0,0),c(-60,60))
gon.w1 <- gon0[2:5]
gon.w2 <- gon0[7]
gon.e <- gon0[5:9]
gon.w1.close <- rbind(tail(get.line(ww,gon0[5]),1),head(get.line(ww,gon0[2]),1))
gon.w2.close <- rbind(tail(get.line(ww,gon0[7]),1),head(get.line(ww,gon0[7]),1))

gon.e.close1 <- rbind(tail(get.line(ww,gon0[1]),1),head(get.line(ww,gon0[6]),1))
gon.e.close2 <- rbind(tail(get.line(ww,gon0[6]),1),head(get.line(ww,gon0[8]),1))
gon.e.close1$x[] <- 360
gon.e.close2$x[] <- 360

ww <- line.append(ww, gon.e.close1)
ww <- line.append(ww, gon.e.close2)
nl <- dim(ww$line)[1]
ww <- gon.change(ww, pgon, c(gon0[1],nl-1,gon0[6],nl,gon0[8:10]))

ww <- line.append(ww, gon.w1.close)
ww <- gon.append(ww, c(gon.w1, dim(ww$line)[1]), name=paste0(ww$names[pgon],":west1"))

ww <- line.append(ww, gon.w2.close)
ww <- gon.append(ww, c(gon.w2, dim(ww$line)[1]), name=paste0(ww$names[pgon],":west2"))

# France
pgon <- 558
gon0 <- get.gon(ww,pgon)
# plot(get.fullgon(ww,pgon),type='l')
# lines(c(0,0),c(-60,60))
gon.w1 <- gon0[c(13:14,1:10)]
gon.e <- gon0[11:12]
gon.w1.close <- rbind(tail(get.line(ww,gon0[10]),1),head(get.line(ww,gon0[13]),1))

gon.e.close <- rbind(tail(get.line(ww,gon0[12]),1),head(get.line(ww,gon0[11]),1))
gon.e.close$x[] <- 360

ww <- line.append(ww, gon.e.close)
nl <- dim(ww$line)[1]
ww <- gon.change(ww, pgon, c(gon.e,nl))

ww <- line.append(ww, gon.w1.close)
nl <- dim(ww$line)[1]
ww <- gon.append(ww, c(gon.w1, nl), name=paste0(ww$names[pgon],":west"))

## UK
pgon <- 584
gon0 <- get.gon(ww,pgon)
# plot(get.fullgon(ww,pgon),type='l')
# lines(c(0,0),c(-60,60))
gon.w1 <- gon0[2]
gon.w2 <- gon0[4]

gon.w1.close <- rbind(tail(get.line(ww,gon0[2]),1),head(get.line(ww,gon0[2]),1))
gon.w2.close <- rbind(tail(get.line(ww,gon0[4]),1),head(get.line(ww,gon0[4]),1))

gon.e.close1 <- rbind(tail(get.line(ww,gon0[1]),1),head(get.line(ww,gon0[3]),1))
gon.e.close1$x[] <- 360
gon.e.close2 <- rbind(tail(get.line(ww,gon0[3]),1),head(get.line(ww,gon0[5]),1))
gon.e.close2$x[] <- 360

ww <- line.append(ww, gon.e.close1)
ww <- line.append(ww, gon.e.close2)
nl <- dim(ww$line)[1]
ww <- gon.change(ww, pgon, c(gon0[1],nl-1,gon0[3],nl,gon0[5]))

ww <- line.append(ww, gon.w1.close)
nl <- dim(ww$line)[1]
ww <- gon.append(ww, c(gon.w1, nl), name=paste0(ww$names[pgon],":west1"))
ww <- line.append(ww, gon.w2.close)
nl <- dim(ww$line)[1]
ww <- gon.append(ww, c(gon.w2, nl), name=paste0(ww$names[pgon],":west2"))

## Ghana
pgon <- 595
gon0 <- get.gon(ww,pgon)
# plot(get.fullgon(ww,pgon),type='l')
# lines(c(0,0),c(-60,60))
gon.w1 <- gon0[2]
gon.w2 <- gon0[4:5]

gon.w1.close <- rbind(tail(get.line(ww,gon0[2]),1),head(get.line(ww,gon0[2]),1))
gon.w2.close <- rbind(tail(get.line(ww,gon0[5]),1),head(get.line(ww,gon0[4]),1))

gon.e.close1 <- rbind(tail(get.line(ww,gon0[1]),1),head(get.line(ww,gon0[3]),1))
gon.e.close1$x[] <- 360
gon.e.close2 <- rbind(tail(get.line(ww,gon0[3]),1),head(get.line(ww,gon0[6]),1))
gon.e.close2$x[] <- 360

ww <- line.append(ww, gon.e.close1)
ww <- line.append(ww, gon.e.close2)
nl <- dim(ww$line)[1]
ww <- gon.change(ww, pgon, c(gon0[1],nl-1,gon0[3],nl,gon0[6:10]))

ww <- line.append(ww, gon.w1.close)
nl <- dim(ww$line)[1]
ww <- gon.append(ww, c(gon.w1, nl), name=paste0(ww$names[pgon],":west1"))
ww <- line.append(ww, gon.w2.close)
nl <- dim(ww$line)[1]
ww <- gon.append(ww, c(gon.w2, nl), name=paste0(ww$names[pgon],":west2"))

## Togo
pgon <- 1403
gon0 <- get.gon(ww,pgon)
# plot(get.fullgon(ww,pgon),type='l')
# lines(c(0,0),c(-60,60))
gon.e1 <- gon0[4]
gon.e2 <- gon0[6:7]

gon.e1.close <- rbind(tail(get.line(ww,gon0[4]),1),head(get.line(ww,gon0[4]),1))
gon.e2.close <- rbind(tail(get.line(ww,gon0[7]),1),head(get.line(ww,gon0[6]),1))
gon.e1.close$x[] <- 360
gon.e2.close$x[] <- 360

gon.w.close1 <- rbind(tail(get.line(ww,gon0[3]),1),head(get.line(ww,gon0[5]),1))
gon.w.close2 <- rbind(tail(get.line(ww,gon0[5]),1),head(get.line(ww,gon0[8]),1))

ww <- line.append(ww, gon.w.close1)
ww <- line.append(ww, gon.w.close2)
nl <- dim(ww$line)[1]
ww <- gon.change(ww, pgon, c(gon0[1:3],nl-1,gon0[5],nl,gon0[8]))

ww <- line.append(ww, gon.e1.close)
nl <- dim(ww$line)[1]
ww <- gon.append(ww, c(gon.e1, nl), name=paste0(ww$names[pgon],":east1"))
ww <- line.append(ww, gon.e2.close)
nl <- dim(ww$line)[1]
ww <- gon.append(ww, c(gon.e2, nl), name=paste0(ww$names[pgon],":east2"))

## Mali
pgon <- 992
gon0 <- get.gon(ww,pgon)
# plot(get.fullgon(ww,pgon),type='l')
# lines(c(0,0),c(-60,60))
gon.w1 <- gon0[c(9,1:2)]
gon.e <- gon0[3:8]
gon.w1.close <- rbind(tail(get.line(ww,gon0[2]),1),head(get.line(ww,gon0[9]),1))
gon.e.close <- rbind(tail(get.line(ww,gon0[8]),1),head(get.line(ww,gon0[3]),1))
gon.e.close$x[] <- 360

ww <- line.append(ww, gon.e.close)
nl <- dim(ww$line)[1]
ww <- gon.change(ww, pgon, c(gon.e,nl))

ww <- line.append(ww, gon.w1.close)
nl <- dim(ww$line)[1]
ww <- gon.append(ww, c(gon.w1, nl), name=paste0(ww$names[pgon],":west"))

g1=150
g2=151
ant.closure <- data.frame(x = c(360,seq(360,0,length=10),0),
                          y=c(tail(get.line(ww,g1)$y,1),rep(-89.99,10),get.line(ww,g2)$y[1]))
ww <- line.append(ww, ant.closure)
nl <- dim(ww$line)[1]
ww <- gon.change(ww,121, c(get.gon(ww,121),nl))

# a last little cleaning (error is 1.E-13, so it should dissappear anyway in output)
ww$x[ww$line$end[g2]] <- 180

ww <- map.LR(ww)
map.export.bin(ww, outfile2)
map.export.ascii(ww, outfile2, ndec=8)

