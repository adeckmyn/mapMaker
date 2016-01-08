library(mapMaker)
infile <- "~/code/NaturalEarth/v3.1.0/data/ne_50m_admin_0_countries_lakes"
outfile <- "~/code/NaturalEarth/build.world50/w50"
outfile2 <- "~/code/NaturalEarth/build.world50/w50b"
Sys.setenv("MYMAPS"=paste0(dirname(outfile),"/"))
assign(paste0(basename(outfile),"MapEnv"),"MYMAPS")

######################

ww <- map.make(read.worldmap(infile))

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
# (551=552)-(553=554)-(555 = 556)
w552$x <- w552$x + 360
w554$x <- w554$x + 360
w556$x <- w556$x + 360

# plot(rbind(w552,e551))
# points(w552,type="o",col=2)
p551 <- rbind(e551[1:5,],w552[7:11,],e551[1,])
# check:
# plot(p551,type="o")
# points(e551,type="o",col2)
# points(w552,type="o",col=3)
ww <- line.change(ww,551,p551)

p553 <- rbind(e553[1:4,],w554[4:7,],e553[1,])
ww <- line.change(ww,553,p553)

p555 <- rbind(w556[2,],e555[2:56,],w556[c(5:7,2),])
ww <- line.change(ww,555,p555)

# Russia: 1285=1328(main), 1296 = 1294
w1285$x <- w1285$x + 360
w1296$x <- w1296$x + 360

p1294 <- rbind(e1294[1:11,],w1296[c(23:31,2:20),],e1294[14:17,]) 
ww <- line.change(ww,1294,p1294)

p1328 <- rbind(e1328[1:633,],w1285[c(337:338,2:321),],e1328[636:4599,])
ww <- line.change(ww,1328,p1328)

# now remove superfluous polygons
for (i in rev(sort(w1))) ww <- remove.line.gon(ww,i)


### FIX BUGS in w50 data:
# (tracked by looking for line$length==2)
# some of these may not be actual bugs
# The China/India border is very contested
# but for 'maps' I need to have lines that fit exactly.
# I don't want 'no-one's land' because it screws up internal=FALSE plots.`

# BE/NL border bug (Nl is now line 1074, not 1077 because 3 polygons were removed)
p187 <- get.line(ww,187) #be
p1074 <- get.line(ww,1074)
p187[29:30,] <- p1074[120:119,]
p1074 <- p1074[-121,]
ww <- line.change(ww, 187, p187)
ww <- line.change(ww, 1074, p1074)

# AF/IR : Iran has 1 extra point in boundary (remove, or add to Afghanistan?)
p859 <- get.line(ww, 859)
p859 <- p859[-235,]
ww <- line.change(ww, 859, p859)

# China/India : many errors
# maybe easier to correct by pasting the whole border from one to the other
p442 <- get.line(ww,442)
p851 <- get.line(ww,851)
p851 <- rbind(p442[1694:1576,],p851[51:1287,])
ww <- line.change(ww, 851, p851)

# Indonesia:89/Papua New G
p792 <- get.line(ww,792)
p1209 <- get.line(ww,1209)
p1209 <- p1209[-(278:279),]
ww <- line.change(ww, 1209, p1209)

# create line database
ww0 <- ww
ww <- map.gon2line(ww0,precision=1E-8)
ww1 <- ww

######################
### Just add water ###
######################
### add the "punched out" boundary lakes as polygons
### so the will also be filled when fill=TRUE
### we can define the border lakes from knowing the line numbers
greatlakes <- c(-1922,-467)
erie <- c(-1923,-465)
ontario <- c(-1924,-463)
spednic <- c(-1925,-375)
## TODO: all islands in the great lakes still have border "0": adapt manually at the end
## worldHires doesn't have tis feature, so I won't bother for now

titicaca <- c(-301,-1490,-297,-1489)

## there is something strange with lake Kariba: a little polygon of Zambia on the other shore...
## a glitch when punching out the lakes?
kariba <- c(-2056,-2058,-2050,-2057,-2053)
malawi <- c(-1834,-1381,-1368,-1382)
tanganyika <- c(-1836, -2055, -606, -229)
kivu <- c(-604,-1723)
edward <- c(-601,-1841)
albert <- c(-599,-1842)

victoria <- c(-1832,-1840, -1212 )

constance <- c(-512,-659,-219)

peipus <- c(-1689,-734)

caspian <- c(-224,-1686,-1206,-1807,-1124)
aral <- c(-1203,-1997,-1200,-1999)
sarygamysh <- c(-1809,-1998)

khanka <- c(-1685,-561)

ww$names <- c(ww$names, "Great Lakes:Erie","Great Lakes:Superior,Huron,Michigan",
              "Great Lakes:Ontario","Lake Spednic","Lake Titicaca",
              "Lake Kariba","Lake Malawi","Lake Tanganyika","Lake Kivu",
              "Lake Edward","Lake Albert","Lake Victoria",
              "Lake Constance","Lake Peipus","Caspian Sea","Aral Sea","Lake Sarygamysh",
              "Lake Khanka"
)
ww$gondata <- c(ww$gondata,NA,erie,NA,greatlakes,NA,ontario,NA,spednic,NA,titicaca,NA,
                kariba,NA,malawi,NA,tanganyika,NA,kivu,NA,edward,NA,albert,NA,victoria,
                NA,constance,NA,peipus,NA,caspian,NA,aral,NA,sarygamysh,NA,khanka)

ww$gon <- gon.parse(ww)
ww <- map.LR(ww)



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



# CHECK
#zl <- which(ww$line$length == 2)
#zp <- unlist(lapply(zl,function(i) which.gon(ww,i)))

# other check: look for polygons with more than one sea-border (0)
# or plot without internal boundaries
# or countries that have several common lines: probably there is a bug inbetween
ww <- map.LR(ww)
map.export.bin(ww, outfile)
map.export.ascii(ww, outfile, ndec=8)

##############
### world2 ###
##############
ww1 <- ww

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
pgon <- 498
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
pgon <- 523
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
pgon <- 570
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
pgon <- 596
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
pgon <- 607
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
pgon <- 1417
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
pgon <- 1005
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


ant.closure <- data.frame(x = c(360,seq(360,0,length=10),0),
                          y=c(tail(get.line(ww,152)$y,1),rep(-88,10),get.line(ww,153)$y[1]))
ww <- line.append(ww, ant.closure)
nl <- dim(ww$line)[1]
ww <- gon.change(ww,121, c(get.gon(ww,121),nl))
ww <- change.gon(ww,121, c(get.gon(ww,121),nl))
# a last little cleaning (error is 1.E-13, so it should dissappear anyway in output)
ww$x[ww$line$end[153]] <- 180

ww <- map.LR(ww)
map.export.bin(ww, outfile2)
map.export.ascii(ww, outfile2, ndec=8)


