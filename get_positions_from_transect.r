
t <- read.table("data-raw/transects/transect_1203.txt" , header = T, as.is = T)
tr <- read.table("data-raw/transects/anchors_1203.txt", header = T)

tr$EastEnd      <- c(tr$Easting[2:length(tr$Easting)], 0)
tr$NorthEnd     <- c(tr$Northing[2:length(tr$Northing)], 0)
tr$x            <- tr$Easting - tr$EastEnd
tr$y            <- tr$Northing - tr$NorthEnd
tr$alpha        <- atan(tr$y/tr$x)
tr$distance     <- sqrt(abs(tr$Easting-tr$EastEnd)^2 + abs(tr$Northing-tr$NorthEnd)^2)
tr$cumdist      <- cumsum(tr$distance)
tr$cumdiststart <- c(0,tr$cumdist[1:length(tr$cumdist)-1])

t$x <- 0
t$y <- 0

for (i in 2:dim(t)[1]){
  s <- t$Station[i]
  ind <-  which(s < tr$cumdist &  s > tr$cumdiststart)
  print(ind)
  extend_x = (s-tr$cumdiststart[ind])
  print(extend_x)
  xpos = tr$Easting[ind] - (s-tr$cumdiststart[ind]) * cos(tr$alpha[ind])
  ypos = tr$Northing[ind] - (s-tr$cumdiststart[ind]) * sin(tr$alpha[ind])
  t$x[i] <- xpos
  t$y[i] <- ypos
  #print(paste(xpos,ypos))
}

plot(tr$Easting, tr$Northing, col = "red" , pch = 20, cex = 3)
points(t$x,t$y, type = "p", cex = .1)
write.csv(t,"test.csv")
