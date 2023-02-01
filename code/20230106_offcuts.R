# Load libraries ---------------------------------------------------------------

library(scattermore)

# Define custom functions ------------------------------------------------------

# Courtesy of George Savva:
# https://georgemsavva.github.io/creativecoding/posts/flowfields/

flowFunction <- function(z) {
  fz = z-zeros[1]
  for(i in zeros[-1]) fz = fz * (z-i)
  for(i in poles) fz = fz / (z-i)
  fz/Mod(fz)
}

makeStreams <- function(startpos,m=100,FUN,d=0.1){
  N=length(startpos)
  pos=matrix(nrow=N, ncol=m)
  pos2=matrix(nrow=N, ncol=m)
  pos[,1] <- startpos
  pos2[,m] <- startpos
  for(i in 2:m){
    v <- FUN(pos[,i-1])
    pos[,i] <- pos[,i-1] + runif(N,0,2)*d * v/Mod(v)
  }
  for(i in (m-1):1){
    v <- FUN(pos2[,i+1])
    pos2[,i] <- pos2[,i+1] - runif(N,0,2)*d* v/Mod(v)
  }
  pos <-cbind(pos,pos2) |> as.vector()
  cbind(Re(pos),Im(pos))
}

# Offcut 1 ---------------------------------------------------------------------

startZ <- 1.3*exp(2i * pi * seq(0,1,l=4000))

set.seed(21358)
poles <- runif(5,-1,1) + 1i*runif(5,-1,1)
zeros <- runif(10,-1,1) + 1i*runif(10,-1,1)

pos <- makeStreams(startZ,m=1000,FUN=flowFunction,d=0.004)

png(
  here::here("img/offcuts/20230106_offcut01.png"),
  width = 6, height = 6, units = "in", res = 600)

par(mar=c(0,0,0,0))
scattermoreplot(
  pos,size=c(2000,2000), xlim=c(-2,2), ylim=c(-2,2), asp=1, axes = FALSE,
  xlab = NA, ylab = NA, col=hsv(0,0,0))

dev.off()

# Offcut 2 ---------------------------------------------------------------------

startZ <- 0.7*exp(2i * pi * seq(0,1,l=4000))

set.seed(13582)
poles <- runif(5,-1,1) + 1i*runif(5,-1,1)
zeros <- runif(10,-1,1) + 1i*runif(10,-1,1)

pos <- makeStreams(startZ,m=2000,FUN=flowFunction,d=0.008)

png(
  here::here("img/offcuts/20230106_offcut02.png"),
  width = 6, height = 6, units = "in", res = 600)

par(mar=c(0,0,0,0))
scattermoreplot(
  pos,size=c(2000,2000), xlim=c(-1,1), ylim=c(-1,1), asp=1, axes = FALSE,
  xlab = NA, ylab = NA, col=hsv(0,0,0))

dev.off()

# Offcut 3 ---------------------------------------------------------------------

startZ <- 0.6*exp(2i * pi * seq(0,1,l=300))

set.seed(35821)
poles <- runif(5,-1,1) + 1i*runif(5,-1,1)
zeros <- runif(10,-1,1) + 1i*runif(10,-1,1)

pos <- makeStreams(startZ,m=1000,FUN=flowFunction,d=0.002)

png(
  here::here("img/offcuts/20230106_offcut03.png"),
  width = 6, height = 6, units = "in", res = 600)

par(mar=c(0,0,0,0))
scattermoreplot(
  pos,size=c(1000,1000), xlim=c(-1,1), ylim=c(-1,1), asp=1, axes = FALSE,
  xlab = NA, ylab = NA, col=hsv(0,0,0))

dev.off()

# Offcut 4 ---------------------------------------------------------------------

startZ <- 0.2*exp(2i * pi * seq(0,1,l=1000))

set.seed(58213)
poles <- runif(10,-1,1) + 1i*runif(10,-1,1)
zeros <- runif(20,-1,1) + 1i*runif(20,-1,1)

pos <- makeStreams(startZ,m=1000,FUN=flowFunction,d=0.01)

png(
  here::here("img/offcuts/20230106_offcut04.png"),
  width = 6, height = 6, units = "in", res = 600)

par(
  bg = "#131313", mar=c(0,0,0,0))
scattermoreplot(
  pos,size=c(2000,2000), xlim=c(-0.8,0.6), ylim=c(-0.8,0.8), asp=1, axes = FALSE,
  xlab = NA, ylab = NA, col = "#FBF3EF")

dev.off()

# Offcut 5 ---------------------------------------------------------------------

startZ <- 1.7*exp(2i * pi * seq(0,1,l=4000))

set.seed(82135)
poles <- runif(10,-1,1) + 1i*runif(10,-1,1)
zeros <- runif(20,-1,1) + 1i*runif(20,-1,1)

pos <- makeStreams(startZ,m=2000,FUN=flowFunction,d=0.008)

png(
  here::here("img/offcuts/20230106_offcut05.png"),
  width = 6, height = 6, units = "in", res = 600)

par(mar=c(0,0,0,0))
scattermoreplot(
  pos,size=c(2000,2000), xlim=c(-2,2), ylim=c(-2,2), asp=1, axes = FALSE,
  xlab = NA, ylab = NA, col=hsv(0,0,0))

dev.off()

# Offcut 6 ---------------------------------------------------------------------

startZ <- 3*exp(2i * pi * seq(0,1,l=2000))

set.seed(6654)
poles <- runif(10,-1,1) + 1i*runif(10,-1,1)
zeros <- runif(20,-1,1) + 1i*runif(20,-1,1)

pos <- makeStreams(startZ,m=2000,FUN=flowFunction,d=0.008)

png(
  here::here("img/offcuts/20230106_offcut06.png"),
  width = 6, height = 6, units = "in", res = 600)

par(
  bg = "#131313", mar=c(0,0,0,0))
scattermoreplot(
  pos,size=c(2000,2000), xlim=c(-2,2), ylim=c(-2,2), asp=1, axes = FALSE,
  xlab = NA, ylab = NA, col = "#FBF3EF")

dev.off()

# Offcut 7 ---------------------------------------------------------------------

startZ <- 1.2*exp(2i * (pi/2) * seq(0,1,l=2000))

set.seed(3757)
poles <- runif(10,-1,1) + 1i*runif(10,-1,1)
zeros <- runif(20,-1,1) + 1i*runif(20,-1,1)

pos <- makeStreams(startZ,m=2000,FUN=flowFunction,d=0.008)

png(
  here::here("img/offcuts/20230106_offcut07.png"),
  width = 6, height = 6, units = "in", res = 600)

par(
  bg = "#131313", mar=c(0,0,0,0))
scattermoreplot(
  pos,size=c(3000,3000), xlim=c(-2,2), ylim=c(-1,2), asp=1, axes = FALSE,
  xlab = NA, ylab = NA, col = "#FBF3EF")

dev.off()

# Offcut 8 ---------------------------------------------------------------------

startZ <- 2*exp(2i * (pi/2) * seq(0,1,l=3000))

set.seed(3009)
poles <- runif(10,-1,1) + 1i*runif(10,-1,1)
zeros <- runif(20,-1,1) + 1i*runif(20,-1,1)

pos <- makeStreams(startZ,m=2000,FUN=flowFunction,d=0.08)

png(
  here::here("img/offcuts/20230106_offcut08.png"),
  width = 6, height = 6, units = "in", res = 600)

par(
  bg = "#131313", mar=c(0,0,0,0))
scattermoreplot(
  pos,size=c(2000,2000), xlim=c(-2,2), ylim=c(-0.5,2), asp=1, axes = FALSE,
  xlab = NA, ylab = NA, col = "#FBF3EF")

dev.off()