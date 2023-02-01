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

# Create data ------------------------------------------------------------------

startZ <- 1.1*exp(2i * pi * seq(0,1,l=10000))

set.seed(21358)
poles <- runif(3,-1,1) + 1i*runif(3,-1,1)
zeros <- runif(5,-1,1) + 1i*runif(5,-1,1)

pos <- makeStreams(startZ,m=1000,FUN=flowFunction,d=0.015)

# Build plot and save to file
png(
  here::here("img/20230106.png"),
  width = 10, height = 10, units = "in", res = 600)

par(
  bg = "#162227", mar=c(0,0,0,0))
scattermoreplot(
  pos,size=c(1700,1700), xlim=c(-2,2), ylim=c(-2,2), asp=1, axes = FALSE,
  xlab = NA, ylab = NA, col = "#D79233")

dev.off()