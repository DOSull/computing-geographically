library(tidyr)
library(dplyr)
library(plot3D)
library(plotly)
library(geometry)

distancexy <- function(from, x=0, y=0) {
  return (sqrt((from[1] - x)^2 + (from[2] - y)^2))
}

distancet <- function(from, to=c(0, 0, 0)) {
  return (to[3] - from)
}


time.budget <- 80
A <- c(-20, 20)
B <- c(20, -20)

x=-100:100
y=-100:100
t=0:time.budget
domain <- expand.grid(x, y, t)
names(domain) <- c('x', 'y', 't')

prism <- domain %>%
  mutate(dist.A = distancexy(A, x, y),
         dist.B = distancexy(B, x, y)) %>%
  mutate(ta = time.budget - dist.A - dist.B) %>%
  filter(ta > 0, dist.A < t, ta > 0, dist.B < time.budget - t)

hull <- convhulln(select(prism, x, y, t)) - 1

add_layer <- function(plt, level, opacity=0.1){
  prsm <- filter(prism, ta > level)
  hll <- convhulln(select(prsm, x, y, t)) - 1
  return (plt %>% add_mesh(data=prsm, x=~x, y=~y, z=~t,
                           i=hll[, 1], j=hll[, 2], k=hll[, 3],
                           opacity=opacity, facecolor=rep('grey', dim(hll)[1])))
}

p <- plot_ly(prism, x=~x, y=~y, z=~t, i=hull[, 1], j=hull[, 2], k=hull[, 3], 
             opacity=0.05, type='mesh3d', facecolor=rep('grey', dim(hull)[1]))

for (i in seq(22.5, 1.5, -3)) {
  p <- add_layer(p, i, opacity=0.05)
}

p

p <- plot_ly(type='volume', data=prism, x=~x, y=~y, z=~t, value=~ta, isomin=0)

p
