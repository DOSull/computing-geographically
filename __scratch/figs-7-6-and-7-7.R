# library(plotly)
library(dplyr)
# library(plotly)
library(timeDate)
library(gg3D)

# environment must include kaleido and plotly
# reticulate::use_condaenv("cg")

## flocking model aquarium plot

setwd('~/Documents/ComputingGeographically/cg-working-copy/examples/time-geography')
flockers <- read.table('flockers.txt', sep=' ', header=T) %>%
  filter(t > 299)

# p <- plot_ly(group_by(flockers, who), x=~x, y=~y, z=~t/5, color=~who, 
#         type='scatter3d', mode='lines', asp=1)
# save_image(p, "./flocker-3.svg")

ggplot(flockers, 
       aes(x = x, y = y, z = t, group = who, color = who)) +
  # scale_color_viridis_c() +
  scale_color_distiller(palette = "Greys", direction = 1) +
  stat_3D(theta = 135, phi = 35, geom = "path", lwd = 0.5) +
  axes_3D(theta = 135, phi = 35) +
  coord_equal() +
  theme_void() +
  theme(legend.position = "none")

ggsave("flockers.svg")


taxis <- read.table('release/taxi_log_2008_by_id/taxis100.txt', sep=',') %>%
  rename(id=V1, t=V2, lon=V3, lat=V4)

lon.mean <- mean(taxis$lon)
lat.mean <- mean(taxis$lat)

taxis.bj <- taxis %>%
  mutate(time = as.double(timeDate(t))) %>%
  filter(#id == 11,
    abs(lon - lon.mean) < .25,
    abs(lat - lat.mean) < .25)

ggplot(taxis.bj, aes(x = lon, y = lat, z = time, group = id, color = id), 
       alpha = 0.25, lwd = 0.5) +
  # scale_color_viridis_c() +
  scale_color_distiller(palette = "Greys", direction = 1) +
  stat_3D(theta = 30, phi = 5, geom = "path") +
  axes_3D(theta = 30, phi = 5) +
  coord_equal() +
  theme_void() +
  theme(legend.position = "none")

ggsave("taxis.svg")

test1 <- read.csv('test1.csv')
test2 <- read.csv('test2.csv')

plot_ly(group_by(test2, id), 
        x=~x, y=~y, z=~t, opacity=1, line=list(width=5),
        type='scatter3d', mode='lines')


library(gg3D)
ggplot(test2, aes(x=x, y=y, z=t, group=id, color=id)) +
  scale_color_distiller(palette='Set1') +
  theme_void() + 
  axes_3D(theta=220, phi=20) +
  stat_3D(theta=220, phi=20, geom="path") +
  theme(legend.position = "none")

library(plot3D)

lines3D(test1$x, test1$y, test1$t, 
        xlim=c(min(test1$x) - 2, max(test1$x) + 2),
        ylim=c(min(test1$y) - 2, max(test1$y) + 2),
        col='red', phi=25, theta=60)


lines3D(test2[test2$id==1,]$x, test2[test2$id==1,]$y, test2[test2$id==1,]$t, 
        xlim=c(-6, 6), ylim=c(-6, 6),
        col='red', phi=25, theta=60)
for (i in 2:8) {
  lines3D(test2[test2$id==i,]$x, test2[test2$id==i,]$y, test2[test2$id==i,]$t, add=T, 
          xlim=c(-6, 6), ylim=c(-6, 6),
          col='red', phi=25, theta=60)
  }



