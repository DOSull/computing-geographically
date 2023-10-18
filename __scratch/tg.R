library(plotly)

setwd('~/Documents/ComputingGeographically/cg-working-copy/examples/tg')

## Fig 7.5

flockers <- read.table('flockers.txt', sep=' ', header=T)
plot_ly(group_by(flockers, who), x=~x, y=~y, z=~t/10, color=~who, 
        type='scatter3d', mode='lines', line=list(width=2), opacity=.35)


## Fig 7.4

library(dplyr)
library(plotly)
library(timeDate)

taxis <- read.table('release/taxi_log_2008_by_id/taxis100.txt', sep=',') %>%
  rename(id=V1, t=V2, lon=V3, lat=V4)

lon.mean <- mean(taxis$lon)
lat.mean <- mean(taxis$lat)

taxis.bj <- taxis %>%
  mutate(time = as.double(timeDate(t))) %>%
  filter(#id == 11,
         abs(lon - lon.mean) < .25,
         abs(lat - lat.mean) < .25) #%>%
#  filter(dayOfWeek(timeDate(t)) == 'Sun')

plot_ly(group_by(taxis.bj, id),
        x=~lon, y=~lat, z=~time/10,
        color=~id, opacity=.35, line=list(width=1),
        type='scatter3d', mode='lines')




library(plot3D)

test1 <- read.csv('tg-day-in-the-life.csv')
locations.0 <- unique(select(test1, x, y)) %>%
  mutate(id=1:5, t=0)  
locations.24 <- locations.0 %>%
  mutate(t=24)

## Fig 7.2

svg('tg-day-in-the-life.svg')

lines3D(test1$x, test1$y, test1$t, 
        xlim=c(min(test1$x) - 2, max(test1$x) + 2),
        ylim=c(min(test1$y) - 2, max(test1$y) + 2),
        xlab='x', ylab='y', zlab='Time, t',
        col='#00000080', lwd=2, alpha=0.5, 
        phi=25, theta=60, bty='g', scale=F, expand=.5)
points3D(locations.0$x, locations.0$y, locations.0$t, add=T, col='black')
arrows3D(locations.0$x, locations.0$y, locations.0$t, 
           locations.24$x, locations.24$y, locations.24$t, 
           add=T, lty='dashed', lwd=0.5, col='black')
dev.off()


test2 <- read.csv('meeting.csv')


## Fig 7.3

svg('tg-meeting.svg')
df <- filter(test2, id==1)
lines3D(df$x, df$y, df$t, 
        xlim=c(-6, 6), ylim=c(-6, 6),
        xlab='x', ylab='y', zlab='Time, t',
        col='black', lwd=.5, 
        phi=25, theta=60, bty='g', scale=F, expand=.75)
for (i in 2:8) {
  df <- filter(test2, id==i)
  lines3D(df$x, df$y, df$t, add=T,
          xlim=c(-6, 6), ylim=c(-6, 6),
          col='black', lwd=.5)
}
dev.off()






#####################
#########################
#####################






plot_ly(group_by(test2, id), 
        x=~x, y=~y, z=~t, opacity=1, line=list(width=5),
        type='scatter3d', mode='lines')




library(rgl)
open3d()
par3d(windowRect=c(2000, 0, 3000, 800))
plot3d(test1$x, test1$y, test1$t, type='l',
       xlim=c(min(test1$x) - 2, max(test1$x) + 2),
       ylim=c(min(test1$y) - 2, max(test1$y) + 2),
       col='red',
       xlab='x', ylab='y', zlab='Time, t')
grid3d(c("z"))

M <- par3d("userMatrix")
M2 <- rotate3d(M, angle=pi/8, x=0, y=0, z=1)
M3 <- scale3d(M2, 1, 1, 0.8)
view3d(userMatrix=M3)
#rgl.postscript('tg-a-day-in-the-life.svg', fmt='svg')
rgl.close()


open3d()
par3d(windowRect=c(2000, 0, 3000, 800))
df <- filter(test2, id==1)
plot3d(df$x, df$y, df$t, type='l',
       xlim=c(-6, 6), ylim=c(-6, 6),
       col='red',
       xlab='x', ylab='y', zlab='Time, t')
for (i in 2:8) {
  df <- filter(test2, id==i)
  lines3d(df$x, df$y, df$t,
          col='red')
}
grid3d(c("z"))

M <- par3d("userMatrix")
M2 <- rotate3d(M, angle=pi/8, x=0, y=0, z=1)
M3 <- scale3d(M2, 1, 1, 0.8)
view3d(userMatrix=M3)
# rgl.postscript('tg-meeting.svg', fmt='svg')
rgl.close()



# Another option but not a great one!
library(gg3D)
ggplot(test2, aes(x=x, y=y, z=t, group=id, color=id)) +
  scale_color_distiller(palette='Set1') +
  theme_void() + 
  axes_3D(theta=220, phi=20) +
  stat_3D(theta=220, phi=20, geom="path") +
  theme(legend.position = "none")
