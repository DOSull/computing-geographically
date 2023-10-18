library(plotly)
library(plot3D)

setwd("~/Documents/code/computing-geographically/__scratch")

test1 <- read.csv("tg-day-in-the-life.csv")
locations_0 <- unique(select(test1, x, y)) %>%
  mutate(id = 1:5, t = 0)
locations_24 <- locations_0 %>%
  mutate(t = 24)

## Fig 7.2

# svg("tg-day-in-the-life.svg")

lines3D(test1$x, test1$y, test1$t, 
        xlim = c(min(test1$x) - 2, max(test1$x) + 2),
        ylim = c(min(test1$y) - 2, max(test1$y) + 2),
        xlab = "x", ylab = "y", zlab = "Time, t",
        col = "#00000080", lwd = 2, alpha = 0.5, 
        phi = 25, theta = 60, bty = "g", scale = F, expand = .5)
points3D(locations_0$x, locations_0$y, locations_0$t, add = T, col = "black")
arrows3D(locations_0$x, locations_0$y, locations_0$t, 
           locations_24$x, locations_24$y, locations_24$t, 
           add = T, lty = "dashed", lwd = 0.5, col = "black")
# dev.off()


test2 <- read.csv("meeting.csv")


## Fig 7.3

svg("tg-meeting.svg")
df <- filter(test2, id == 1)
lines3D(df$x, df$y, df$t, 
        xlim = c(-6, 6), ylim = c(-6, 6),
        xlab = "x", ylab = "y", zlab = "Time, t",
        col = "black", lwd = .5, 
        phi = 25, theta = 60, bty = "g", scale = F, expand = .75)
for (i in 2:8) {
  df <- filter(test2, id == i)
  lines3D(df$x, df$y, df$t, add = T,
          xlim = c(-6, 6), ylim = c(-6, 6),
          col = "black", lwd = .5)
}
dev.off()






#####################
#########################
#####################






plot_ly(group_by(test2, id), 
        x = ~x, y = ~y, z = ~t, opacity = 1, line = list(width = 5),
        type = "scatter3d", mode = "lines")




library(rgl)
open3d()
par3d(windowRect = c(2000, 0, 3000, 800))
plot3d(test1$x, test1$y, test1$t, type = "l",
       xlim = c(min(test1$x) - 2, max(test1$x) + 2),
       ylim = c(min(test1$y) - 2, max(test1$y) + 2),
       col = "red",
       xlab = "x", ylab = "y", zlab = "Time, t")
grid3d(c("z"))

M <- par3d("userMatrix")
M2 <- rotate3d(M, angle = pi/8, x = 0, y = 0, z = 1)
M3 <- scale3d(M2, 1, 1, 0.8)
view3d(userMatrix = M3)
#rgl.postscript("tg-a-day-in-the-life.svg", fmt = "svg")
rgl.close()


open3d()
par3d(windowRect = c(2000, 0, 3000, 800))
df <- filter(test2, id == 1)
plot3d(df$x, df$y, df$t, type = "l",
       xlim = c(-6, 6), ylim = c(-6, 6),
       col = "red",
       xlab = "x", ylab = "y", zlab = "Time, t")
for (i in 2:8) {
  df <- filter(test2, id == i)
  lines3d(df$x, df$y, df$t,
          col = "red")
}
grid3d(c("z"))

M <- par3d("userMatrix")
M2 <- rotate3d(M, angle = pi/8, x = 0, y = 0, z = 1)
M3 <- scale3d(M2, 1, 1, 0.8)
view3d(userMatrix = M3)
# rgl.postscript("tg-meeting.svg", fmt = "svg")
rgl.close()



# Another option but not a great one!
library(gg3D)
ggplot(test2, aes(x = x, y = y, z = t, group = id, color = id)) +
  scale_color_distiller(palette = "Set1") +
  theme_void() + 
  axes_3D(theta = 220, phi = 20) +
  stat_3D(theta = 220, phi = 20, geom = "path") +
  theme(legend.position = "none")
