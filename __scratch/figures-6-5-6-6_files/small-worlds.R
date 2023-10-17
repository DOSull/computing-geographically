library(igraph)
library(dplyr)
library(scales)
library(ggplot2)
library(reshape2)
library(svglite)
library(RColorBrewer)

setwd("~/Documents/ComputingGeographically/cg-working-copy/examples/trade")

plot.the.graph <- function(G, main='', layout=layout.grid(G), vertex.color='black') {
  plot(G, main=main,
       layout=layout, 
       vertex.label=NA, vertex.color=vertex.color, vertex.size=5, vertex.shape='circle', vertex.lwd=0, 
       edge.color='black', edge.width=0.5)
}

# base.graph <- watts.strogatz.game(2, 20, 2, 0)

base.graph <- make_lattice(length=20, dim=2, nei=2)
ccs <- c(transitivity(base.graph, type='average'))
mpls <- c(mean(distances(base.graph)))
ps <- c(0)

for (i in 1:500) {
  p <- 10 ^ (-6 * runif(1))
  the.graph <- rewire(base.graph, each_edge(p))
  if (components(the.graph)$no == 1) {
    ccs <- c(ccs, transitivity(the.graph, type='average'))
    mpls <- c(mpls, mean(distances(the.graph)))
    ps <- c(ps, p)
  }
}

df.raw <- data.frame(p=ps, cc=ccs, mpl=mpls) %>% 
   mutate(cc = rescale(cc, to=c(0, 1)), mpl = rescale(mpl, to=c(0, 1))) %>% 
   slice(2:n())

df.melt <- df.raw %>%
  melt(id = 'p')

ggplot(df.melt, aes(x=p, y=value, shape=variable)) + 
  geom_point(cex=.8) + 
  # geom_smooth() + 
  scale_x_log10() +
  scale_shape_manual(values=c(1, 4)) +
  xlab('Probability of rewiring') +
  ylab('Value relative to base lattice')
ggsave('small-worlds-trend.svg')

pdf('small-worlds.pdf', width=7.66, height=2.84)
par(mfrow=c(1,5), mar=rep(1, 4))
plot.the.graph(rewire(base.graph, each_edge(0.001)), main='p = 0.001')
plot.the.graph(rewire(base.graph, each_edge(0.005)), main='p = 0.005')
plot.the.graph(rewire(base.graph, each_edge(0.025)), main='p = 0.025')
plot.the.graph(rewire(base.graph, each_edge(0.125)), main='p = 0.125')
plot.the.graph(rewire(base.graph, each_edge(0.625)), main='p = 0.625')
dev.off()


## 1-D small worlds
the.graph <- make_lattice(50, dim=1, circular = TRUE)
par(mfrow=c(1,4), mar=rep(1,4))
plot.the.graph(the.graph, layout=layout.circle(the.graph))
for (i in 1:3) {
  the.graph <- rewire(the.graph, each_edge(0.1))
  plot.the.graph(the.graph, layout=layout.circle(the.graph))
}

## redrawings
the.graph <- make_lattice(length=20, dim=2, nei=1)
the.graph <- rewire(the.graph, each_edge(1/400))
vertex_attr(the.graph, 'centrality') <- centralization.closeness(the.graph)$res

fine = 500 # this will adjust the resolving power.
# ylord <- brewer.pal(7, 'YlOrRd')
ylord <- brewer.pal(9, 'Greys')
pal = colorRampPalette(ylord)

#this gives you the colors you want for every point
graphCol = pal(fine)[as.numeric(cut(V(the.graph)$centrality, breaks=fine))]

svg('redrawings-bw.svg', width=4.4, height=4.4)
par(mfrow=c(3, 3), mar=rep(1, 4))
plot.the.graph(the.graph, layout=layout_randomly(the.graph), vertex.color=graphCol)
plot.the.graph(the.graph, layout=layout.circle(the.graph), vertex.color=graphCol)
plot.the.graph(the.graph, layout=layout_with_mds(the.graph), vertex.color=graphCol)
plot.the.graph(the.graph, layout=layout_as_tree(the.graph), vertex.color=graphCol)
plot.the.graph(the.graph, vertex.color=graphCol)
plot.the.graph(the.graph, layout=layout_as_tree(the.graph, circular=TRUE), vertex.color=graphCol)
plot.the.graph(the.graph, layout=layout.kamada.kawai(the.graph), vertex.color=graphCol)
plot.the.graph(the.graph, layout=layout.drl(the.graph), vertex.color=graphCol)
plot.the.graph(the.graph, layout=layout.fruchterman.reingold(the.graph), vertex.color=graphCol)
dev.off()
