.libPaths("C:\\Users\\TempAdmin2\\Documents\\R\\win-library\\3.2" )
library(networkD3)
library(ggplot2)
library(igraph)
library(visNetwork)

setwd("C:/Users/TempAdmin2/Documents/Data Science/R Scripts/IDVA/Hubway Dataset")
nodeDF <- read.csv('hubwayNodes.csv')
edgeDF <- read.csv('hubwayEdges.csv')

#length(which(edgeDF$gender == 'Female'))
tdf <- data.frame(table(edgeDF$Source))
png("hist.png", width = 8, height = 8, units = 'in', res=300)
hist(tdf$Freq, main = 'Histogram of Source Stations', xlab = 'No. of Stations', ylab = 'Used as Source')
dev.off()
sliceEDF <- edgeDF[(edgeDF$Source %in% c(tdf$Var1[which(tdf$Freq < 40)]) & edgeDF$subscription_type == 'Casual'),]
sliceNDF <- nodeDF[which(nodeDF$Id %in% c(tdf$Var1[which(tdf$Freq < 40)])),]

comboDF <- merge(sliceEDF[,c(1,2)], sliceNDF[,c(1,2)], by.x = 'Source', by.y = 'Id')
colnames(comboDF) <- c('Source', 'Target', 'SourceName')
comboDF <- merge(comboDF[,c(1,2,3)], nodeDF[,c(1,2)], by.x = 'Target', by.y = 'Id')
colnames(comboDF) <- c('Source', 'Target', 'SourceName', 'TargetName')


n <- simpleNetwork(comboDF[,c(3,4)])
n
saveNetwork(n, file = "Hubway.html")

#install.packages('visNetwork')
# library(visNetwork)
# nodes <- data.frame(comboDF$SourceName)
# edge <- data.frame(comboDF[,c(3,4)])
# visNetwork(nodes, edge, width="100%", height="400px", main="Network!")

net <- graph.data.frame(sliceEDF, nodeDF[nodeDF$Id %in% sliceEDF$Target | nodeDF$Id %in% sliceEDF$Source,],  directed=T)

#net <- simplify(net, remove.multiple = F, remove.loops = T)

plot(net)

deg <- degree(net, mode="all")
E(net)$size <- deg
V(net)$size <- 10
E(net)$arrow.size <- 0.08
E(net)$edge.color <- "gray80"
plot(net)

png("mygraph.png", width = 8, height = 8, units = 'in', res=300)
plot(net, vertex.shape="none", vertex.label=V(net),
     vertex.label.font=1, vertex.label.color="#005f0b",
     vertex.label.cex=.6, edge.color="#6c6877", 
     main = 'Network graph of 3 stations with least sources')
dev.off()

png("mygraph1.png", width = 8, height = 8, units = 'in', res=300)
l <- layout.grid.3d(net)
plot(net, layout=l, main = 'Network graph of 3 stations with least sources')
dev.off()

png("mygraph2.png", width = 8, height = 8, units = 'in', res=300)
l <- layout.kamada.kawai(net)
l <- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
plot(net, rescale=F, layout=l, main = 'Network graph of 3 stations with least sources', 
     vertex.label=V(net), vertex.label.cex=.6, vertex.label.color="#005f0b")
dev.off()
