.libPaths("C:\\Users\\TempAdmin2\\Documents\\R\\win-library\\3.2" )
library(networkD3)
library(ggplot2)
library(igraph)
library(visNetwork)
library(ggmap)
library(mapproj)
library(lubridate)
library(reshape2)
library(chorddiag)

setwd("C:/Users/TempAdmin2/Documents/Data Science/R Scripts/IDVA/Hubway Dataset")
nodeDF <- read.csv('hubwayNodes.csv')
edgeDF <- read.csv('hubwayEdges.csv')

map <- get_map(location = 'Boston', zoom = 13)
ggmap(map)


hist(edgeDF$birth_date[which(!is.na(edgeDF$birth_date))])

edgeDF$AgeCat <- rep(NA, length(edgeDF$Source))
edgeDF$AgeCat[which(edgeDF$birth_date < 1965)] <- 1
edgeDF$AgeCat[which(edgeDF$birth_date > 1965 & edgeDF$birth_date < 1980)] <- 2
edgeDF$AgeCat[which(edgeDF$birth_date > 1980)] <- 3
demo <- dcast(data.frame(table(edgeDF$Source, edgeDF$AgeCat)), Var1 ~ Var2)
temp <- data.frame(c(demo$`1`/sum(demo$`1`)), c(demo$`2`/sum(demo$`2`)), c(demo$`3`/sum(demo$`3`)))
temp2 <- data.frame(t(apply(temp, 1, function(x)(x-min(x))/(max(x)-min(x)))))
colnames(temp2) <- c('BabyBoomers', 'GenX', 'GenY')
temp2[temp2 < 1] <- 0
temp3 <- data.frame(factor(names(temp2)[max.col(temp2)], ordered = TRUE))
colnames(temp3) <- 'AgeDemo'
nodeDF <- cbind(nodeDF, temp3)
write.csv(file='cartoplot.csv', x=nodeDF[, c(1,2,5,6,10)])

####Temporal Viz
edgeDF$end_date <- as.POSIXlt(edgeDF$end_date, format="%Y-%m-%d %H:%M:%S")
temporal <- cbind.data.frame(hour(edgeDF$start_date), edgeDF$subscription_type)
colnames(temporal) <- c('Hours', 'Type')
tempDF <- data.frame(xtabs(~ Hours + Type, temporal))
g <- ggplot(data=tempDF, aes(x=as.integer(tempDF$Hours), y=tempDF$Freq, fill = as.factor(Type)))
g + geom_area(position = 'stack') + scale_fill_manual(values=c("#00a550", "#70439a"))
g <- g + theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.border=element_blank(),
          panel.background = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA))  
  
ggsave("tempPlot.eps", g)

# m <- matrix(c(11975,  5871, 8916, 2868,
#               1951, 10048, 2060, 6171,
#               8010, 16145, 8090, 8045,
#               1013,   990,  940, 6907),
#             byrow = TRUE,
#             nrow = 4, ncol = 4)
# haircolors <- c("black", "blonde", "brown", "red")
# dimnames(m) <- list(have = haircolors,
#                     prefer = haircolors)
# 
# 
# groupColors <- c("#000000", "#FFDD89", "#957244", "#F26223")
# chorddiag(m, groupColors = groupColors, groupnamePadding = 20)


riverCross <- c(87, 97, 70, 67, 72, 84, 90, 98 ,  18, 17, 10, 53, 60, 38)
dim(nodeDF[which(nodeDF$Id %in% riverCross),c(1,2,6)])

chordEdge <- edgeDF[which(edgeDF$Source %in% riverCross), c(1,2,6)] 
chordEdge <- chordEdge[which(chordEdge$Target %in% riverCross),]

chordEdge <- chordEdge[(chordEdge$Source != chordEdge$Target),]

chordMat <- dcast(chordEdge, Source ~ Target)
chordMat[is.na(chordMat)] <- 0
chordMatrix <- as.matrix(chordMat[ ,-1])


sourcen <- as.character(chordMat$Source)
distn <- as.character(colnames(chordMatrix))

#which(!(sourcen %in% distn))
#tempm <- as.matrix(tempm[c(-3), ])


dimnames(chordMatrix) <- list(have = sourcen,
                        prefer = distn)

groupColors <- c("#4d86bb", "#4d86bb","#4d86bb", "#4d86bb","#4d86bb","#4d86bb", "#ef4026","#ef4026","#ef4026","#ef4026","#ef4026","#ef4026","#ef4026")
g <- chorddiag(chordMatrix, groupColors = groupColors, groupnamePadding = 20, 
          showGroupnames = FALSE, showTicks = FALSE) +

g + theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.border=element_blank(),
          panel.background = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA))  

ggsave("chordplot.eps", g)
