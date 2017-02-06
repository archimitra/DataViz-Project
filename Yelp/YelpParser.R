.libPaths("C:\\Users\\TempAdmin2\\Documents\\R\\win-library\\3.2")
setwd("C:/Users/TempAdmin2/Documents/Data Science/R Scripts/IDVA/Yelp")
#untar(tarfile = "yelp_dataset_challenge_academic_dataset.tar")
library("jsonlite")
library('stringr')
library('tidyr')
library('reshape2')
library('ggplot2')
library('igraph')
library('networkD3')
library('lubridate')

#####Business data loader#########
Businessdf <- stream_in(file("yelp_academic_dataset_business.json"))
colnames(Businessdf)

#Handpicking just Edinburgh
length(unique(df$city))
Businessdf <- Businessdf[(which(tolower(Businessdf$city) == 'edinburgh')), ]

#Filterting certain category of business 
Businessdf$Restaurant <- str_detect(tolower(Businessdf$categories), "restaurants")
#temp <- unnest(Businessdf[,c(8,5)])
#temp2 <- dcast(temp, name~categories)

Businessdf <- Businessdf[which(Businessdf$Restaurant == TRUE),]


#Getting attribute count
flatAtt <- flatten(Businessdf$attributes)
flatAtt <- as.data.frame(sapply(flatAtt, FUN = function(x) !is.na(x)))
flatAtt <- flatAtt*1
#Handpicking attributes
names(flatAtt)[c(1:22, 31, 37, 46, 51)]
flatAtt <- flatAtt[ ,c(1:22, 31, 37, 46, 51)]

#Creating hours attribute
flatHours <- flatten(Businessdf$hours)
flatHours <- as.data.frame(sapply(flatHours, FUN = function(x) !is.na(x)))
flatHours <- flatHours*1
sumflatHours <- rowSums(flatHours)
hoursBool <- as.data.frame(sapply(sumflatHours, function(x) if (x>10) x=1 else 0))
colnames(hoursBool) <- 'HourAtt'

hist(Merchdf$review_count, xlim=c(0,150), breaks=100, main="Histogram for Review counts", xlab="review counts")

#Merged dataframe for Merch and forward
Merchdf <- cbind(Businessdf[,c(1, 4, 7,8,10, 12, 13, 16, 17)], hoursBool, flatAtt)
Merchdf$catlen <- rowSums(Merchdf[,c(11:36)])
hist(Merchdf$catlen)

#Merchandizing by Reviews for Stars
g <- ggplot(Merchdf, aes(x=as.factor(catlen), y=review_count)) + 
      scale_y_log10() +
      geom_boxplot(outlier.shape=NA, aes(fill = catlen)) +
      #geom_jitter(size = 2) +
      xlab('Merchandizing Label') + ylab('Review Count') +
  scale_fill_gradient(low = "#c43131", high = "#e73b35") + 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        legend.position="none",
        panel.border=element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA))
ggsave('MerchReview.eps', g)

#Merchandizing by Stars for Operating Hours
# g <- ggplot(Merchdf, aes(x=catlen, y=stars, color = as.factor(`Operating Hours`))) + 
#   geom_jitter(size = 2) +
#   xlab('Merchandizing Labels Count') + ylab('Stars') +
#   scale_color_discrete(name="Operating\nHours")
# ggsave('MerchStars.png', g)

#All attributes
meltAtt <- melt(cbind(flatAtt, hoursBool))
meltAtt <- meltAtt[which(meltAtt$value == 1),]
meltAtt$value <- as.factor(meltAtt$value)
  g <- ggplot(meltAtt, aes(x=variable, fill = value)) + 
    geom_bar(width = 0.5) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 13),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.line=element_blank(),
          axis.text.y=element_blank(),
          axis.text.x=element_blank(),
          legend.position="none",
          panel.border=element_blank(),
          panel.background = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA)) +
    scale_y_continuous(breaks = c(0,316.5,633,949.5,1266))

ggsave('MerchAtts.png', g)

########Spatial Mapper###############
write.csv(Merchdf, 'merchCarto.csv')
g <- ggplot(df, aes(x)) + 
  stat_ecdf(geom='point', colour = "red", size = 3) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.line=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        legend.position="none",
        panel.border=element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA))
ggsave('cumplot.eps', g)

#####Review data loader#########
con <- file("yelp_academic_dataset_review.json")
readdoc <- readLines(con)#, n = 500)
Reviewdf <- data.frame()
for (i in 1:length(readdoc)){
  Reviewdf <- rbind(df,as.data.frame(fromJSON(readdoc[i])))
}
close(con)

#Alternate method - Takes forever
df <- stream_in(file("yelp_academic_dataset_review.json"))

Reviewdf <- Reviewdf[which(Reviewdf$business_id %in% Merchdf$business_id),]

#####CheckIn data loader######### - Don't know how to use it
# CheckIndf <- stream_in(file("yelp_academic_dataset_checkin.json"))
# unique(df$city)
# length(unique(df$city))
# length(unique(df$state))
# unique(df$state)
# df[1,]
# View(df)

#####Tip data loader#########
Tipdf <- stream_in(file("yelp_academic_dataset_tip.json"))
colnames(Tipdf)
Tipdf <- Tipdf[(which(Tipdf$business_id %in% Merchdf$business_id)),]
tippedBus <- as.data.frame(table(Tipdf$business_id))
tippedBus <- tippedBus[order(tippedBus$Freq, decreasing = TRUE), ]

#Loading previously saved user data
load("~/Data Science/R Scripts/IDVA/Yelp/ddd.RData")
#Creating the userdf
userdf <- df[(df$user_id %in% Tipdf$user_id), ]
rm(df)

temp <- unnest(userdf$elite)
temp <- unnest(userdf[,c(5,11)])
temp2 <- dcast(temp, user_id ~ elite, fun.aggregate = max)
hist(temp$elite)

tipper <- merge(tippedBus, Merchdf[,c(1, 6, 8)], by.x = 'Var1', by.y = 'business_id', all.y = TRUE)
g <- 
  ggplot(tipper, aes(x=catlen, y=stars, color = Freq)) + 
  geom_jitter(size = 2, na.rm=TRUE) +
  xlab('Merchandizing Labels Count') + ylab('Stars') +
  scale_fill_discrete(name="Operating\nHours") +
  scale_color_gradient(low = "#132B43", high = "#56B1F7", space = "Lab", na.value = "#e17f35", 
guide = "colourbar")
ggsave('TipperMerchStars.png', g)

tipper$Freq[which(is.na(tipper$Freq))] <- 0
table(tipper$Freq)
hist(tipper$Freq[which(tipper$Freq < 30)])
ggplot(tipper[which(tipper$Freq < 30),], aes(x=catlen, y=Freq)) + 
  geom_line(stat='identity') +
  xlab('Merchandizing Labels Count') + ylab('Stars') +
  scale_fill_discrete(name="Operating\nHours") +
  scale_color_gradient(low = "#132B43", high = "#56B1F7", 
                       space = "Lab", 
                       na.value = "#e17f35", 
                       guide = "colourbar")

confind <- function(friends){
  output <- data.frame()
  if (length(friends$friends[[1]]) > 0){
    lst <- strsplit(friends$friends[[1]], " ")
    matcher <- strsplit(userdf$user_id, " ")
    output <- data.frame(userdf$user_id[which(lst %in% matcher)])
    #print(dim(output))
    output$friend <- rep(friends$user_id, dim(output)[[1]])
    colnames(output) <- c('network','node')
  }
  return (output)
}

revNet <- data.frame(network = character(0), friend = character(0), stringsAsFactors=FALSE)
for (user in (1:dim(userdf)[1])){
  revNet <- rbind(revNet,confind(userdf[user,5:6]))
}

revNet <- cbind(revNet, type = 'friend')
revNet <- rbind(revNet, cbind(network = Reviewdf[,8], node = Reviewdf[,2], type = 'business'))

#Reviewed well merchanized resturants
nodeWell <- revNet$node[which(revNet$network %in% Merchdf$business_id[which(Merchdf$catlen >= 23)])]
nodeNotWell <- revNet$node[which(revNet$network %in% Merchdf$business_id[which(Merchdf$catlen < 23)])]
networker <- revNet[which(revNet$network %in% nodeWell & revNet$node %in% nodeWell),c(1,2)]
networker <- revNet[which(revNet$network %in% nodeNotWell & revNet$node %in% nodeNotWell),c(1,2)]

n <- simpleNetwork(networker, charge = -2000)
n

verticedf <- subset(userdf[which(userdf$user_id %in% networker$node | 
                                   userdf$user_id %in% networker$network),], select=c(5,1:4,11))
verticedf$year <- year(as.POSIXct(paste(verticedf$yelping_since,"-01",sep=""),'%Y-%m-%d'))


temp <- unnest(verticedf[,c(1,6)])
temp2 <- dcast(temp, user_id ~ elite, fun.aggregate = max)
temp <- data.frame(lapply(temp2, function(x) replace(x, is.infinite(x),0)))
temp <- data.frame(lapply(temp[,-1], function(x) replace(x, x != 0, 1)))
temp2$eliteness <- rowSums(temp)
verticedf <- merge(verticedf, temp2[,c(1,12)], by.x = 'user_id', by.y = 'user_id', all.x = TRUE)
verticedf$eliteness[which(is.na(verticedf$eliteness))] <- 0

net <- graph_from_data_frame(d=networker, vertices=verticedf, directed = FALSE) 
net <- simplify(net, remove.multiple = F, remove.loops = T)


V(net)$size <- V(net)$eliteness
V(net)$color <- '#c02126'
E(net)$color <- '#ebebeb'
pdf("nodenotWell.pdf") 
plot(net, edge.arrow.size=.4, vertex.label=NA, 
     #vertex.size = 6, 
     layout=layout.fruchterman.reingold)
dev.off()
