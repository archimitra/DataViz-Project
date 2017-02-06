#Libraries
library('httr')
library('curlconverter')
library('plyr')
library('ggplot2')
options(warn=-1)

#Auth from Graph API explorer
auth <- ''


#Function to grab data from Facebook
dataGrabber <- function(endpoint, auth){
  output <- list()
  URL <- paste('"https://graph.facebook.com/v2.7/', endpoint ,'"', sep='')
  repeat{
    curler <- paste('curl -X GET -H "Authorization: OAuth ', auth, '"', sep='')
    resp <- make_req(straighten(paste(curler, URL, sep = ' ')))
    output[length(output)+1] <- list(content(resp[[1]]()))
    if(is.null(output[[length(output)]]$paging$`next`))
      {
      break
    }
    URL <- output[[length(output)]]$paging$`next`
  }
  return (output)
}

#Function to convert to dataframe
dfconverter<-function(lister){
  df <- data.frame(stringsAsFactors = FALSE) 
  for (pagi in lister){
    for (post in pagi$data){
      df <- rbind.fill(df, as.data.frame(post))
    }
  }
  return(df)
}


#Creating post dataframe
posts <- dataGrabber('me/posts', auth)
postDF <- dfconverter(posts)
postDF$id <- sapply(postDF$id, as.character)

#LIKE and Comment dataframe for posts
likesCommentGrabber<- function(endpoint){
  indiDF <- dfconverter(dataGrabber(endpoint, auth))
  return (indiDF)
}

#Like data cleaner
likeList <- sapply(postDF$id, function(x) likesCommentGrabber(paste(x,'/likes',sep='')))
likesDF <- as.data.frame(do.call(rbind, likeList))
likesDF$postID <- row.names(likesDF)
likesDF$postID <- regmatches(likesDF$postID,regexpr('([0-9]|_)*',likesDF$postID))

#Comment data cleaner
commentList <- sapply(postDF$id, function(x) likesCommentGrabber(paste(x,'/comments',sep='')))
commentDF <- as.data.frame(do.call(rbind, commentList))
commentDF$postID <- row.names(commentDF)
commentDF$postID <- regmatches(commentDF$postID,regexpr('([0-9]|_)*',commentDF$postID))


#Getting type of posts and appending it to the postDF
typeDF <- data.frame()
typeList <- lapply(postDF$id, function(x) dataGrabber(paste(x,'?fields=type',sep=''),auth))
for (type in typeList){
  tryCatch({
    typeDF <- rbind(typeDF, data.frame(type))
   }, error=function(e){})
}
typeDF <- dcast(typeDF, id~type)
postDF <- merge(x = postDF, y = typeDF, by = "id", all = TRUE)
postDF <- postDF[rev(order(postDF$created_time)), ]

#~~~~~~~~~~~Dataframe treatment~~~~~~~~~~~~~~~~
#Treating time data 
postDF$created_time <- as.POSIXlt(postDF$created_time, format="%Y-%m-%dT%H:%M:%S")
postDF$Year <- format(postDF$created_time, '%Y')
postDF$Month <- months(postDF$created_time)

#Calculating and adding like and comment per post data
postDF$TotalLikes <- lapply(postDF$id, function(x) length(which(likesDF$postID == x)))
postDF$TotalLikes <- as.numeric(postDF$TotalLikes)
postDF$TotalComments <- lapply(postDF$id, function(x) length(which(commentDF$postID == x)))
postDF$TotalComments <- as.numeric(postDF$TotalComments)

#Creating life event boolean
postDF$lifeEvent <- !is.na(str_extract(as.character(postDF$story), "life event"))

#~~~~@@@@@@!!!!!WARNING!!!!!@@@@@@~~~~~~
#The following section consists of plots
#This is totally to coded work for my personal use case and exist here just as an example
#The working of the following plots depend on how your dataset looks like
#Proceed with extreme prejudice

#Likes by Year
likeByYear <- aggregate(TotalLikes ~ Year, data=postDF, FUN=sum)
ggplot(data=postDF, aes(x=Year, y=TotalComments)) +
  geom_boxplot()
  xlab("Year") + ylab("Facebook Likes") + ggtitle("Likes by Years")

#Posts by Year
postByYear <- aggregate(rep(1, nrow(postDF)), by = list(x = postDF$Year, y = postDF$lifeEvent), sum)
colnames(postByYear) <- c('Year', 'lifeEvent' ,'Posts')
postByYear$lifeEvent <- as.character(lapply(postByYear$lifeEvent, function(x) if (x==TRUE){ 'Life Event'} else {'Normal Post'}))
ggplot(data=postByYear) + aes(x=factor(Year)) + 
  geom_line(aes(y=Posts, group=lifeEvent, colour = lifeEvent),size=1.2) + 
  xlab("Year") + ylab("Facebook Posts") +
  ggtitle('Facebook Posts by Year') +
  scale_fill_discrete(breaks=c(TRUE,FALSE),
                      labels=c('Normal Post', 'Life Event')) +
  ylab('Posts') + xlab('Year') +
  theme(legend.title = element_blank(),
        plot.title = element_text(size=24, color = '#1B3690'),
        axis.title.x = element_text(size=14,color = '#30309F'),
        axis.title.y = element_text(size=14,color = '#30309F'))

#Comments by Year
commentByYear <- aggregate(TotalComments ~ Year+photo, data=postDF, FUN=sum)
commentByYear <- commentByYear[(which(commentByYear$Year > 2009)),]
commentByYear$photo <- as.character(lapply(commentByYear$photo, function(x) if (x==0){ 'Post'} else {'Photo Post'}))

g <- ggplot(data=commentByYear, aes(x=Year, y=TotalComments, fill=photo)) +
  geom_bar(stat='identity', position=position_dodge(width = 0.7), width = 0.6) + 
  scale_fill_manual(values=c("#f7f7f7", "#8b9dc3"))  +
  theme(axis.line=element_blank(),
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

ggsave("filename.eps", g)


#Likes + Posts
g <-  ggplot(data=postDF[-(which(postDF$Year==1991)),], aes(x=created_time, y=TotalLikes)) +
    geom_point(aes(color=type),size=7) +
    scale_color_manual(values=c("#e79123", "#0DB9E8","#10E8A0", "#1BFFF3")) +
    ylab('Likes') + xlab('Year') +
    theme(axis.line=element_blank(),
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

ggsave("scatter1.eps", g, width = 16, height = 9)

    # theme(legend.title = element_blank(),
#       legend.justification=c(0,0), 
#       legend.position=c(0,0),
#       plot.title = element_text(size=24, color = '#f7f7f7'),
#       axis.title.x = element_text(size=14,color = '#f7f7f7'),
#       axis.title.y = element_text(size=14,color = '#ffffff'),
#       panel.background = element_rect(fill = "#3B589D"),
#       panel.grid.major.x = element_blank(), 
#       panel.grid.minor.x = element_blank(),
#       panel.grid.major.y = element_blank(),
#       plot.background = element_rect(fill = "#3B589D"),
#       axis.text = element_text(colour = '#ffffff'))
                
                    
StatusDF <- aggregate(TotalLikes ~ lifeEvent ,data = postDF[which(postDF$Year<2013),], FUN=sum)
StatusDF <- cbind(StatusDF,c(rep('2009-2012',2)))
colnames(StatusDF) <- c('LifeEvent', 'Likes', 'Years')

StatusDF1 <- aggregate(TotalLikes ~ lifeEvent ,data = postDF[which(postDF$Year>=2013),], FUN=sum)
StatusDF1 <- cbind(StatusDF1,c(rep('2013-2016',2)))
colnames(StatusDF1) <- c('LifeEvent', 'Likes', 'Years')

StatusDF <- rbind(StatusDF,StatusDF1)
StatusDF$LifeEvent <- as.factor(StatusDF$LifeEvent)

g <- ggplot(data=StatusDF, aes(x=Years, y=Likes, fill=LifeEvent)) +
      geom_bar(stat='identity', width=.5)  + 
      coord_flip() +
      scale_fill_manual(values=c("#f7f7f7", "#8b9dc3")) + 
      ylab('Likes') + xlab('Year') +
      theme(axis.line=element_blank(),
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

ggsave("horiBar.eps", g)

  
  
#    theme(legend.title = element_blank(),
#           legend.justification=c(0,0), 
#           legend.position=c(1,0),
#           plot.title = element_text(size=28, color = '#f7f7f7'),
#           axis.title.x = element_text(size=18,color = '#f7f7f7'),
#           axis.title.y = element_text(size=18,color = '#ffffff'),
#           panel.background = element_rect(fill = "#3B589D"),
#           panel.grid.major.y = element_blank(), 
#           panel.grid.minor.y = element_blank(),
#           plot.background = element_rect(fill = "#3B589D"),
#           axis.text = element_text(colour = '#ffffff'))
#   
    
#WordCloud
temp <- data.frame(table(likesDF$name))
temp2 <- data.frame(table(commentDF$from.name))
colnames(temp2) <- c('name', 'freq')  
colnames(temp2) <- c('name', 'freq')  
wordcloud <- rbind(temp,temp2)
wordcloud <- aggregate(freq~name, data=wordcloud, FUN=sum)
write.table(wordcloud, file = "worldcloud.csv")  


df <- aggregate(TotalComments ~ type, data=postDF, FUN=sum)
df <- melt(data.frame(df))
df<-df[order(df$TotalComments),]
row.names(df) <- c(1,2,3,4)

g <- ggplot(df, aes(x=type, y=TotalComments, fill=type, label=TotalComments)) +
       coord_polar() + geom_bar(width = 1.0, stat='identity') +
  scale_fill_manual(values=c("#0F8CFF", "#0DB9E8","#10E8A0", "#1BFFF3")) + 
  ylab('Posts') + xlab('Type') +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA))

ggsave("Polar.eps", g)

  
    
#   theme(legend.title = element_blank(),
#         legend.justification=c(0,0), 
#         legend.position=c(1,0),
#         plot.title = element_text(size=28, color = '#f7f7f7'),
#         axis.title.x = element_text(size=18,color = '#f7f7f7'),
#         axis.title.y = element_text(size=18,color = '#ffffff'),
#         panel.background = element_rect(fill = "#3B589D"),
#         panel.grid.major.y = element_blank(), 
#         panel.grid.minor.y = element_blank(),
#         plot.background = element_rect(fill = "#3B589D"),
#         axis.text = element_text(colour = '#ffffff'))
# 
