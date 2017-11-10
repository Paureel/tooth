multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


data(ToothGrowth)
tg.agg <- aggregate(ToothGrowth, by=list(ToothGrowth$supp, ToothGrowth$dose),FUN=mean, na.rm=TRUE)
g1 <- ggplot(tg.agg, aes(tg.agg$Group.2, tg.agg$len)) +   
  geom_bar(aes(fill = tg.agg$Group.1), position = "dodge", stat="identity")
g1 <- g1+scale_fill_manual(values=c("orange", "steelblue"))

boxplot(len~supp*dose, data=ToothGrowth, notch=FALSE, 
col=(c("orange","steelblue")),main="Tooth Growth", xlab="Suppliment and Dose")
t <- vector()
for (i in unique(ToothGrowth$dose)){
  t <- c(t, t.test(ToothGrowth[ToothGrowth$dose == i & ToothGrowth$supp == "VC" ,]$len,ToothGrowth[ToothGrowth$dose == i & ToothGrowth$supp == "OJ" ,]$len, var.equal=FALSE, paired=FALSE)[3])
  
  
  
}

g2 <- ggplot(data = ToothGrowth[ToothGrowth$dose == 0.5,], aes(len, fill = supp)) + geom_density(alpha = 0.2) + ggtitle("A")
g3 <- ggplot(data = ToothGrowth[ToothGrowth$dose == 1,], aes(len, fill = supp)) + geom_density(alpha = 0.2) + ggtitle("B")
g4 <- ggplot(data = ToothGrowth[ToothGrowth$dose == 2,], aes(len, fill = supp)) + geom_density(alpha = 0.2) + ggtitle("C")

binded <- data.frame(nrow = 10000)
for (i in unique(ToothGrowth$dose)){
B = 10000
n = nrow(ToothGrowth[ToothGrowth$dose == i,])
boot.samples1 = matrix(sample(ToothGrowth[ToothGrowth$dose == i & ToothGrowth$supp == "VC" ,]$len, size = B * n, replace = TRUE),B, n)
boot.samples2 = matrix(sample(ToothGrowth[ToothGrowth$dose == i & ToothGrowth$supp == "OJ" ,]$len, size = B * n, replace = TRUE),B, n)
boot.statistics1 = apply(boot.samples1, 1, mean)
boot.statistics2 = apply(boot.samples2, 1, mean)
binded <- cbind(binded, data.frame("VC" = boot.statistics1, "OJ" = boot.statistics2))
}
binded <- binded[,-1]
melted <- melt(binded)

stats <- ggplot(data.frame(meanTime = boot.statistics),aes(x=meanTime)) +
  geom_histogram(binwidth=0.25,aes(y=..density..)) +
  geom_density(color="red")
g5 <- ggplot(data = melted, aes(value, fill = variable)) + geom_density(alpha = 0.2)  + ggtitle("D")
multiplot(g2, g3, g4, g5, cols=2)
#t.test(melted[melted$variable == "VC.2" ,]$value,melted[melted$variable == "OJ.2", ]$value,var.equal=FALSE, paired=FALSE)