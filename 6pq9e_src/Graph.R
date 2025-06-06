library(ggplot2)
library(plyr)

data = read.csv("edu_detailed.csv",header=TRUE)
data2 = read.csv("nit_detailed.csv",header=TRUE)

edu1_1 <- data.frame(choice=c(rep("Tap",200)),probability=c(data$V1))
edu1_2 <- data.frame(choice=c(rep("Bottled",200)),probability=c(data$V2))
edu1_3 <- data.frame(choice=c(rep("Filtered",200)),probability=c(data$V3))

edu2_1 <- data.frame(choice=c(rep("Tap",200)),probability=c(data$V4))
edu2_2 <- data.frame(choice=c(rep("Bottled",200)),probability=c(data$V5))
edu2_3 <- data.frame(choice=c(rep("Filtered",200)),probability=c(data$V6))

edu3_1 <- data.frame(choice=c(rep("Tap",200)),probability=c(data$V7))
edu3_2 <- data.frame(choice=c(rep("Bottled",200)),probability=c(data$V8))
edu3_3 <- data.frame(choice=c(rep("Filtered",200)),probability=c(data$V9))

edu4_1 <- data.frame(choice=c(rep("Tap",200)),probability=c(data$V10))
edu4_2 <- data.frame(choice=c(rep("Bottled",200)),probability=c(data$V11))
edu4_3 <- data.frame(choice=c(rep("Filtered",200)),probability=c(data$V12))

edu5_1 <- data.frame(choice=c(rep("Tap",200)),probability=c(data$V13))
edu5_2 <- data.frame(choice=c(rep("Bottled",200)),probability=c(data$V14))
edu5_3 <- data.frame(choice=c(rep("Filtered",200)),probability=c(data$V15))

nitmax_1 <- data.frame(choice=c(rep("Tap",200)),probability=c(data2$V1))
nitmax_2 <- data.frame(choice=c(rep("Bottled",200)),probability=c(data2$V2))
nitmax_3 <- data.frame(choice=c(rep("Filtered",200)),probability=c(data2$V3))

nitq75_1 <- data.frame(choice=c(rep("Tap",200)),probability=c(data2$V4))
nitq75_2 <- data.frame(choice=c(rep("Bottled",200)),probability=c(data2$V5))
nitq75_3 <- data.frame(choice=c(rep("Filtered",200)),probability=c(data2$V6))

nitmed_1 <- data.frame(choice=c(rep("Tap",200)),probability=c(data2$V7))
nitmed_2 <- data.frame(choice=c(rep("Bottled",200)),probability=c(data2$V8))
nitmed_3 <- data.frame(choice=c(rep("Filtered",200)),probability=c(data2$V9))

nitq25_1 <- data.frame(choice=c(rep("Tap",200)),probability=c(data2$V10))
nitq25_2 <- data.frame(choice=c(rep("Bottled",200)),probability=c(data2$V11))
nitq25_3 <- data.frame(choice=c(rep("Filtered",200)),probability=c(data2$V12))

nitmin_1 <- data.frame(choice=c(rep("Tap",200)),probability=c(data2$V13))
nitmin_2 <- data.frame(choice=c(rep("Bottled",200)),probability=c(data2$V14))
nitmin_3 <- data.frame(choice=c(rep("Filtered",200)),probability=c(data2$V15))

edu1 <- rbind(edu1_1,edu1_2,edu1_3)
edu2 <- rbind(edu2_1,edu2_2,edu2_3)
edu3 <- rbind(edu3_1,edu3_2,edu3_3)
edu4 <- rbind(edu4_1,edu4_2,edu4_3)
edu5 <- rbind(edu5_1,edu5_2,edu5_3)

nit1 <- rbind(nitmin_1,nitmin_2,nitmin_3)
nit2 <- rbind(nitq25_1,nitq25_2,nitq25_3)
nit3 <- rbind(nitmed_1,nitmed_2,nitmed_3)
nit4 <- rbind(nitq75_1,nitq75_2,nitq75_3)
nit5 <- rbind(nitmax_1,nitmax_2,nitmax_3)

m_edu1 <- ddply(edu1, "choice", summarise, grp.mean=mean(probability))
m_edu2 <- ddply(edu2, "choice", summarise, grp.mean=mean(probability))
m_edu3 <- ddply(edu3, "choice", summarise, grp.mean=mean(probability))
m_edu4 <- ddply(edu4, "choice", summarise, grp.mean=mean(probability))
m_edu5 <- ddply(edu5, "choice", summarise, grp.mean=mean(probability))

m_nit1 <- ddply(nit1, "choice", summarise, grp.mean=mean(probability))
m_nit2 <- ddply(nit2, "choice", summarise, grp.mean=mean(probability))
m_nit3 <- ddply(nit3, "choice", summarise, grp.mean=mean(probability))
m_nit4 <- ddply(nit4, "choice", summarise, grp.mean=mean(probability))
m_nit5 <- ddply(nit5, "choice", summarise, grp.mean=mean(probability))

head(m_edu1)
head(m_edu2)
head(m_edu3)
head(m_edu4)
head(m_edu5)

head(m_nit1)
head(m_nit2)
head(m_nit3)
head(m_nit4)
head(m_nit5)

library(gridExtra)
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

plot1 <- ggplot(edu1, aes(probability, fill=choice)) + geom_density(alpha = .5) + xlim(0,0.7)+geom_vline(data=m_edu1, aes(xintercept=grp.mean,color=choice),linetype="dashed")  + labs(title = "Edu. No degree") + theme(legend.position = "bottom")
plot2 <- ggplot(edu2, aes(probability, fill=choice)) + geom_density(alpha = .5) + xlim(0,0.7)+geom_vline(data=m_edu2, aes(xintercept=grp.mean,color=choice),linetype="dashed")  + labs(title = "Edu. Middle school") + theme(legend.position = "none")
plot3 <- ggplot(edu3, aes(probability, fill=choice)) + geom_density(alpha = .5) + xlim(0,0.7)+geom_vline(data=m_edu3, aes(xintercept=grp.mean,color=choice),linetype="dashed")  + labs(title = "Edu. High school") + theme(legend.position = "none")
plot4 <- ggplot(edu4, aes(probability, fill=choice)) + geom_density(alpha = .5) + xlim(0,0.7)+geom_vline(data=m_edu4, aes(xintercept=grp.mean,color=choice),linetype="dashed")  + labs(title = "Edu. Bachelor") + theme(legend.position = "none")
plot5 <- ggplot(edu5, aes(probability, fill=choice)) + geom_density(alpha = .5) + xlim(0,0.7)+geom_vline(data=m_edu5, aes(xintercept=grp.mean,color=choice),linetype="dashed")  + labs(title = "Edu. Master and higher") + theme(legend.position = "none")
plotS <- ggplot() + theme_void() 
plot6 <- ggplot(nit1, aes(probability, fill=choice)) + geom_density(alpha = .5) + xlim(0,0.7)+geom_vline(data=m_nit1, aes(xintercept=grp.mean,color=choice),linetype="dashed")  + labs(title = "nitrates min.") + theme(legend.position = "none")
plot7 <- ggplot(nit2, aes(probability, fill=choice)) + geom_density(alpha = .5) + xlim(0,0.7)+geom_vline(data=m_nit2, aes(xintercept=grp.mean,color=choice),linetype="dashed")  + labs(title = "nitrates q25") + theme(legend.position = "none")
plot8 <- ggplot(nit3, aes(probability, fill=choice)) + geom_density(alpha = .5) + xlim(0,0.7)+geom_vline(data=m_nit3, aes(xintercept=grp.mean,color=choice),linetype="dashed")  + labs(title = "nitrates median") + theme(legend.position = "none")
plot9 <- ggplot(nit4, aes(probability, fill=choice)) + geom_density(alpha = .5) + xlim(0,0.7)+geom_vline(data=m_nit4, aes(xintercept=grp.mean,color=choice),linetype="dashed")  + labs(title = "nitrates q75") + theme(legend.position = "none")
plot10 <- ggplot(nit5, aes(probability, fill=choice)) + geom_density(alpha = .5) + xlim(0,0.7)+geom_vline(data=m_nit5, aes(xintercept=grp.mean,color=choice),linetype="dashed")  + labs(title = "nitrates max.") + theme(legend.position = "none") 

legend <- get_legend(plot1)
plot1 <- plot1 + theme(legend.position="none")

grid.arrange(plot1,plot2,plot3,plot4,plot5,plotS,plot6,plot7,plot8,plot9,plot10,legend,ncol=2)