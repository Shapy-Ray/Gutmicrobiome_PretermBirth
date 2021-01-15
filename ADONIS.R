all <- read.table("P.H/adonis.bray.txt",sep = "\t",header = T)
sub <- read.table("P.subgroups/adonis.bray.sub.txt",sep = "\t",header = T)

all1 <- subset(all,P<0.05)
all1 <- all1[which(all1$Category%in%c("gestationalage.delivery","tocolytictherapy",
                                    "CRP","neoweight","apgar.1min","apgar.5min","apgar.10min")),]
sub1 <- subset(sub,P<0.05)

variables <- union(all1$Category,sub1$Category)

all <- subset(all,Category%in%variables)
sub <- subset(sub,Category%in%variables)

all <- all[order(all$R2,decreasing = T),]
all$Category <- factor(all$Category,levels = all$Category)
sub$Category <- factor(sub$Category,levels = all$Category)


library(ggplot2)

p <- ggplot(all,aes(Category,R2,fill=Category))
p + geom_bar(stat = "identity")+theme_bw()+theme(legend.position = "none")
ggsave("P.H.pdf",width = 4,height = 2)

p <- ggplot(sub,aes(Category,R2,fill=Category))
p + geom_bar(stat = "identity")+theme_bw()+theme(legend.position = "none")
ggsave("sub.pdf",width = 4,height = 2)

all$Category1 <- paste(all$Category,"all",sep = ".")
sub$Category1 <- paste(sub$Category,"sub",sep = ".")

m <- rbind(all,sub)
p <- ggplot(m,aes(Category,R2,group=Category1,fill=Category1))
p + geom_bar(aes(group=Category1,fill=Category1),stat = "identity",position = "dodge")+theme_bw()+theme(legend.position = "none")

