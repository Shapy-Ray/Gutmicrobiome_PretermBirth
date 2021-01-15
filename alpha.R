dta <- read.table("adiv-4group.txt",sep = "\t",header = T)
#meta <- read.table("Preterm.intestin.txt",sep = "\t",header = T,row.names = 1)

library(ggplot2)

p <- ggplot(dta,aes(group,shannon,fill=group))
p + geom_boxplot()+theme_bw()+theme(legend.position = "none",axis.text.y = element_text(angle = 90))
ggsave("shannon.pdf",width = 2,height = 2)

p <- ggplot(dta,aes(group,PD_whole_tree,fill=group))
p + geom_boxplot()+theme_bw()+theme(legend.position = "none",axis.text.y = element_text(angle = 90))
ggsave("pdwtree.pdf",width = 2,height = 2)

p <- ggplot(dta,aes(group,observed_otus,fill=group))
p + geom_boxplot()+theme_bw()+theme(legend.position = "none",axis.text.y = element_text(angle = 90))
ggsave("obotus.pdf",width = 2,height = 2)

dta.p.h <- subset(dta,group%in%c("0.Healthy","1.Preterm"))
wilcox.test(dta.p.h$shannon~dta.p.h$group)
wilcox.test(dta.p.h$PD_whole_tree~dta.p.h$group)
wilcox.test(dta.p.h$observed_otus~dta.p.h$group)

dta.sub <- subset(dta,group!="1.Preterm")
library(agricolae)
com.sha <- with(dta.sub,kruskal(shannon,group,group=F,p.adj="fdr"))
com.pdt <- with(dta.sub,kruskal(PD_whole_tree,group,group=F,p.adj="fdr"))
com.obs <- with(dta.sub,kruskal(observed_otus,group,group=F,p.adj="fdr"))   
print(com.sha)
print(com.pdt)
print(com.obs)

