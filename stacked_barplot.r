
library(ggplot2)
dta=read.table("L2.4groups/L2.txt",header=T)
num_groups = length(unique(dta$Group))

pdf("L2.4groups/L2.stacked_barplot.pdf",height=3.5,width=num_groups*0.5+3,useDingbats=FALSE)

colors = c("#80B1D3","#B3DE69","#FFFFB3","#8DD3C7","#4daf4a","#377eb8","#BEBADA","#FB8072","#FDB462","#FCCDE5","#BC80BD","#CCEBC5","#FFED6F","#CD4F39","#BC41A4","#4F94CD")
color = c(colors[1:8],"#D9D9D9")

dta$Stack<-factor(dta$Stack,levels = unique(dta$Stack))
ggplot(dta,aes(x=Group,y=Value,fill=Stack))+geom_bar(width=.8,stat='identity')+
  scale_fill_manual(values=color,breaks=rev(levels(dta$Stack)))+ theme_classic()+ xlab("")+ylab("")+labs(fill="")+
  theme(axis.title.x=element_text(vjust=3.5), axis.text.x=element_text(size=8,color="black"),axis.line=element_line(colour ="grey"),
        panel.background = element_rect(fill = "transparent",colour = "grey", size = 1, linetype = 1), panel.border=element_rect(fill="transparent",color="grey"))

