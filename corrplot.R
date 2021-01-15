L6 <- read.table("table.json.even3000_sorted_L6.txt",sep = "\t",header = T,row.names = 1)
map <- read.table("Preterm.intestin.txt",sep = "\t",header = T,row.names = 1)

library(corrplot);library(psych)
select <- L6[which(rownames(L6)%in% c("Gemmiger","Coprococcus","Bilophila",
                                                         "Actinomyces","Granulicatella","Oscillospira",
                                                         "Rothia","Prevotella","Clostridium","Porphyromonas",
                                                         "Lautropia","Haemophilus","Neisseria",
                                                         "Streptococcus","Lactobacillus")),]

all_ind <- map[,c("CRP","gestationalage.delivery","neoweight","apgar.1min","apgar.5min","apgar.10min","BMI.delivery","age")]
all_ind <- all_ind[complete.cases(all_ind),]
sid <- intersect(colnames(L6),rownames(all_ind))
all1 <- t(select[,sid])
all_ind <- all_ind[sid,]
m <- merge(all1,all_ind,by="row.names")
all_res <- corr.test(m[,2:17],m[,18:25],method = "spearman",adjust = "none")
# p <- data.frame(t(all_res$p))
# p1 <- sapply(p,p.adjust,method="fdr")
rownames(p1) <- rownames(p)
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                           "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                           "#4393C3", "#2166AC", "#053061"))
colors <- c("#67001E","#67001D",col2(26),"#053063","#053062")
out.corr <- corrplot(all_res$r,col = rev(colors),
                     p.mat = all_res$p, sig.level = 0.1, insig = "blank",tl.col = "black")
