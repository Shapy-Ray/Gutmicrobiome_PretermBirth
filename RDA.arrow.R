library(ggplot2)
library(vegan)
library(dplyr)
library(scales)
library(grid)
library(reshape2)
library(phyloseq)

map <- read.table("Preterm.intestin.txt",sep = "\t",header = T)
map = map[complete.cases(map),]
map$group <- substr(map$SampleID,1,6)
otu <- import_biom(BIOMfilename = "table.json.even3000.biom")
map <- sample_data(map)
rownames(map) <- map$SampleID
moth_merge <- merge_phyloseq(otu, map)
moth_merge
colnames(tax_table(moth_merge)) <- c("Kingdom", "Phylum", "Class","Order", "Family", "Genus","Species")
erie <- moth_merge
bray_not_na <- phyloseq::distance(physeq = erie, method = "bray")

cap_ord <- ordinate(
  physeq = erie, 
  method = "CAP",
  distance = bray_not_na,
  formula = ~ gestationalage.delivery + apgar.1min + apgar.5min + apgar.10min + BMI.delivery + age )
cap_plot <- plot_ordination(physeq = erie, ordination = cap_ord, color = "group", axes = c(1,2)) + 
  scale_color_manual(values = c("#a65628", "magenta"))

arrowmat <- vegan::scores(cap_ord, display = "bp")
arrowdf <- data.frame(labels = rownames(arrowmat), arrowmat)
arrow_map <- aes(xend = CAP1, yend = CAP2, x = 0, y = 0, shape = NULL, color = NULL, label = labels)
label_map <- aes(x = 1.3 * CAP1, y = 1.3 * CAP2, shape = NULL, color = NULL, label = labels)

arrowhead = arrow(length = unit(0.02, "npc"))
cap_plot + 
  geom_segment(mapping = arrow_map, size = .5, data = arrowdf, color = "gray", arrow = arrowhead) + 
  geom_text(mapping = label_map, size = 4,data = arrowdf, show.legend = FALSE) +
  theme_bw()
ggsave("P.H.caparrow.pdf",width = 4,height = 3)

### subgroups
map <- read.table("Preterm.intestin.pretermsubgroup.txt",sep = "\t",header = T)
map = map[complete.cases(map),]
#map$group <- substr(map$SampleID,1,6)
otu <- import_biom(BIOMfilename = "preterm.subgroups.even3000.biom")
map <- sample_data(map)
rownames(map) <- map$SampleID
moth_merge <- merge_phyloseq(otu, map)
moth_merge
colnames(tax_table(moth_merge)) <- c("Kingdom", "Phylum", "Class","Order", "Family", "Genus","Species")

erie <- moth_merge
bray_not_na <- phyloseq::distance(physeq = erie, method = "bray")

erie_CAP <- ordinate(
  physeq = erie, 
  method = "CAP", 
  distance = bray_not_na,
  formula = ~ gestationalage.delivery + apgar.1min + apgar.5min + 
    apgar.10min + BMI.delivery + age + neoweight + remissionstage.day +CRP 
)

RDAplot <- plot_ordination(physeq = erie,ordination = erie_CAP,color = "group") + 
  scale_color_manual(values = c("#E96446", "#302F3D", "#87CEFA")) 


arrowmat <- scores(erie_CAP, display = "bp",)
arrowdf <- data.frame(labels = rownames(arrowmat), arrowmat)
arrow_map <- aes(xend = CAP1*1, yend = CAP2*1, x = 0, y = 0, 
                 shape = NULL, color = NULL, label = labels)
label_map <- aes(x = 1.2 * CAP1,y = 1.2 * CAP2, shape = NULL, color = NULL, label = labels)
RDAplot + 
  geom_segment(
    mapping = arrow_map, 
    size = .5, 
    data = arrowdf, 
    color = "gray", 
    arrow = arrowhead
  ) + 
  geom_text(
    mapping = label_map, 
    size = 4,  
    data = arrowdf, 
    show.legend = FALSE
  ) + 
  theme_bw()
ggsave("P.sub.caparrow.pdf",width = 5.3,height = 3)
