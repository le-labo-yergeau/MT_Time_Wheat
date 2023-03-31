###Produce stack bar chart for functions and taxonomy
##Taxonomy for ALL at phylum level - use the cluster for first part
library(tidyverse)
library(here)
#Import data files
MT <- readRDS(file = here("data","intermediate","MT.RDS"))
annot <- readRDS(file = here("data","intermediate","annot.RDS")) 
#Sort
MT.s <- MT[order(row.names(MT)),]
rm(MT)
annot.s <- annot[order(annot$gene_id),] 
rm(annot)
sum(annot.s$gene_id == row.names(MT.s)) #14,709,097
MT.tax <- data.frame(cbind(MT.s, annot.s$tax_phylum))
rm(annot.s)
rm(MT.s)

tax.phylum.summary <- MT.tax %>%
  group_by(annot.s.tax_phylum) %>%
  summarise(across(.cols=everything(), ~ sum(.x, na.rm = TRUE)))
saveRDS(tax.phylum.summary, file = here("data", "intermediate", "tax.phylum.summary.RDS"))

#Back from cluster, load file
tax.phylum.summary <- data.frame(readRDS(file = here("data", "intermediate", "tax.phylum.summary.RDS")))
colnames(tax.phylum.summary) <- gsub("X", "", colnames(tax.phylum.summary)) 
row.names(tax.phylum.summary) <- tax.phylum.summary[,1]
tax.phylum.summary <- tax.phylum.summary[,-1]
#Convert to relative abundance
tax.phylum.summary.rel <- data.frame(t(apply(tax.phylum.summary, 1, "/", colSums(tax.phylum.summary))))
colnames(tax.phylum.summary.rel) <- gsub("X", "", colnames(tax.phylum.summary.rel)) 
colSums(tax.phylum.summary.rel) #Should all be ones
#Keep only above 1% on average across all samples
tax.phylum.summary.rel.top <- tax.phylum.summary.rel[rowMeans(tax.phylum.summary.rel)>0.01, ] 
#Prepare for ggplot
map <- readRDS(file = here("data","intermediate", "map.RDS"))
map.s <- map[order(row.names(map)),]#Sort
tax.phylum.summary.rel.top.t <- t(tax.phylum.summary.rel.top) 
tax.phylum.summary.rel.top.t.s <- tax.phylum.summary.rel.top.t[order(row.names(tax.phylum.summary.rel.top.t)),]#Sort 
sum(row.names(map.s)==row.names(tax.phylum.summary.rel.top.t.s))#120
others <- data.frame(1-rowSums(tax.phylum.summary.rel.top.t.s)) #Add others
colnames(others)<-"Others"
tax.map <- data.frame(map.s,tax.phylum.summary.rel.top.t.s, others)
rowSums(tax.map[,6:20]) #Should be one
saveRDS(tax.map, file = here("data", "intermediate", "tax.map.RDS")) #For 08-ANOVAs
tax.map.long <- gather(tax.map,Phylum,relabund,6:20) #transform in long format for ggplot
tax.map.long$growthstage <- factor(tax.map.long$growthstage, c("tillering", "stemelongation", "booting", "heading", "flowering" ))#Reorder manually the growth stages

#Plot
palette(c(brewer.pal(n = 9, name = "Set1"),"lightgrey", "black", "darkred", "darkblue", "darkgreen", "purple4", "darkgrey", "white"))
stack.phylum <- ggplot(tax.map.long, aes(fill = Phylum, y = relabund, x = growthstage)) + 
  geom_bar( stat = "identity", position = "fill") +
  ylab("Fraction of reads") + 
  scale_fill_manual(values = palette(), guide = guide_legend(label.theme = element_text(face = "italic", size = 8)), labels = c("Acidobacteria", "Actinobacteria", "Ascomycota", "Bacteroidetes", "Can. Rokubacteria", "Chloroflexi", "Cyanobacteria", "Firmicutes", "Gemmatimonadetes", "No match", "Planctomycetes", "Proteobacteria", "Streptophyta", "Verrucomicrobia", "Others")) +
  theme_bw() +
  scale_y_continuous( expand = c(0,0)) +
  scale_x_discrete(name = "Growth Stage", labels = c("Tillering", "Stem Elongation", "Booting", "Heading", "Flowering")) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))+
  facet_wrap(vars(treatment2), labeller = labeller(treatment2 = c("A" = "Drought at Stem Elongation", "B" = "Drought at Booting", "C" = "Drought at Heading", "D" = "Control")))
stack.phylum
ggsave(stack.phylum, filename = here("output", "figs", "fig4.tiff"), device = "tiff", compression = "lzw", dpi = 600)

##Functions at the COG category level for ALL - use the cluster for first part
library(tidyverse)
library(here)
#Import data files
MT <- readRDS(file = here("data","intermediate","MT.RDS"))
annot <- readRDS(file = here("data","intermediate","annot.RDS")) 
#Sort
MT.s <- MT[order(row.names(MT)),]
rm(MT)
annot.s <- annot[order(annot$gene_id),] 
rm(annot)
sum(annot.s$gene_id == row.names(MT.s)) #14,709,097
MT.fun <- data.frame(cbind(MT.s, annot.s$cog_category))
rm(annot.s)
rm(MT.s)

fun.cog.summary <- MT.fun %>%
  group_by(annot.s.cog_category) %>%
  summarise(across(.cols=everything(), ~ sum(.x, na.rm = TRUE)))
saveRDS(fun.cog.summary, file = here("data", "intermediate", "fun.cog.summary.RDS"))

#Back from cluster, load file
fun.cog.summary <- data.frame(readRDS(file = here("data", "intermediate", "fun.cog.summary.RDS")))
colnames(fun.cog.summary) <- gsub("X", "", colnames(fun.cog.summary)) 
row.names(fun.cog.summary) <- fun.cog.summary[,1]
fun.cog.summary <- fun.cog.summary[,-1]
#Convert to relative abundance
fun.cog.summary.rel <- data.frame(t(apply(fun.cog.summary, 1, "/", colSums(fun.cog.summary))))
colnames(fun.cog.summary.rel) <- gsub("X", "", colnames(fun.cog.summary.rel)) 
colSums(fun.cog.summary.rel) #Should all be ones
#Keep only above 1% on average across all samples
fun.cog.summary.rel.top <- fun.cog.summary.rel[rowMeans(fun.cog.summary.rel)>0.01, ] 
#Prepare for ggplot
map <- readRDS(file = here("data","intermediate", "map.RDS"))
map.s <- map[order(row.names(map)),]#Sort
fun.cog.summary.rel.top.t <- t(fun.cog.summary.rel.top)#Transpose 
fun.cog.summary.rel.top.t.s <- fun.cog.summary.rel.top.t[order(row.names(fun.cog.summary.rel.top.t)),]#Sort 
sum(row.names(map.s)==row.names(fun.cog.summary.rel.top.t.s))#120
others <- data.frame(1-rowSums(fun.cog.summary.rel.top.t.s)) #Add others
colnames(others)<-"Others"
fun.map <- data.frame(map.s,fun.cog.summary.rel.top.t.s, others)
rowSums(fun.map[,6:20]) #Should be ones
saveRDS(fun.map, file = here("data", "intermediate", "fun.map.RDS")) #For 08-ANOVAs
fun.map.long <- gather(fun.map,COG,relabund,6:20) #transform in long format for ggplot
fun.map.long$growthstage <- factor(fun.map.long$growthstage, c("tillering", "stemelongation", "booting", "heading", "flowering" ))#Reorder manually the growth stages

#Plot
palette(c(brewer.pal(n = 9, name = "Set1"),"lightgrey", "black", "darkred", "darkblue", "darkgreen", "purple4", "darkgrey", "white"))
stack.cog <- ggplot(fun.map.long, aes(fill = COG, y = relabund, x = growthstage)) + 
  geom_bar( stat = "identity", position = "fill") +
  ylab("Fraction of reads") + 
  scale_fill_manual(values = palette(), labels = c("AA transport and metabolism", "CH transport and metabolism", "Cell envelope/outer membrane", "DNA replication/recomb./repair", "Energy prod. and conversion", "Function unknown", "General function prediction only", "Inorganic ion transport and metabolism", "Lipid metabolism", "No match", "Others", "Posttranslational mod./prot. turnover/chaperones", "Signal transduction", "Transcription", "Translation/ribosomes"), guide = guide_legend(label.theme = element_text(size = 8))) +
  theme_bw() +
  scale_y_continuous( expand = c(0,0)) +
  scale_x_discrete(name = "Growth Stage", labels = c("Tillering", "Stem Elongation", "Booting", "Heading", "Flowering")) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))+
  facet_wrap(vars(treatment2), labeller = labeller(treatment2 = c("A" = "Drought at Stem Elongation", "B" = "Drought at Booting", "C" = "Drought at Heading", "D" = "Control")))
stack.cog
ggsave(stack.cog, filename = here("output", "figs", "fig5.tiff"), device = "tiff", compression = "lzw", dpi = 600)
