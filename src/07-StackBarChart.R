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

#Omit NULL
tax.map.long.nonull <- tax.map.long[tax.map.long$Phylum != "NULL.",]
stack.phylum.2 <- ggplot(tax.map.long.nonull, aes(fill = Phylum, y = relabund, x = growthstage)) + 
  geom_bar( stat = "summary", fun ="mean", position = "stack") +
  ylab("Fraction of reads") + 
  scale_fill_manual(values = palette(), guide = guide_legend(label.theme = element_text(face = "italic", size = 8)), labels = c("Acidobacteria", "Actinobacteria", "Ascomycota", "Bacteroidetes", "Can. Rokubacteria", "Chloroflexi", "Cyanobacteria", "Firmicutes", "Gemmatimonadetes", "Planctomycetes", "Proteobacteria", "Streptophyta", "Verrucomicrobia", "Others")) +
  theme_bw() +
  scale_y_continuous( expand = c(0,0)) +
  scale_x_discrete(name = "Growth Stage", labels = c("Tillering", "Stem Elongation", "Booting", "Heading", "Flowering")) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))+
  facet_wrap(vars(treatment2), labeller = labeller(treatment2 = c("A" = "Drought at Stem Elongation", "B" = "Drought at Booting", "C" = "Drought at Heading", "D" = "Control")))
stack.phylum.2

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
stack.cog.1 <- ggplot(fun.map.long, aes(fill = COG, y = relabund, x = growthstage)) + 
  geom_bar( stat = "identity", position = "fill") +
  ylab("Fraction of reads") + 
  scale_fill_manual(values = palette(), labels = c("AA transport and metabolism", "CH transport and metabolism", "Cell envelope/outer membrane", "DNA replication/recomb./repair", "Energy prod. and conversion", "Function unknown", "General function prediction only", "Inorganic ion transport and metabolism", "Lipid metabolism", "No match", "Others", "Posttranslational mod./prot. turnover/chaperones", "Signal transduction", "Transcription", "Translation/ribosomes"), guide = guide_legend(label.theme = element_text(size = 8))) +
  theme_bw() +
  scale_y_continuous( expand = c(0,0)) +
  scale_x_discrete(name = "Growth Stage", labels = c("Tillering", "Stem Elongation", "Booting", "Heading", "Flowering")) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))+
  facet_wrap(vars(treatment2), labeller = labeller(treatment2 = c("A" = "Drought at Stem Elongation", "B" = "Drought at Booting", "C" = "Drought at Heading", "D" = "Control")))
stack.cog.1

#Omit NULL
fun.map.long.nonull <- fun.map.long[fun.map.long$COG != "NULL.",]
stack.cog.2 <- ggplot(fun.map.long.nonull, aes(fill = COG, y = relabund, x = growthstage)) + 
  geom_bar(stat = "summary", fun ="mean", position = "stack") +
  ylab("Fraction of reads") + 
  scale_fill_manual(values = palette(), labels = c("AA transport and metabolism", "CH transport and metabolism", "Cell envelope/outer membrane", "DNA replication/recomb./repair", "Energy prod. and conversion", "Function unknown", "General function prediction only", "Inorganic ion transport and metabolism", "Lipid metabolism", "Others", "Posttranslational mod./prot. turnover/chaperones", "Signal transduction", "Transcription", "Translation/ribosomes"), guide = guide_legend(label.theme = element_text(size = 8))) +
  theme_bw() +
  scale_y_continuous( expand = c(0,0)) +
  scale_x_discrete(name = "Growth Stage", labels = c("Tillering", "Stem Elongation", "Booting", "Heading", "Flowering")) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))+
  facet_wrap(vars(treatment2), labeller = labeller(treatment2 = c("A" = "Drought at Stem Elongation", "B" = "Drought at Booting", "C" = "Drought at Heading", "D" = "Control")))
stack.cog.2

###Stack bar charts for the DA transcripts (treatments vs. control, 5 growth stages x 3 treatments = 15)
#Create DA transcripts lists
A.T.list <- readRDS(here("data", "intermediate", "DE.results.T.AvsD.RDS"))$DEfound
#17,362 elements
A.SE.list <- readRDS(here("data", "intermediate", "DE.results.SE.AvsD.RDS"))$DEfound
#21,399 elements
A.B.list <- readRDS(here("data", "intermediate", "DE.results.B.AvsD.RDS"))$DEfound
#126,995 elements
A.H.list <- readRDS(here("data", "intermediate", "DE.results.H.AvsD.RDS"))$DEfound
#31,006 elements
A.F.list <- readRDS(here("data", "intermediate", "DE.results.F.AvsD.RDS"))$DEfound
#8,715 elements
B.T.list <- readRDS(here("data", "intermediate", "DE.results.T.BvsD.RDS"))$DEfound
#4,637 elements
B.SE.list <- readRDS(here("data", "intermediate", "DE.results.SE.BvsD.RDS"))$DEfound
#5,187 elements
B.B.list <- readRDS(here("data", "intermediate", "DE.results.B.BvsD.RDS"))$DEfound
#59,607 elements
B.H.list <- readRDS(here("data", "intermediate", "DE.results.H.BvsD.RDS"))$DEfound
#7,636 elements
B.F.list <- readRDS(here("data", "intermediate", "DE.results.F.BvsD.RDS"))$DEfound
#11,932 elements
C.T.list <- readRDS(here("data", "intermediate", "DE.results.T.CvsD.RDS"))$DEfound
#4,927 elements
C.SE.list <- readRDS(here("data", "intermediate", "DE.results.SE.CvsD.RDS"))$DEfound
#5,651 elements
C.B.list <- readRDS(here("data", "intermediate", "DE.results.B.CvsD.RDS"))$DEfound
#7,754 elements
C.H.list <- readRDS(here("data", "intermediate", "DE.results.H.CvsD.RDS"))$DEfound
#32,141 elements
C.F.list <- readRDS(here("data", "intermediate", "DE.results.F.CvsD.RDS"))$DEfound
#15,877 elements

#Create a data frame
annot.DA <- readRDS(here("data", "intermediate", "annot.RDS")) #14709097 obs. of 33 variables
DA  <- data.frame("DroughtSEatT" = as.integer(annot.DA$gene_id %in% A.T.list),
                                "DroughtSEatSE" = as.integer(annot.DA$gene_id %in% A.SE.list),
                                 "DroughtSEatB" = as.integer(annot.DA$gene_id %in% A.B.list),
                                 "DroughtSEatH" = as.integer(annot.DA$gene_id %in% A.H.list),
                                 "DroughtSEatF" = as.integer(annot.DA$gene_id %in% A.F.list),
                                "DroughtBatT" = as.integer(annot.DA$gene_id %in% B.T.list),
                                "DroughtBatSE" = as.integer(annot.DA$gene_id %in% B.SE.list),
                                "DroughtBatB" = as.integer(annot.DA$gene_id %in% B.B.list),
                                 "DroughtBatH" = as.integer(annot.DA$gene_id %in% B.H.list),
                                 "DroughtBatF" = as.integer(annot.DA$gene_id %in% B.F.list),
                                "DroughtHatT" = as.integer(annot.DA$gene_id %in% C.T.list),
                                "DroughtHatSE" = as.integer(annot.DA$gene_id %in% C.SE.list),
                                "DroughtHatB" = as.integer(annot.DA$gene_id %in% C.B.list),
                                "DroughtHatH" = as.integer(annot.DA$gene_id %in% C.H.list),
                                 "DroughtHatF" = as.integer(annot.DA$gene_id %in% C.F.list)
)
#Merge
annot.DA <- cbind(annot.DA, DA)
#Remove empty rows
annot.DA <- annot.DA[!rowSums(annot.DA[,34:48])==0,] # 317997 obs. by 48 variables
#Save
saveRDS(annot.DA, file = here("data","intermediate","annot.DA.RDS"))

##Genus level
#Summary at the genus level
tax.genus.DA.summary <- annot.DA %>%
  group_by(tax_genus) %>%
  summarise(across(DroughtSEatT:DroughtHatF, ~ sum(.x, na.rm = TRUE)))

#Convert to relative abundance
tax.genus.DA.summary[,2:16] <- data.frame(t(apply(tax.genus.DA.summary[,2:16], 1, "/", colSums(tax.genus.DA.summary[,2:16]))))
colSums(tax.genus.DA.summary[,2:16]) #Should all be ones
#Keep only above 0.25% on average across all samples
tax.genus.DA.summary.top <- tax.genus.DA.summary[rowMeans(tax.genus.DA.summary[,2:16])>0.0025, ] 
#Prepare for ggplot
tax.genus.DA.summary.t <- data.frame(t(tax.genus.DA.summary.top))
colnames(tax.genus.DA.summary.t) <- tax.genus.DA.summary.t[1,]
tax.genus.DA.summary.t <- tax.genus.DA.summary.t[-1,]
tax.genus.DA.summary.t$GrowthStage <- rep(c("Tillering", "Stem Elongation", "Booting", "Heading", "Flowering"),3)
tax.genus.DA.summary.t$Treatment <- c(rep("Drought at Stem Elongation",5), rep("Drought at Booting",5), rep("Drought at Heading", 5))
tax.genus.DA.summary.t <- tax.genus.DA.summary.t[,-8] #Remove NULL
tax.genus.DA.long <- gather(tax.genus.DA.summary.t, Genus, relabund, 1:14) #transform in long format for ggplot
tax.genus.DA.long$GrowthStage <- factor(tax.genus.DA.long$GrowthStage, c("Tillering", "Stem Elongation", "Booting", "Heading", "Flowering"))#Reorder manually the growth stages
tax.genus.DA.long$Treatment <- factor(tax.genus.DA.long$Treatment, c("Drought at Stem Elongation", "Drought at Booting", "Drought at Heading"))#Reorder manually the growth stages
tax.genus.DA.long$relabund <- as.numeric(tax.genus.DA.long$relabund)#Make sure it's numeric

#Plot
palette(c(brewer.pal(n = 9, name = "Set1"),"lightgrey", "black", "darkred", "darkblue", "darkgreen", "purple4", "darkgrey", "white"))
stack.genus.DA <- ggplot(tax.genus.DA.long, aes(fill = Genus, y = relabund, x = GrowthStage)) + 
  geom_bar( stat = "summary", fun ="mean", position = "stack") +
  ylab("Fraction of DA transcripts") + 
  scale_fill_manual(values = palette(), guide = guide_legend(label.theme = element_text(face = "italic", size = 8))) +
  theme_bw() +
  scale_y_continuous(limits = c(0,0.130), expand = c(0,0)) +
  scale_x_discrete(name = "Growth Stage", labels = c("Tillering", "Stem Elongation", "Booting", "Heading", "Flowering")) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))+
  facet_wrap(vars(Treatment))
stack.genus.DA

##COG_function level
#Summary at the function level
tax.fun.DA.summary <- annot.DA %>%
  group_by(cog_function) %>%
  summarise(across(DroughtSEatT:DroughtHatF, ~ sum(.x, na.rm = TRUE)))

#Convert to relative abundance
tax.fun.DA.summary[,2:16] <- data.frame(t(apply(tax.fun.DA.summary[,2:16], 1, "/", colSums(tax.fun.DA.summary[,2:16]))))
colSums(tax.fun.DA.summary[,2:16]) #Should all be ones
#Keep only above 0.25% on average across all samples
tax.fun.DA.summary.top <- tax.fun.DA.summary[rowMeans(tax.fun.DA.summary[,2:16])>0.0025, ] 
#Prepare for ggplot
tax.fun.DA.summary.t <- data.frame(t(tax.fun.DA.summary.top))
colnames(tax.fun.DA.summary.t) <- tax.fun.DA.summary.t[1,]
tax.fun.DA.summary.t <- tax.fun.DA.summary.t[-1,]
tax.fun.DA.summary.t$GrowthStage <- rep(c("Tillering", "Stem Elongation", "Booting", "Heading", "Flowering"),3)
tax.fun.DA.summary.t$Treatment <- c(rep("Drought at Stem Elongation",5), rep("Drought at Booting",5), rep("Drought at Heading", 5))
tax.fun.DA.summary.t <- tax.fun.DA.summary.t[,-12] #Remove NULL
tax.fun.DA.long <- gather(tax.fun.DA.summary.t, fun, relabund, 1:21) #transform in long format for ggplot
tax.fun.DA.long$GrowthStage <- factor(tax.fun.DA.long$GrowthStage, c("Tillering", "Stem Elongation", "Booting", "Heading", "Flowering"))#Reorder manually the growth stages
tax.fun.DA.long$Treatment <- factor(tax.fun.DA.long$Treatment, c("Drought at Stem Elongation", "Drought at Booting", "Drought at Heading"))#Reorder manually the growth stages
tax.fun.DA.long$relabund <- as.numeric(tax.fun.DA.long$relabund)#Make sure it's numeric

#Plot
palette(c(brewer.pal(n = 9, name = "Set1"),"lightgrey", "black", "darkred", "darkblue", "darkgreen", "purple4", "grey33", "skyblue", "cyan", "magenta", "brown4", "yellow4"))
stack.fun.DA <- ggplot(tax.fun.DA.long, aes(fill = fun, y = relabund, x = GrowthStage)) + 
  geom_bar( stat = "summary", fun ="mean", position = "stack") +
  ylab("Fraction of DA transcripts") + 
  scale_fill_manual(values = palette(), guide = guide_legend(label.theme = element_text(size = 8), ncol = 1, title = "COG function")) +
  theme_bw() +
  scale_y_continuous(limits = c(0,0.150), expand = c(0,0)) +
  scale_x_discrete(name = "Growth Stage", labels = c("Tillering", "Stem Elongation", "Booting", "Heading", "Flowering")) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))+
  facet_wrap(vars(Treatment))
stack.fun.DA

###Stack bar charts for the DA transcripts (stage vs. previous, 4 growth stage pairs x 4 treatments = 16)
#Create DA transcripts lists
TSE.A.list <- readRDS(here("data", "intermediate", "DE.results.TvsSE.A.RDS"))$DEfound
#28,296 elements
TSE.B.list<- readRDS(here("data", "intermediate", "DE.results.TvsSE.B.RDS"))$DEfound
#21,554 elements
TSE.C.list<- readRDS(here("data", "intermediate", "DE.results.TvsSE.C.RDS"))$DEfound
#22,991 elements
TSE.D.list<- readRDS(here("data", "intermediate", "DE.results.TvsSE.D.RDS"))$DEfound
#22,633 elements
SEB.A.list <- readRDS(here("data", "intermediate", "DE.results.SEvsB.A.RDS"))$DEfound
#273,969 elements
SEB.B.list<- readRDS(here("data", "intermediate", "DE.results.SEvsB.B.RDS"))$DEfound
#68,720 elements
SEB.C.list<- readRDS(here("data", "intermediate", "DE.results.SEvsB.C.RDS"))$DEfound
#29,475 elements
SEB.D.list<- readRDS(here("data", "intermediate", "DE.results.SEvsB.D.RDS"))$DEfound
#30,286 elements
BH.A.list <- readRDS(here("data", "intermediate", "DE.results.BvsH.A.RDS"))$DEfound
#287,091 elements
BH.B.list<- readRDS(here("data", "intermediate", "DE.results.HvsB.B.RDS"))$DEfound
#49,704 elements
BH.C.list<- readRDS(here("data", "intermediate", "DE.results.BvsH.C.RDS"))$DEfound
#62,413 elements
BH.D.list<- readRDS(here("data", "intermediate", "DE.results.HvsB.D.RDS"))$DEfound
#18,573 elements
HF.A.list <- readRDS(here("data", "intermediate", "DE.results.HvsF.A.RDS"))$DEfound
#150,083 elements
HF.B.list<- readRDS(here("data", "intermediate", "DE.results.HvsF.B.RDS"))$DEfound
#51,672 elements
HF.C.list<- readRDS(here("data", "intermediate", "DE.results.HvsF.C.RDS"))$DEfound
#622,302 elements
HF.D.list<- readRDS(here("data", "intermediate", "DE.results.HvsF.D.RDS"))$DEfound
#85,833 elements

#Create a data frame
annot <- readRDS(here("data", "intermediate", "annot.RDS")) #14,709,097 obs. of 33 variables
DA.stage  <- data.frame("TvsSE.DroughtSE" = as.integer(annot$gene_id %in% TSE.A.list),
                  "TvsSE.DroughtB" = as.integer(annot$gene_id %in% TSE.B.list),
                  "TvsSE.DroughtH" = as.integer(annot$gene_id %in% TSE.C.list),
                  "TvsSE.CTRL" = as.integer(annot$gene_id %in% TSE.D.list),
                  "SEvsB.DroughtSE" = as.integer(annot$gene_id %in% SEB.A.list),
                  "SEvsB.DroughtB" = as.integer(annot$gene_id %in% SEB.B.list),
                  "SEvsB.DroughtH" = as.integer(annot$gene_id %in% SEB.C.list),
                  "SEvsB.CTRL" = as.integer(annot$gene_id %in% SEB.D.list),
                  "BvsH.DroughtSE" = as.integer(annot$gene_id %in% BH.A.list),
                  "BvsH.DroughtB" = as.integer(annot$gene_id %in% BH.B.list),
                  "BvsH.DroughtH" = as.integer(annot$gene_id %in% BH.C.list),
                  "BvsH.CTRL" = as.integer(annot$gene_id %in% BH.D.list),
                  "HvsF.DroughtSE" = as.integer(annot$gene_id %in% HF.A.list),
                  "HvsF.DroughtB" = as.integer(annot$gene_id %in% HF.B.list),
                  "HvsF.DroughtH" = as.integer(annot$gene_id %in% HF.C.list),
                  "HvsF.CTRL" = as.integer(annot$gene_id %in% HF.D.list)
)
#Merge
annot.DA.stage <- cbind(annot, DA.stage)
rm(annot)
rm(DA.stage)
#Remove empty rows
annot.DA.stage <- annot.DA.stage[!rowSums(annot.DA.stage[,34:49])==0,] #1,255,985  obs. by 49 variables
#Save
saveRDS(annot.DA.stage, file = here("data","intermediate","annot.DA.stage.RDS"))

##Genus level
#Summary at the genus level
tax.genus.DA.stage.summary <- annot.DA.stage %>%
  group_by(tax_genus) %>%
  summarise(across(TvsSE.DroughtSE:HvsF.CTRL, ~ sum(.x, na.rm = TRUE)))

#Convert to relative abundance
tax.genus.DA.stage.summary[,2:17] <- data.frame(t(apply(tax.genus.DA.stage.summary[,2:17], 1, "/", colSums(tax.genus.DA.stage.summary[,2:17]))))
colSums(tax.genus.DA.stage.summary[,2:17]) #Should all be ones
#Keep only above 0.25% on average across all samples
tax.genus.DA.stage.summary.top <- tax.genus.DA.stage.summary[rowMeans(tax.genus.DA.stage.summary[,2:17])>0.0025, ] 
#Prepare for ggplot
tax.genus.DA.stage.summary.t <- data.frame(t(tax.genus.DA.stage.summary.top))
colnames(tax.genus.DA.stage.summary.t) <- tax.genus.DA.stage.summary.t[1,]
tax.genus.DA.stage.summary.t <- tax.genus.DA.stage.summary.t[-1,]
tax.genus.DA.stage.summary.t$Treatment <- rep(c("Drought at Stem Elongation", "Drought at Booting", "Drought at Heading", "Control"),4)
tax.genus.DA.stage.summary.t$GrowthStage <- c(rep("TvsSE",4), rep("SEvsB",4), rep("BvsH", 4), rep("HvsF",4))
tax.genus.DA.stage.summary.t <- tax.genus.DA.stage.summary.t[,-8] #Remove NULL
tax.genus.DA.stage.long <- gather(tax.genus.DA.stage.summary.t, Genus, relabund, 1:14) #transform in long format for ggplot
tax.genus.DA.stage.long$GrowthStage <- factor(tax.genus.DA.stage.long$GrowthStage, c("TvsSE", "SEvsB", "BvsH", "HvsF"))#Reorder manually the growth stages
tax.genus.DA.stage.long$Treatment <- factor(tax.genus.DA.stage.long$Treatment, c("Drought at Stem Elongation", "Drought at Booting", "Drought at Heading", "Control"))#Reorder manually the growth stages
tax.genus.DA.stage.long$relabund <- as.numeric(tax.genus.DA.stage.long$relabund)#Make sure it's numeric

#Plot
palette(c(brewer.pal(n = 9, name = "Set1"),"lightgrey", "black", "darkred", "darkblue", "darkgreen", "purple4", "darkgrey", "white"))
stack.genus.stage.DA <- ggplot(tax.genus.DA.stage.long, aes(fill = Genus, y = relabund, x = Treatment)) + 
  geom_bar( stat = "summary", fun ="mean", position = "stack") +
  ylab("Fraction of reads") + 
  scale_fill_manual(values = palette(), guide = guide_legend(label.theme = element_text(face = "italic", size = 8))) +
  theme_bw() +
  scale_y_continuous(limits = c(0,0.135), expand = c(0,0)) +
  scale_x_discrete(name = "Treatment", labels = c("Drought at SE", "Drought at B", "Drought at H", "Control")) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))+
  facet_wrap(vars(GrowthStage))
stack.genus.stage.DA

##COG_function level
#Summary at the fun level
tax.fun.DA.stage.summary <- annot.DA.stage %>%
  group_by(cog_function) %>%
  summarise(across(TvsSE.DroughtSE:HvsF.CTRL, ~ sum(.x, na.rm = TRUE)))

#Convert to relative abundance
tax.fun.DA.stage.summary[,2:17] <- data.frame(t(apply(tax.fun.DA.stage.summary[,2:17], 1, "/", colSums(tax.fun.DA.stage.summary[,2:17]))))
colSums(tax.fun.DA.stage.summary[,2:17]) #Should all be ones
#Keep only above 0.5% on average across all samples
tax.fun.DA.stage.summary.top <- tax.fun.DA.stage.summary[rowMeans(tax.fun.DA.stage.summary[,2:17])>0.005, ] 
#Prepare for ggplot
tax.fun.DA.stage.summary.t <- data.frame(t(tax.fun.DA.stage.summary.top))
colnames(tax.fun.DA.stage.summary.t) <- tax.fun.DA.stage.summary.t[1,]
tax.fun.DA.stage.summary.t <- tax.fun.DA.stage.summary.t[-1,]
tax.fun.DA.stage.summary.t$Treatment <- rep(c("Drought at Stem Elongation", "Drought at Booting", "Drought at Heading", "Control"),4)
tax.fun.DA.stage.summary.t$GrowthStage <- c(rep("TvsSE",4), rep("SEvsB",4), rep("BvsH", 4), rep("HvsF",4))
tax.fun.DA.stage.summary.t <- tax.fun.DA.stage.summary.t[,-5] #Remove NULL
tax.fun.DA.stage.long <- gather(tax.fun.DA.stage.summary.t, fun, relabund, 1:9) #transform in long format for ggplot
tax.fun.DA.stage.long$GrowthStage <- factor(tax.fun.DA.stage.long$GrowthStage, c("TvsSE", "SEvsB", "BvsH", "HvsF"))#Reorder manually the growth stages
tax.fun.DA.stage.long$Treatment <- factor(tax.fun.DA.stage.long$Treatment, c("Drought at Stem Elongation", "Drought at Booting", "Drought at Heading", "Control"))#Reorder manually the growth stages
tax.fun.DA.stage.long$relabund <- as.numeric(tax.fun.DA.stage.long$relabund)#Make sure it's numeric

#Plot
palette(c(brewer.pal(n = 9, name = "Set1"),"lightgrey", "black", "darkred", "darkblue", "darkgreen", "purple4", "darkgrey", "white"))
stack.fun.stage.DA <- ggplot(tax.fun.DA.stage.long, aes(fill = fun, y = relabund, x = Treatment)) + 
  geom_bar( stat = "summary", fun ="mean", position = "stack") +
  ylab("Fraction of reads") + 
  scale_fill_manual(values = palette(), guide = guide_legend(label.theme = element_text(size = 8), title = "COG function")) +
  theme_bw() +
  scale_y_continuous(limits = c(0,0.1), expand = c(0,0)) +
  scale_x_discrete(name = "Treatment", labels = c("Drought at SE", "Drought at B", "Drought at H", "Control")) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))+
  facet_wrap(vars(GrowthStage))
stack.fun.stage.DA
