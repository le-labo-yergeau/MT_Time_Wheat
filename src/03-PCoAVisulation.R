#Create Bray-Curtis Dissimilarity matrices from relative abundance MT tables -- On the Compute cluster
#load libraries (because cluster)
library(here)
library(vegan)

#Import the objects
MT <- readRDS(file = here("data", "intermediate", "MT.RDS"))
MT.bact <- readRDS(file = here("data", "intermediate", "MT.bact.RDS"))
MT.fungi <- readRDS(file = here("data", "intermediate", "MT.fungi.RDS"))

#Normalise the matrices
MT.rel <- data.frame(t(apply(MT, 1, "/", colSums(MT))))
colnames(MT.rel) <- gsub("X", "", colnames(MT.rel)) 
colSums(MT.rel) #Should all be ones
rm(MT)
MT.bact.rel <- data.frame(t(apply(MT.bact, 1, "/", colSums(MT.bact))))
colnames(MT.bact.rel) <- gsub("X", "", colnames(MT.bact.rel)) 
colSums(MT.bact.rel) #Should all be ones
rm(MT.bact)
MT.fungi.rel <- data.frame(t(apply(MT.fungi, 1, "/", colSums(MT.fungi))))
colnames(MT.fungi.rel) <- gsub("X", "", colnames(MT.fungi.rel)) 
colSums(MT.fungi.rel) #Should all be ones
rm(MT.fungi)

#Save intermediate files
saveRDS(MT.rel, file = here("data", "intermediate", "MT.rel.RDS"))
saveRDS(MT.bact.rel, file = here("data", "intermediate", "MT.bact.rel.RDS"))
saveRDS(MT.fungi.rel, file = here("data", "intermediate", "MT.fungi.rel.RDS"))

#Calculate Bray
bray <- as.matrix(vegdist(t(MT.rel), method = "bray", diag = T, upper = T)) 
bray.bact <- as.matrix(vegdist(t(MT.bact.rel), method = "bray", diag = T, upper = T)) 
bray.fungi <- as.matrix(vegdist(t(MT.fungi.rel), method = "bray", diag = T, upper = T))

#Save intermediate files, exit cluster
saveRDS(bray, file = here("data", "intermediate", "bray.RDS"))
saveRDS(bray.bact, file = here("data", "intermediate", "bray.bact.RDS"))
saveRDS(bray.fungi, file = here("data", "intermediate", "bray.fungi.RDS"))

#Back from cluster, load intermediate files
bray <- readRDS(file = here("data", "intermediate", "bray.RDS"))
bray.bact <- readRDS(file = here("data", "intermediate", "bray.bact.RDS"))
bray.fungi <- readRDS(file = here("data", "intermediate", "bray.fungi.RDS"))
map <- readRDS(file = here("data","intermediate", "map.RDS"))

#Sort mapping
map.s <- map[order(row.names(map)),]

#Sort all
bray.s <- bray[order(row.names(bray)),order(colnames(bray))]
sum(row.names(bray.s) == row.names(map.s))#120
sum(row.names(bray.s) == colnames(bray.s))#120
saveRDS(bray.s,file = here("data", "intermediate", "bray.s.RDS"))

#Sort bacteria
bray.bact.s <- bray.bact[order(row.names(bray.bact)),order(colnames(bray.bact))]
sum(row.names(bray.bact.s) == row.names(map.s))#120
sum(row.names(bray.bact.s) == colnames(bray.bact.s))#120
saveRDS(bray.bact.s,file = here("data", "intermediate", "bray.bact.s.RDS"))

#Sort fungi
bray.fungi.s <- bray.fungi[order(row.names(bray.fungi)),order(colnames(bray.fungi))]
sum(row.names(bray.fungi.s) == row.names(map.s))#120
sum(row.names(bray.fungi.s) == colnames(bray.fungi.s))#120
saveRDS(bray.fungi.s,file = here("data", "intermediate", "bray.fungi.s.RDS"))


##All
pcoa <- cmdscale(sqrt(bray.s))

#Sort
pcoa.s <- pcoa[order(row.names(pcoa)),]
sum(row.names(pcoa.s) == row.names(map.s)) # 120

#create a data frame
pcoa.map <- data.frame(pcoa.s, map.s)
pcoa.map$growthstage <- factor(pcoa.map$growthstage, c("tillering", "stemelongation", "booting", "heading", "flowering" ))#reorder to fit time
pcoa.map$treatment <- factor(pcoa.map$treatment, c("ND", "RW", "DR" ))#reorder


#Plot PCoA
#Treatment1
pcoa.plot.all.1 <- ggplot(data=pcoa.map, aes(x=X1, y=X2, shape=growthstage, colour=treatment)) + 
  geom_point() +
  xlab("PCoA axis 1") + 
  ylab("PCoA axis 2") + 
  theme_bw()+
  scale_color_discrete(name = "Treatment", labels = c("Not disturbed", "Rewetted", "Dry"), type = c("green", "orange","red"))+
  scale_shape_discrete(name = "Growth Stage", labels = c("Tillering", "Stem Elongation", "Booting", "Heading", "Flowering"))
pcoa.plot.all.1

#Treatment2
pcoa.plot.all.2 <- ggplot(data=pcoa.map, aes(x=X1, y=X2, shape=treatment2, colour=growthstage)) + 
  geom_point() +
  xlab("PCoA axis 1") + 
  ylab("PCoA axis 2") + 
  theme_bw()+
  scale_shape_discrete(name = "Treatment", labels = c("Drought at Stem Elongation", "Drought at Booting", "Drought at Heading", "Control"))+
  scale_color_discrete(name = "Growth Stage", labels = c("Tillering", "Stem Elongation", "Booting", "Heading", "Flowering"))
pcoa.plot.all.2

##Fungi
#Pcoa
pcoa.fungi <- cmdscale(sqrt(bray.fungi.s))

#Sort
pcoa.fungi.s <- pcoa.fungi[order(row.names(pcoa.fungi)),]
sum(row.names(pcoa.fungi.s) == row.names(map.s)) # 120

#create a data frame
pcoa.map.fungi <- data.frame(pcoa.fungi.s, map.s)
pcoa.map.fungi$growthstage <- factor(pcoa.map.fungi$growthstage, c("tillering", "stemelongation", "booting", "heading", "flowering" ))#reorder to fit time


#Plot PCoA
pcoa.plot.fungi <- ggplot(data=pcoa.map.fungi, aes(x=X1, y=X2, shape=treatment2, colour=growthstage)) + 
  geom_point() +
  xlab("PCoA axis 1") + 
  ylab("PCoA axis 2") + 
  theme_bw()+
  scale_shape_discrete(name = "Treatment", labels = c("Drought at Stem Elongation", "Drought at Booting", "Drought at Heading", "Control"))+
  scale_color_discrete(name = "Growth Stage", labels = c("Tillering", "Stem Elongation", "Booting", "Heading", "Flowering"))
pcoa.plot.fungi

##Bacteria
#Pcoa
pcoa.bact <- cmdscale(sqrt(bray.bact.s))

#Sort
pcoa.bact.s <- pcoa.bact[order(row.names(pcoa.bact)),]
sum(row.names(pcoa.bact.s) == row.names(map.s)) # 120

#create a data frame
pcoa.map.bact <- data.frame(pcoa.bact.s, map.s)
pcoa.map.bact$growthstage <- factor(pcoa.map.bact$growthstage, c("tillering", "stemelongation", "booting", "heading", "flowering" ))#reorder to fit time


#Plot PCoA
pcoa.plot.bact <- ggplot(data=pcoa.map.bact, aes(x=X1, y=X2, shape=treatment2, colour=growthstage)) + 
  geom_point() +
  xlab("PCoA axis 1") + 
  ylab("PCoA axis 2") + 
  theme_bw()+
  scale_shape_discrete(name = "Treatment", labels = c("Drought at Stem Elongation", "Drought at Booting", "Drought at Heading", "Control"))+
  scale_color_discrete(name = "Growth Stage", labels = c("Tillering", "Stem Elongation", "Booting", "Heading", "Flowering"))
pcoa.plot.bact

