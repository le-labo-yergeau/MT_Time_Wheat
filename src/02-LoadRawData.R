##Import data and create intermediate objects to be loaded in the other scripts
#When done with this script, restart the project without saving workspace to free memory
#Import mapping file
map <- read.table(file = here("data", "raw", "mapping_file.tsv"), row.names = 1, header = T, sep = "\t", comment.char = "") #120 obs in 5 var
saveRDS(map, file = here("data", "intermediate", "map.RDS"))
#Create lists 
list.samples.T.A <- row.names(map[map$growthstage=="tillering" & map$treatment2=="A",])
list.samples.T.B <- row.names(map[map$growthstage=="tillering" & map$treatment2=="B",])
list.samples.T.C <- row.names(map[map$growthstage=="tillering" & map$treatment2=="C",])
list.samples.T.D <- row.names(map[map$growthstage=="tillering" & map$treatment2=="D",])
list.samples.B.A <- row.names(map[map$growthstage=="booting" & map$treatment2=="A",])
list.samples.B.B <- row.names(map[map$growthstage=="booting" & map$treatment2=="B",])
list.samples.B.C <- row.names(map[map$growthstage=="booting" & map$treatment2=="C",])
list.samples.B.D <- row.names(map[map$growthstage=="booting" & map$treatment2=="D",])
list.samples.SE.A <- row.names(map[map$growthstage=="stemelongation" & map$treatment2=="A",])
list.samples.SE.B <- row.names(map[map$growthstage=="stemelongation" & map$treatment2=="B",])
list.samples.SE.C <- row.names(map[map$growthstage=="stemelongation" & map$treatment2=="C",])
list.samples.SE.D <- row.names(map[map$growthstage=="stemelongation" & map$treatment2=="D",])
list.samples.H.A <- row.names(map[map$growthstage=="heading" & map$treatment2=="A",])
list.samples.H.B <- row.names(map[map$growthstage=="heading" & map$treatment2=="B",])
list.samples.H.C <- row.names(map[map$growthstage=="heading" & map$treatment2=="C",])
list.samples.H.D <- row.names(map[map$growthstage=="heading" & map$treatment2=="D",])
list.samples.F.A <- row.names(map[map$growthstage=="flowering" & map$treatment2=="A",])
list.samples.F.B <- row.names(map[map$growthstage=="flowering" & map$treatment2=="B",])
list.samples.F.C <- row.names(map[map$growthstage=="flowering" & map$treatment2=="C",])
list.samples.F.D <- row.names(map[map$growthstage=="flowering" & map$treatment2=="D",])

#Import annotations
annot <- fread(file = here("data", "raw", "annotations.tsv"), header = T, sep = "\t", quote = "") #14,709,098 obs of 33 variables
annot <- annot[-1,] #First row weird and does not match with MT file (14,709,098 obs): remove.
#Get gene ID for bacteria and fungi
list.genes.bact <- annot[annot$tax_kingdom == "k__Bacteria",2]
list.genes.fungi <- annot[grep("mycota", annot$tax_phylum, ignore.case = TRUE),2]
#save intermediate files
saveRDS(list.genes.bact, file = here("data", "intermediate", "list.genes.bact.RDS"))
saveRDS(list.genes.fungi, file = here("data", "intermediate", "list.genes.fungi.RDS"))
#Save and then remove large objects
saveRDS(annot, file = here("data", "intermediate", "annot.RDS"))
rm(annot)

#Import MT gene table --> # 14,709,097 obs in 121 vars
MT <- data.frame(fread(file = here("data", "raw", "merged_gene_abundance.tsv"), header = T,  sep = "\t"), row.names = 1) 
colnames(MT) <- gsub("X", "", colnames(MT)) 
#Create subsets growth stage x treatment for DE analyses and save as .RDS. Should all be 14709097 obs of 6 variables
MT.T.A <- MT[,colnames(MT) %in% list.samples.T.A]
saveRDS(MT.T.A, file = here("data", "intermediate", "MT.T.A.RDS"))
rm(MT.T.A)
MT.T.B <- MT[,colnames(MT) %in% list.samples.T.B]
saveRDS(MT.T.B, file = here("data", "intermediate", "MT.T.B.RDS"))
rm(MT.T.B)
MT.T.C <- MT[,colnames(MT) %in% list.samples.T.C]
saveRDS(MT.T.C, file = here("data", "intermediate", "MT.T.C.RDS"))
rm(MT.T.C)
MT.T.D <- MT[,colnames(MT) %in% list.samples.T.D]
saveRDS(MT.T.D, file = here("data", "intermediate", "MT.T.D.RDS"))
rm(MT.T.D)
MT.B.A <- MT[,colnames(MT) %in% list.samples.B.A]
saveRDS(MT.B.A, file = here("data", "intermediate", "MT.B.A.RDS"))
rm(MT.B.A)
MT.B.B <- MT[,colnames(MT) %in% list.samples.B.B]
saveRDS(MT.B.B, file = here("data", "intermediate", "MT.B.B.RDS"))
rm(MT.B.B)
MT.B.C <- MT[,colnames(MT) %in% list.samples.B.C]
saveRDS(MT.B.C, file = here("data", "intermediate", "MT.B.C.RDS"))
rm(MT.B.C)
MT.B.D <- MT[,colnames(MT) %in% list.samples.B.D]
saveRDS(MT.B.D, file = here("data", "intermediate", "MT.B.D.RDS"))
rm(MT.B.D)
MT.SE.A <- MT[,colnames(MT) %in% list.samples.SE.A]
saveRDS(MT.SE.A, file = here("data", "intermediate", "MT.SE.A.RDS"))
rm(MT.SE.A)
MT.SE.B <- MT[,colnames(MT) %in% list.samples.SE.B]
saveRDS(MT.SE.B, file = here("data", "intermediate", "MT.SE.B.RDS"))
rm(MT.SE.B)
MT.SE.C <- MT[,colnames(MT) %in% list.samples.SE.C]
saveRDS(MT.SE.C, file = here("data", "intermediate", "MT.SE.C.RDS"))
rm(MT.SE.C)
MT.SE.D <- MT[,colnames(MT) %in% list.samples.SE.D]
saveRDS(MT.SE.D, file = here("data", "intermediate", "MT.SE.D.RDS"))
rm(MT.SE.D)
MT.H.A <- MT[,colnames(MT) %in% list.samples.H.A]
saveRDS(MT.H.A, file = here("data", "intermediate", "MT.H.A.RDS"))
rm(MT.H.A)
MT.H.B <- MT[,colnames(MT) %in% list.samples.H.B]
saveRDS(MT.H.B, file = here("data", "intermediate", "MT.H.B.RDS"))
rm(MT.H.B)
MT.H.C <- MT[,colnames(MT) %in% list.samples.H.C]
saveRDS(MT.H.C, file = here("data", "intermediate", "MT.H.C.RDS"))
rm(MT.H.C)
MT.H.D <- MT[,colnames(MT) %in% list.samples.H.D]
saveRDS(MT.H.D, file = here("data", "intermediate", "MT.H.D.RDS"))
rm(MT.H.D)
MT.F.A <- MT[,colnames(MT) %in% list.samples.F.A]
saveRDS(MT.F.A, file = here("data", "intermediate", "MT.F.A.RDS"))
rm(MT.F.A)
MT.F.B <- MT[,colnames(MT) %in% list.samples.F.B]
saveRDS(MT.F.B, file = here("data", "intermediate", "MT.F.B.RDS"))
rm(MT.F.B)
MT.F.C <- MT[,colnames(MT) %in% list.samples.F.C]
saveRDS(MT.F.C, file = here("data", "intermediate", "MT.F.C.RDS"))
rm(MT.F.C)
MT.F.D <- MT[,colnames(MT) %in% list.samples.F.D]
saveRDS(MT.F.D, file = here("data", "intermediate", "MT.F.D.RDS"))
rm(MT.F.D)

#Create subset MT datasets
#Bacteria -->10,560,477 obs of 120 variables
MT.bact <- MT[row.names(MT) %in% list.genes.bact$gene_id,]
saveRDS(MT.bact, file = here("data", "intermediate", "MT.bact.RDS"))
#Fungi -->27,776 obs of 120 variables
MT.fungi <- MT[row.names(MT) %in% list.genes.fungi$gene_id, ]
saveRDS(MT.fungi, file = here("data", "intermediate", "MT.fungi.RDS"))
#Save and then remove large objects
saveRDS(MT, file = here("data", "intermediate", "MT.RDS"))
rm(MT)

#Same subsetting as above but for bacteria
#Create subsets growth stage x treatment for DE analyses and save as .RDS 
MT.bact.T.A <- MT.bact[,colnames(MT.bact) %in% list.samples.T.A]
saveRDS(MT.bact.T.A, file = here("data", "intermediate", "MT.bact.T.A.RDS"))
rm(MT.bact.T.A)
MT.bact.T.B <- MT.bact[,colnames(MT.bact) %in% list.samples.T.B]
saveRDS(MT.bact.T.B, file = here("data", "intermediate", "MT.bact.T.B.RDS"))
rm(MT.bact.T.B)
MT.bact.T.C <- MT.bact[,colnames(MT.bact) %in% list.samples.T.C]
saveRDS(MT.bact.T.C, file = here("data", "intermediate", "MT.bact.T.C.RDS"))
rm(MT.bact.T.C)
MT.bact.T.D <- MT.bact[,colnames(MT.bact) %in% list.samples.T.D]
saveRDS(MT.bact.T.D, file = here("data", "intermediate", "MT.bact.T.D.RDS"))
rm(MT.bact.T.D)
MT.bact.B.A <- MT.bact[,colnames(MT.bact) %in% list.samples.B.A]
saveRDS(MT.bact.B.A, file = here("data", "intermediate", "MT.bact.B.A.RDS"))
rm(MT.bact.B.A)
MT.bact.B.B <- MT.bact[,colnames(MT.bact) %in% list.samples.B.B]
saveRDS(MT.bact.B.B, file = here("data", "intermediate", "MT.bact.B.B.RDS"))
rm(MT.bact.B.B)
MT.bact.B.C <- MT.bact[,colnames(MT.bact) %in% list.samples.B.C]
saveRDS(MT.bact.B.C, file = here("data", "intermediate", "MT.bact.B.C.RDS"))
rm(MT.bact.B.C)
MT.bact.B.D <- MT.bact[,colnames(MT.bact) %in% list.samples.B.D]
saveRDS(MT.bact.B.D, file = here("data", "intermediate", "MT.bact.B.D.RDS"))
rm(MT.bact.B.D)
MT.bact.SE.A <- MT.bact[,colnames(MT.bact) %in% list.samples.SE.A]
saveRDS(MT.bact.SE.A, file = here("data", "intermediate", "MT.bact.SE.A.RDS"))
rm(MT.bact.SE.A)
MT.bact.SE.B <- MT.bact[,colnames(MT.bact) %in% list.samples.SE.B]
saveRDS(MT.bact.SE.B, file = here("data", "intermediate", "MT.bact.SE.B.RDS"))
rm(MT.bact.SE.B)
MT.bact.SE.C <- MT.bact[,colnames(MT.bact) %in% list.samples.SE.C]
saveRDS(MT.bact.SE.C, file = here("data", "intermediate", "MT.bact.SE.C.RDS"))
rm(MT.bact.SE.C)
MT.bact.SE.D <- MT.bact[,colnames(MT.bact) %in% list.samples.SE.D]
saveRDS(MT.bact.SE.D, file = here("data", "intermediate", "MT.bact.SE.D.RDS"))
rm(MT.bact.SE.D)
MT.bact.H.A <- MT.bact[,colnames(MT.bact) %in% list.samples.H.A]
saveRDS(MT.bact.H.A, file = here("data", "intermediate", "MT.bact.H.A.RDS"))
rm(MT.bact.H.A)
MT.bact.H.B <- MT.bact[,colnames(MT.bact) %in% list.samples.H.B]
saveRDS(MT.bact.H.B, file = here("data", "intermediate", "MT.bact.H.B.RDS"))
rm(MT.bact.H.B)
MT.bact.H.C <- MT.bact[,colnames(MT.bact) %in% list.samples.H.C]
saveRDS(MT.bact.H.C, file = here("data", "intermediate", "MT.bact.H.C.RDS"))
rm(MT.bact.H.C)
MT.bact.H.D <- MT.bact[,colnames(MT.bact) %in% list.samples.H.D]
saveRDS(MT.bact.H.D, file = here("data", "intermediate", "MT.bact.H.D.RDS"))
rm(MT.bact.H.D)
MT.bact.F.A <- MT.bact[,colnames(MT.bact) %in% list.samples.F.A]
saveRDS(MT.bact.F.A, file = here("data", "intermediate", "MT.bact.F.A.RDS"))
rm(MT.bact.F.A)
MT.bact.F.B <- MT.bact[,colnames(MT.bact) %in% list.samples.F.B]
saveRDS(MT.bact.F.B, file = here("data", "intermediate", "MT.bact.F.B.RDS"))
rm(MT.bact.F.B)
MT.bact.F.C <- MT.bact[,colnames(MT.bact) %in% list.samples.F.C]
saveRDS(MT.bact.F.C, file = here("data", "intermediate", "MT.bact.F.C.RDS"))
rm(MT.bact.F.C)
MT.bact.F.D <- MT.bact[,colnames(MT.bact) %in% list.samples.F.D]
saveRDS(MT.bact.F.D, file = here("data", "intermediate", "MT.bact.F.D.RDS"))
rm(MT.bact.F.D)

#Same subsetting as above but for fungi
#Create subsets growth stage x treatment for DE analyses and save as .RDS 
MT.fungi.T.A <- MT.fungi[,colnames(MT.fungi) %in% list.samples.T.A]
saveRDS(MT.fungi.T.A, file = here("data", "intermediate", "MT.fungi.T.A.RDS"))
rm(MT.fungi.T.A)
MT.fungi.T.B <- MT.fungi[,colnames(MT.fungi) %in% list.samples.T.B]
saveRDS(MT.fungi.T.B, file = here("data", "intermediate", "MT.fungi.T.B.RDS"))
rm(MT.fungi.T.B)
MT.fungi.T.C <- MT.fungi[,colnames(MT.fungi) %in% list.samples.T.C]
saveRDS(MT.fungi.T.C, file = here("data", "intermediate", "MT.fungi.T.C.RDS"))
rm(MT.fungi.T.C)
MT.fungi.T.D <- MT.fungi[,colnames(MT.fungi) %in% list.samples.T.D]
saveRDS(MT.fungi.T.D, file = here("data", "intermediate", "MT.fungi.T.D.RDS"))
rm(MT.fungi.T.D)
MT.fungi.B.A <- MT.fungi[,colnames(MT.fungi) %in% list.samples.B.A]
saveRDS(MT.fungi.B.A, file = here("data", "intermediate", "MT.fungi.B.A.RDS"))
rm(MT.fungi.B.A)
MT.fungi.B.B <- MT.fungi[,colnames(MT.fungi) %in% list.samples.B.B]
saveRDS(MT.fungi.B.B, file = here("data", "intermediate", "MT.fungi.B.B.RDS"))
rm(MT.fungi.B.B)
MT.fungi.B.C <- MT.fungi[,colnames(MT.fungi) %in% list.samples.B.C]
saveRDS(MT.fungi.B.C, file = here("data", "intermediate", "MT.fungi.B.C.RDS"))
rm(MT.fungi.B.C)
MT.fungi.B.D <- MT.fungi[,colnames(MT.fungi) %in% list.samples.B.D]
saveRDS(MT.fungi.B.D, file = here("data", "intermediate", "MT.fungi.B.D.RDS"))
rm(MT.fungi.B.D)
MT.fungi.SE.A <- MT.fungi[,colnames(MT.fungi) %in% list.samples.SE.A]
saveRDS(MT.fungi.SE.A, file = here("data", "intermediate", "MT.fungi.SE.A.RDS"))
rm(MT.fungi.SE.A)
MT.fungi.SE.B <- MT.fungi[,colnames(MT.fungi) %in% list.samples.SE.B]
saveRDS(MT.fungi.SE.B, file = here("data", "intermediate", "MT.fungi.SE.B.RDS"))
rm(MT.fungi.SE.B)
MT.fungi.SE.C <- MT.fungi[,colnames(MT.fungi) %in% list.samples.SE.C]
saveRDS(MT.fungi.SE.C, file = here("data", "intermediate", "MT.fungi.SE.C.RDS"))
rm(MT.fungi.SE.C)
MT.fungi.SE.D <- MT.fungi[,colnames(MT.fungi) %in% list.samples.SE.D]
saveRDS(MT.fungi.SE.D, file = here("data", "intermediate", "MT.fungi.SE.D.RDS"))
rm(MT.fungi.SE.D)
MT.fungi.H.A <- MT.fungi[,colnames(MT.fungi) %in% list.samples.H.A]
saveRDS(MT.fungi.H.A, file = here("data", "intermediate", "MT.fungi.H.A.RDS"))
rm(MT.fungi.H.A)
MT.fungi.H.B <- MT.fungi[,colnames(MT.fungi) %in% list.samples.H.B]
saveRDS(MT.fungi.H.B, file = here("data", "intermediate", "MT.fungi.H.B.RDS"))
rm(MT.fungi.H.B)
MT.fungi.H.C <- MT.fungi[,colnames(MT.fungi) %in% list.samples.H.C]
saveRDS(MT.fungi.H.C, file = here("data", "intermediate", "MT.fungi.H.C.RDS"))
rm(MT.fungi.H.C)
MT.fungi.H.D <- MT.fungi[,colnames(MT.fungi) %in% list.samples.H.D]
saveRDS(MT.fungi.H.D, file = here("data", "intermediate", "MT.fungi.H.D.RDS"))
rm(MT.fungi.H.D)
MT.fungi.F.A <- MT.fungi[,colnames(MT.fungi) %in% list.samples.F.A]
saveRDS(MT.fungi.F.A, file = here("data", "intermediate", "MT.fungi.F.A.RDS"))
rm(MT.fungi.F.A)
MT.fungi.F.B <- MT.fungi[,colnames(MT.fungi) %in% list.samples.F.B]
saveRDS(MT.fungi.F.B, file = here("data", "intermediate", "MT.fungi.F.B.RDS"))
rm(MT.fungi.F.B)
MT.fungi.F.C <- MT.fungi[,colnames(MT.fungi) %in% list.samples.F.C]
saveRDS(MT.fungi.F.C, file = here("data", "intermediate", "MT.fungi.F.C.RDS"))
rm(MT.fungi.F.C)
MT.fungi.F.D <- MT.fungi[,colnames(MT.fungi) %in% list.samples.F.D]
saveRDS(MT.fungi.F.D, file = here("data", "intermediate", "MT.fungi.F.D.RDS"))
rm(MT.fungi.F.D)

