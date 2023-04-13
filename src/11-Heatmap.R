###Heatmaps for shared DA transcripts
##Control vs. treatment analysis
#Load data
MT.rel <- readRDS(here("data","intermediate","MT.rel.RDS"))
shared.maj.1 <- readRDS(here("data","intermediate","shared.maj.1.RDS"))
#Subset MT.rel to the shared DA transcripts
MT.rel.shared1 <- MT.rel[rownames(MT.rel) %in% shared.maj.1$gene_id, ] #183 obs. of 120 variables
row.names(MT.rel.shared1) == shared.maj.1$gene_id#Check
rm(MT.rel)
#Scale
MT.rel.shared1.scale <- scale(t(MT.rel.shared1))
colnames(MT.rel.shared1.scale) <- gsub("gene_id_", "", colnames(MT.rel.shared1.scale))#Remove gene_id
#Create annotation for columns (Kingdom)
tax.shared1 <- data.frame("Kingdom" = shared.maj.1$tax_kingdom)
row.names(tax.shared1) <- gsub("gene_id_", "", shared.maj.1$gene_id)
tax.shared1$Kingdom <- gsub("k__", "", tax.shared1$Kingdom)
#Create annotation for rows (Treatment x Stage)
map <- readRDS(here("data","intermediate", "map.RDS"))
map <- map[order(row.names(map)),] #sort
MT.rel.shared1.scale <- MT.rel.shared1.scale[order(row.names(MT.rel.shared1.scale)),] #sort
row.names(map) == row.names(MT.rel.shared1.scale)#Check
treatment <- data.frame("Treatment" = map$treatment)
row.names(treatment) <- row.names(map)

#heatmap
ann_colors = list(Treatment = c("DR" = "red", "ND" = "green", "RW" = "orange"), 
                  Kingdom = c("Archaea" = "red", "Bacteria" = "blue", "NULL" = "yellow")
                  )
heatmap.shared1 <- pheatmap(MT.rel.shared1.scale, annotation_row = treatment, annotation_col = tax.shared1,
                            cluster_rows = TRUE, fontsize_col = 8, show_rownames = F, annotation_colors = ann_colors
                            )
#Save figure
ggsave(file = here("output", "figs", "fig8.tiff"), heatmap.shared1, width = 14, height = 7, units = "in", dpi = 600, compression = "lzw")
