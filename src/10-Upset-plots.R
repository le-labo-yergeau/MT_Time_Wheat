###Get shared and unique transcripts across different treatments and then upset plot
##Do only for ALL transcripts
#Transcripts DA in treatments at stages drought and post-drought: 
#       TreatA: SE,B  treatB: B,H treat C: H,F

#Use data frame from 07-Stack... Contains all 15 treatment x stage combos
annot.DA <- readRDS(file = here("data","intermediate","annot.DA.RDS")) #Empty rows removed: 317997 obs of 48 variables

#Plot using the UpSetR package
upset.plot <- upset(annot.DA[,c(35,36,41,42,47,48)], sets = c("DroughtSEatSE", "DroughtSEatB", "DroughtBatB", "DroughtBatH", "DroughtHatH", "DroughtHatF"), nintersects = NA, keep.order = TRUE)
upset.plot

#Create figure 7
upset.plot
fig7 <- grid.grab(wrap.grobs = TRUE)
ggsave(file = here("output", "figs", "fig7.tiff"), fig7, width = 14, height = 7, units = "in", dpi = 600, compression = "lzw")

#Get genes and annotation when majority, export tables, save object for 11-Heatmap
shared.maj <- annot.DA[rowSums(annot.DA[,c(35,36,41,42,47,48)])>=3,] #Across all: 800 transcripts
write.table(shared.maj, file = here("output", "tables", "shared.DA.all.treatvsctrl.txt"), sep = "\t")
shared.maj.1 <- annot.DA[rowSums(annot.DA[,c(35,41,47)])==3,] #At same stage: 183 transcripts
write.table(shared.maj.1, file = here("output", "tables", "TableS1.txt"), sep = "\t")
saveRDS(shared.maj.1, file = here("data", "intermediate", "shared.maj.1.RDS"))
shared.maj.2<- annot.DA[rowSums(annot.DA[,c(36,42,48)])==3,] #After 1 stage: 3 transcripts
write.table(shared.maj.2, file = here("output", "tables", "shared.DA.next2.treatvsctrl.txt"), sep = "\t")

#Transcripts DA in treatments vs. control
#       Four panels: one per comparison

#Use data frame from 07-Stack... Contains all 16 treatment x stage combos
annot.DA.stage <- readRDS(file = here("data","intermediate","annot.DA.stage.RDS")) #Empty rows removed: 1255985 obs of 49 variables

#Plot using the UpSetR package
upset.plot.TSE <- upset(annot.DA.stage[,34:37], sets = c("TvsSE.DroughtSE", "TvsSE.DroughtB", "TvsSE.DroughtH", "TvsSE.CTRL"), nintersects = NA, keep.order = TRUE)
upset.plot.TSE
upset.plot.SEB <- upset(annot.DA.stage[,38:41], sets = c("SEvsB.DroughtSE", "SEvsB.DroughtB", "SEvsB.DroughtH", "SEvsB.CTRL"), nintersects = NA, keep.order = TRUE)
upset.plot.SEB
upset.plot.BH <- upset(annot.DA.stage[,42:45], sets = c("BvsH.DroughtSE", "BvsH.DroughtB", "BvsH.DroughtH", "BvsH.CTRL"), nintersects = NA, keep.order = TRUE)
upset.plot.BH
upset.plot.HF <- upset(annot.DA.stage[,46:49], sets = c("HvsF.DroughtSE", "HvsF.DroughtB", "HvsF.DroughtH", "HvsF.CTRL"), nintersects = NA, keep.order = TRUE)
upset.plot.HF

#Create figure 7b
upset.plot.TSE
TSE <- grid.grab(wrap.grobs = TRUE)
ggsave(file = here("output", "figs", "fig7TSE.tiff"), TSE, width = 7, height = 3.5, units = "in", dpi = 600, compression = "lzw")
upset.plot.SEB
SEB <- grid.grab(wrap.grobs = TRUE)
ggsave(file = here("output", "figs", "fig7SEB.tiff"), SEB, width = 7, height = 3.5, units = "in", dpi = 600, compression = "lzw")
upset.plot.BH
BH <- grid.grab(wrap.grobs = TRUE)
ggsave(file = here("output", "figs", "fig7BH.tiff"), BH, width = 7, height = 3.5, units = "in", dpi = 600, compression = "lzw")
upset.plot.HF
HF <- grid.grab(wrap.grobs = TRUE)
ggsave(file = here("output", "figs", "fig7HF.tiff"), HF, width = 7, height = 3.5, units = "in", dpi = 600, compression = "lzw")




