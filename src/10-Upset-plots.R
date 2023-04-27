###Get shared and unique transcripts across different treatments and then upset plot
##Do only for ALL transcripts
#Transcripts DA in treatments at stages drought and post-drought: 
#       TreatA: SE,B  treatB: B,H treat C: H,F

#Use data frame from 07-Stack... Contains all 15 treatment x stage combos
annot.DA <- readRDS(file = here("data","intermediate","annot.DA.RDS")) #Empty rows removed: 317997 obs of 48 variables
#focus on DR and RW
annot.DA.DR.RW <- annot.DA[,c(35,36,41,42,47,48)]
annot.DA.DR.RW <- annot.DA.DR.RW[rowSums(annot.DA.DR.RW)>0,] #remove empty rows

#Plot using the ComplexUpset package
upset.plot <-  upset(data = annot.DA.DR.RW,
                     intersect = c("DroughtSEatSE", "DroughtSEatB", "DroughtBatB", "DroughtBatH", "DroughtHatH", "DroughtHatF"), 
                     min_size=100,
                     base_annotations=list('Intersection size'=intersection_size(counts=FALSE)),
                     
                )
upset.plot

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

#Plot using the ComplexUpset package
upset.plot.TSE <- upset(annot.DA.stage[,34:37], intersect = c("TvsSE.DroughtSE", "TvsSE.DroughtB", "TvsSE.DroughtH", "TvsSE.CTRL"))
upset.plot.TSE
upset.plot.SEB <- upset(annot.DA.stage[,38:41], intersect = c("SEvsB.DroughtSE", "SEvsB.DroughtB", "SEvsB.DroughtH", "SEvsB.CTRL"))
upset.plot.SEB
upset.plot.BH <- upset(annot.DA.stage[,42:45], intersect = c("BvsH.DroughtSE", "BvsH.DroughtB", "BvsH.DroughtH", "BvsH.CTRL"))
upset.plot.BH
upset.plot.HF <- upset(annot.DA.stage[,46:49], intersect = c("HvsF.DroughtSE", "HvsF.DroughtB", "HvsF.DroughtH", "HvsF.CTRL"))
upset.plot.HF



