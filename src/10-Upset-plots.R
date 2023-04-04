#Get shared and unique transcripts across different treatments and then upset plot
#Do only for ALL transcripts
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

#Get genes and annotation when majority
shared.maj <- annot.DA[rowSums(annot.DA[,c(35,36,41,42,47,48)])>=3,] #Across all: 800 transcripts

shared.maj.1 <- annot.DA[rowSums(annot.DA[,c(35,41,47)])==3,] #At same stage: 183 transcripts
shared.maj.2<- annot.DA[rowSums(annot.DA[,c(36,42,48)])==3,] #After 1 stage: 3 transcripts



