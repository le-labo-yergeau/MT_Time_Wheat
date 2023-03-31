###DE analyses for all pairs of treatments x stage
#Run on the cluster using DE.sh: from the src folder: sbatch DE.sh
#To work on cluster, need to load packages here and EBseq (packages need to be installed: use interactive mode)
library(here)
library(EBSeq)

#Create function for DE analysis
DE <- function(x,y){
  #Merge the two tables
  xy <- as.matrix(cbind(x,y)) 
  #Remove rows with all zeroes
  xy <- xy[!rowSums(xy) == 0,]
  #Create group file 
  groups <- as.factor(c(rep("X",6),rep("Y",6)))
  #Create size Factors file
  size <- MedianNorm(xy)
  #Run EBTest
  EBtestOUT <- EBTest(xy, Conditions = groups, sizeFactors = size, maxround = 5)
  #Get results
  DE.results <- GetDEResults(EBtestOUT, FDR = 0.05)
  return(DE.results)
}

##All genes
#Tillering
#Compare to control (D)

#A vs D
#Load data
MT.T.A <- readRDS(file = here("data", "intermediate", "MT.T.A.RDS"))
MT.T.D <- readRDS(file = here("data", "intermediate", "MT.T.D.RDS"))
#use the DE function to get results
DE.results.T.AvsD <- DE(MT.T.A,MT.T.D)
saveRDS(DE.results.T.AvsD, file = here("data", "intermediate", "DE.results.T.AvsD.RDS"))
#Get number of DE transcripts
length(DE.results.T.AvsD$DEfound) #17,362
DE.counts <- data.frame(GrowthStage="Tillering", Treatment="A", Subset="All", DEcount=length(DE.results.T.AvsD$DEfound))#create the dataframe
rm(DE.results.T.AvsD)
rm(MT.T.A)

#B vs D
#Load data
MT.T.B <- readRDS(file = here("data", "intermediate", "MT.T.B.RDS"))
#use the DE function to get results
DE.results.T.BvsD <- DE(MT.T.B,MT.T.D)
saveRDS(DE.results.T.BvsD, file = here("data", "intermediate", "DE.results.T.BvsD.RDS"))
#Get number of DE transcripts
length(DE.results.T.BvsD$DEfound) #4,637
DE.counts[nrow(DE.counts)+1,] = c("Tillering", "B", "All", length(DE.results.T.BvsD$DEfound)) #append to DE.counts
rm(DE.results.T.BvsD)
rm(MT.T.B)

#C vs D
#Load data
MT.T.C <- readRDS(file = here("data", "intermediate", "MT.T.C.RDS"))
#use the DE function to get results
DE.results.T.CvsD <- DE(MT.T.C,MT.T.D)
saveRDS(DE.results.T.CvsD, file = here("data", "intermediate", "DE.results.T.CvsD.RDS"))
#Get number of DE transcripts
length(DE.results.T.CvsD$DEfound) #4,927
DE.counts[nrow(DE.counts)+1,] = c("Tillering", "C", "All", length(DE.results.T.CvsD$DEfound)) #append to DE.counts
rm(DE.results.T.CvsD)
rm(MT.T.C)
rm(MT.T.D)

#Booting
#Compare to control (D)

#A vs D
#Load data
MT.B.A <- readRDS(file = here("data", "intermediate", "MT.B.A.RDS"))
MT.B.D <- readRDS(file = here("data", "intermediate", "MT.B.D.RDS"))
#use the DE function to get results
DE.results.B.AvsD <- DE(MT.B.A,MT.B.D)
saveRDS(DE.results.B.AvsD, file = here("data", "intermediate", "DE.results.B.AvsD.RDS"))
#Get number of DE transcripts
length(DE.results.B.AvsD$DEfound) #126,995
DE.counts[nrow(DE.counts)+1,] = c("Booting", "A", "All", length(DE.results.B.AvsD$DEfound)) #append to DE.counts
rm(DE.results.B.AvsD)
rm(MT.B.A)

#B vs D
#Load data
MT.B.B <- readRDS(file = here("data", "intermediate", "MT.B.B.RDS"))
#use the DE function to get results
DE.results.B.BvsD <- DE(MT.B.B,MT.B.D)
saveRDS(DE.results.B.BvsD, file = here("data", "intermediate", "DE.results.B.BvsD.RDS"))
#Get number of DE transcripts
length(DE.results.B.BvsD$DEfound) #59,607
DE.counts[nrow(DE.counts)+1,] = c("Booting", "B", "All", length(DE.results.B.BvsD$DEfound)) #append to DE.counts
rm(DE.results.B.BvsD)
rm(MT.B.B)

#C vs D
#Load data
MT.B.C <- readRDS(file = here("data", "intermediate", "MT.B.C.RDS"))
#use the DE function to get results
DE.results.B.CvsD <- DE(MT.B.C,MT.B.D)
saveRDS(DE.results.B.CvsD, file = here("data", "intermediate", "DE.results.B.CvsD.RDS"))
#Get number of DE transcripts
length(DE.results.B.CvsD$DEfound) #7,754
DE.counts[nrow(DE.counts)+1,] = c("Booting", "C", "All", length(DE.results.B.CvsD$DEfound)) #append to DE.counts
rm(DE.results.B.CvsD)
rm(MT.B.C)
rm(MT.B.D)


#Stem elongation
#Compare to control (D)

#A vs D
#Load data
MT.SE.A <- readRDS(file = here("data", "intermediate", "MT.SE.A.RDS"))
MT.SE.D <- readRDS(file = here("data", "intermediate", "MT.SE.D.RDS"))
#use the DE function to get results
DE.results.SE.AvsD <- DE(MT.SE.A,MT.SE.D)
saveRDS(DE.results.SE.AvsD, file = here("data", "intermediate", "DE.results.SE.AvsD.RDS"))
#Get number of DE transcripts
length(DE.results.SE.AvsD$DEfound) #21,399
DE.counts[nrow(DE.counts)+1,] = c("Stem Elongation", "A", "All", length(DE.results.SE.AvsD$DEfound)) #append to DE.counts
rm(DE.results.SE.AvsD)
rm(MT.SE.A)

#B vs D
#Load data
MT.SE.B <- readRDS(file = here("data", "intermediate", "MT.SE.B.RDS"))
#use the DE function to get results
DE.results.SE.BvsD <- DE(MT.SE.B,MT.SE.D)
saveRDS(DE.results.SE.BvsD, file = here("data", "intermediate", "DE.results.SE.BvsD.RDS"))
#Get number of DE transcripts
length(DE.results.SE.BvsD$DEfound) #5,187
DE.counts[nrow(DE.counts)+1,] = c("Stem Elongation", "B", "All", length(DE.results.SE.BvsD$DEfound)) #append to DE.counts
rm(DE.results.SE.BvsD)
rm(MT.SE.B)

#C vs D
#Load data
MT.SE.C <- readRDS(file = here("data", "intermediate", "MT.SE.C.RDS"))
#use the DE function to get results
DE.results.SE.CvsD <- DE(MT.SE.C,MT.SE.D)
saveRDS(DE.results.SE.CvsD, file = here("data", "intermediate", "DE.results.SE.CvsD.RDS"))
#Get number of DE transcripts
length(DE.results.SE.CvsD$DEfound) #5,651
DE.counts[nrow(DE.counts)+1,] = c("Stem Elongation", "C", "All", length(DE.results.SE.CvsD$DEfound)) #append to DE.counts
rm(DE.results.SE.CvsD)
rm(MT.SE.C)
rm(MT.SE.D)

#Heading
#Compare to control (D)

#A vs D
#Load data
MT.H.A <- readRDS(file = here("data", "intermediate", "MT.H.A.RDS"))
MT.H.D <- readRDS(file = here("data", "intermediate", "MT.H.D.RDS"))
#use the DE function to get results
DE.results.H.AvsD <- DE(MT.H.A,MT.H.D)
saveRDS(DE.results.H.AvsD, file = here("data", "intermediate", "DE.results.H.AvsD.RDS"))
#Get number of DE transcripts
length(DE.results.H.AvsD$DEfound) #31,006
DE.counts[nrow(DE.counts)+1,] = c("Heading", "A", "All", length(DE.results.H.AvsD$DEfound)) #append to DE.counts
rm(DE.results.H.AvsD)
rm(MT.H.A)

#B vs D
#Load data
MT.H.B <- readRDS(file = here("data", "intermediate", "MT.H.B.RDS"))
#use the DE function to get results
DE.results.H.BvsD <- DE(MT.H.B,MT.H.D)
saveRDS(DE.results.H.BvsD, file = here("data", "intermediate", "DE.results.H.BvsD.RDS"))
#Get number of DE transcripts
length(DE.results.H.BvsD$DEfound) #7,636
DE.counts[nrow(DE.counts)+1,] = c("Heading", "B", "All", length(DE.results.H.BvsD$DEfound)) #append to DE.counts
rm(DE.results.H.BvsD)
rm(MT.H.B)

#C vs D
#Load data
MT.H.C <- readRDS(file = here("data", "intermediate", "MT.H.C.RDS"))
#use the DE function to get results
DE.results.H.CvsD <- DE(MT.H.C,MT.H.D)
saveRDS(DE.results.H.CvsD, file = here("data", "intermediate", "DE.results.H.CvsD.RDS"))
#Get number of DE transcripts
length(DE.results.H.CvsD$DEfound) #32,141
DE.counts[nrow(DE.counts)+1,] = c("Heading", "C", "All", length(DE.results.H.CvsD$DEfound)) #append to DE.counts
rm(DE.results.H.CvsD)
rm(MT.H.C)
rm(MT.H.D)

#Flowering
#Compare to control (D)

#A vs D
#Load data
MT.F.A <- readRDS(file = here("data", "intermediate", "MT.F.A.RDS"))
MT.F.D <- readRDS(file = here("data", "intermediate", "MT.F.D.RDS"))
#use the DE function to get results
DE.results.F.AvsD <- DE(MT.F.A,MT.F.D)
saveRDS(DE.results.F.AvsD, file = here("data", "intermediate", "DE.results.F.AvsD.RDS"))
#Get number of DE transcripts
length(DE.results.F.AvsD$DEfound) #8715
DE.counts[nrow(DE.counts)+1,] = c("Flowering", "A", "All", length(DE.results.F.AvsD$DEfound)) #append to DE.counts
rm(DE.results.F.AvsD)
rm(MT.F.A)

#B vs D
#Load data
MT.F.B <- readRDS(file = here("data", "intermediate", "MT.F.B.RDS"))
#use the DE function to get results
DE.results.F.BvsD <- DE(MT.F.B,MT.F.D)
saveRDS(DE.results.F.BvsD, file = here("data", "intermediate", "DE.results.F.BvsD.RDS"))
#Get number of DE transcripts
length(DE.results.F.BvsD$DEfound) #11,932
DE.counts[nrow(DE.counts)+1,] = c("Flowering", "B", "All", length(DE.results.F.BvsD$DEfound)) #append to DE.counts
rm(DE.results.F.BvsD)
rm(MT.F.B)

#C vs D
#Load data
MT.F.C <- readRDS(file = here("data", "intermediate", "MT.F.C.RDS"))
#use the DE function to get results
DE.results.F.CvsD <- DE(MT.F.C,MT.F.D)
saveRDS(DE.results.F.CvsD, file = here("data", "intermediate", "DE.results.F.CvsD.RDS"))
#Get number of DE transcripts
length(DE.results.F.CvsD$DEfound) #15,877
DE.counts[nrow(DE.counts)+1,] = c("Flowering", "C", "All", length(DE.results.F.CvsD$DEfound)) #append to DE.counts
rm(DE.results.F.CvsD)
rm(MT.F.C)
rm(MT.F.D)

##Bacterial genes
#Tillering
#Compare to control (D)

#A vs D
#Load data
MT.bact.T.A <- readRDS(file = here("data", "intermediate", "MT.bact.T.A.RDS"))
MT.bact.T.D <- readRDS(file = here("data", "intermediate", "MT.bact.T.D.RDS"))
#use the DE function to get results
DE.results.bact.T.AvsD <- DE(MT.bact.T.A,MT.bact.T.D)
saveRDS(DE.results.bact.T.AvsD, file = here("data", "intermediate", "DE.results.bact.T.AvsD.RDS"))
#Get number of DE transcripts
length(DE.results.bact.T.AvsD$DEfound) #10,808
DE.counts[nrow(DE.counts)+1,] = c("Tillering", "A", "Bacteria", length(DE.results.bact.T.AvsD$DEfound)) #append to DE.counts
rm(DE.results.bact.T.AvsD)
rm(MT.bact.T.A)

#B vs D
#Load data
MT.bact.T.B <- readRDS(file = here("data", "intermediate", "MT.bact.T.B.RDS"))
#use the DE function to get results
DE.results.bact.T.BvsD <- DE(MT.bact.T.B,MT.bact.T.D)
saveRDS(DE.results.bact.T.BvsD, file = here("data", "intermediate", "DE.results.bact.T.BvsD.RDS"))
#Get number of DE transcripts
length(DE.results.bact.T.BvsD$DEfound) #3,031
DE.counts[nrow(DE.counts)+1,] = c("Tillering", "B", "Bacteria", length(DE.results.bact.T.BvsD$DEfound)) #append to DE.counts
rm(DE.results.bact.T.BvsD)
rm(MT.bact.T.B)

#C vs D
#Load data
MT.bact.T.C <- readRDS(file = here("data", "intermediate", "MT.bact.T.C.RDS"))
#use the DE function to get results
DE.results.bact.T.CvsD <- DE(MT.bact.T.C,MT.bact.T.D)
saveRDS(DE.results.bact.T.CvsD, file = here("data", "intermediate", "DE.results.bact.T.CvsD.RDS"))
#Get number of DE transcripts
length(DE.results.bact.T.CvsD$DEfound) #3,315
DE.counts[nrow(DE.counts)+1,] = c("Tillering", "C", "Bacteria", length(DE.results.bact.T.CvsD$DEfound)) #append to DE.counts
rm(DE.results.bact.T.CvsD)
rm(MT.bact.T.C)
rm(MT.bact.T.D)

#Booting
#Compare to control (D)

#A vs D
#Load data
MT.bact.B.A <- readRDS(file = here("data", "intermediate", "MT.bact.B.A.RDS"))
MT.bact.B.D <- readRDS(file = here("data", "intermediate", "MT.bact.B.D.RDS"))
#use the DE function to get results
DE.results.bact.B.AvsD <- DE(MT.bact.B.A,MT.bact.B.D)
saveRDS(DE.results.bact.B.AvsD, file = here("data", "intermediate", "DE.results.bact.B.AvsD.RDS"))
#Get number of DE transcripts
length(DE.results.bact.B.AvsD$DEfound) #96,554
DE.counts[nrow(DE.counts)+1,] = c("Booting", "A", "Bacteria", length(DE.results.bact.B.AvsD$DEfound)) #append to DE.counts
rm(DE.results.bact.B.AvsD)
rm(MT.bact.B.A)

#B vs D
#Load data
MT.bact.B.B <- readRDS(file = here("data", "intermediate", "MT.bact.B.B.RDS"))
#use the DE function to get results
DE.results.bact.B.BvsD <- DE(MT.bact.B.B,MT.bact.B.D)
saveRDS(DE.results.bact.B.BvsD, file = here("data", "intermediate", "DE.results.bact.B.BvsD.RDS"))
#Get number of DE transcripts
length(DE.results.bact.B.BvsD$DEfound) #38,346
DE.counts[nrow(DE.counts)+1,] = c("Booting", "B", "Bacteria", length(DE.results.bact.B.BvsD$DEfound)) #append to DE.counts
rm(DE.results.bact.B.BvsD)
rm(MT.bact.B.B)

#C vs D
#Load data
MT.bact.B.C <- readRDS(file = here("data", "intermediate", "MT.bact.B.C.RDS"))
#use the DE function to get results
DE.results.bact.B.CvsD <- DE(MT.bact.B.C,MT.bact.B.D)
saveRDS(DE.results.bact.B.CvsD, file = here("data", "intermediate", "DE.results.bact.B.CvsD.RDS"))
#Get number of DE transcripts
length(DE.results.bact.B.CvsD$DEfound) #4,818
DE.counts[nrow(DE.counts)+1,] = c("Booting", "C", "Bacteria", length(DE.results.bact.B.CvsD$DEfound)) #append to DE.counts
rm(DE.results.bact.B.CvsD)
rm(MT.bact.B.C)
rm(MT.bact.B.D)


#Stem elongation
#Compare to control (D)

#A vs D
#Load data
MT.bact.SE.A <- readRDS(file = here("data", "intermediate", "MT.bact.SE.A.RDS"))
MT.bact.SE.D <- readRDS(file = here("data", "intermediate", "MT.bact.SE.D.RDS"))
#use the DE function to get results
DE.results.bact.SE.AvsD <- DE(MT.bact.SE.A,MT.bact.SE.D)
saveRDS(DE.results.bact.SE.AvsD, file = here("data", "intermediate", "DE.results.bact.SE.AvsD.RDS"))
#Get number of DE transcripts
length(DE.results.bact.SE.AvsD$DEfound) #13,102
DE.counts[nrow(DE.counts)+1,] = c("Stem Elongation", "A", "Bacteria", length(DE.results.bact.SE.AvsD$DEfound)) #append to DE.counts
rm(DE.results.bact.SE.AvsD)
rm(MT.bact.SE.A)

#B vs D
#Load data
MT.bact.SE.B <- readRDS(file = here("data", "intermediate", "MT.bact.SE.B.RDS"))
#use the DE function to get results
DE.results.bact.SE.BvsD <- DE(MT.bact.SE.B,MT.bact.SE.D)
saveRDS(DE.results.bact.SE.BvsD, file = here("data", "intermediate", "DE.results.bact.SE.BvsD.RDS"))
#Get number of DE transcripts
length(DE.results.bact.SE.BvsD$DEfound) #3,082
DE.counts[nrow(DE.counts)+1,] = c("Stem Elongation", "B", "Bacteria", length(DE.results.bact.SE.BvsD$DEfound)) #append to DE.counts
rm(DE.results.bact.SE.BvsD)
rm(MT.bact.SE.B)

#C vs D
#Load data
MT.bact.SE.C <- readRDS(file = here("data", "intermediate", "MT.bact.SE.C.RDS"))
#use the DE function to get results
DE.results.bact.SE.CvsD <- DE(MT.bact.SE.C,MT.bact.SE.D)
saveRDS(DE.results.bact.SE.CvsD, file = here("data", "intermediate", "DE.results.bact.SE.CvsD.RDS"))
#Get number of DE transcripts
length(DE.results.bact.SE.CvsD$DEfound) #3,590
DE.counts[nrow(DE.counts)+1,] = c("Stem Elongation", "C", "Bacteria", length(DE.results.bact.SE.CvsD$DEfound)) #append to DE.counts
rm(DE.results.bact.SE.CvsD)
rm(MT.bact.SE.C)
rm(MT.bact.SE.D)

#Heading
#Compare to control (D)

#A vs D
#Load data
MT.bact.H.A <- readRDS(file = here("data", "intermediate", "MT.bact.H.A.RDS"))
MT.bact.H.D <- readRDS(file = here("data", "intermediate", "MT.bact.H.D.RDS"))
#use the DE function to get results
DE.results.bact.H.AvsD <- DE(MT.bact.H.A,MT.bact.H.D)
saveRDS(DE.results.bact.H.AvsD, file = here("data", "intermediate", "DE.results.bact.H.AvsD.RDS"))
#Get number of DE transcripts
length(DE.results.bact.H.AvsD$DEfound) #19,852
DE.counts[nrow(DE.counts)+1,] = c("Heading", "A", "Bacteria", length(DE.results.bact.H.AvsD$DEfound)) #append to DE.counts
rm(DE.results.bact.H.AvsD)
rm(MT.bact.H.A)

#B vs D
#Load data
MT.bact.H.B <- readRDS(file = here("data", "intermediate", "MT.bact.H.B.RDS"))
#use the DE function to get results
DE.results.bact.H.BvsD <- DE(MT.bact.H.B,MT.bact.H.D)
saveRDS(DE.results.bact.H.BvsD, file = here("data", "intermediate", "DE.results.bact.H.BvsD.RDS"))
#Get number of DE transcripts
length(DE.results.bact.H.BvsD$DEfound) #4,630
DE.counts[nrow(DE.counts)+1,] = c("Heading", "B", "Bacteria", length(DE.results.bact.H.BvsD$DEfound)) #append to DE.counts
rm(DE.results.bact.H.BvsD)
rm(MT.bact.H.B)

#C vs D
#Load data
MT.bact.H.C <- readRDS(file = here("data", "intermediate", "MT.bact.H.C.RDS"))
#use the DE function to get results
DE.results.bact.H.CvsD <- DE(MT.bact.H.C,MT.bact.H.D)
saveRDS(DE.results.bact.H.CvsD, file = here("data", "intermediate", "DE.results.bact.H.CvsD.RDS"))
#Get number of DE transcripts
length(DE.results.bact.H.CvsD$DEfound) #
DE.counts[nrow(DE.counts)+1,] = c("Heading", "C", "Bacteria", length(DE.results.bact.H.CvsD$DEfound)) #append to DE.counts
rm(DE.results.bact.H.CvsD)
rm(MT.bact.H.C)
rm(MT.bact.H.D)

#Flowering
#Compare to control (D)

#A vs D
#Load data
MT.bact.F.A <- readRDS(file = here("data", "intermediate", "MT.bact.F.A.RDS"))
MT.bact.F.D <- readRDS(file = here("data", "intermediate", "MT.bact.F.D.RDS"))
#use the DE function to get results
DE.results.bact.F.AvsD <- DE(MT.bact.F.A,MT.bact.F.D)
saveRDS(DE.results.bact.F.AvsD, file = here("data", "intermediate", "DE.results.bact.F.AvsD.RDS"))
#Get number of DE transcripts
length(DE.results.bact.F.AvsD$DEfound) #5,273
DE.counts[nrow(DE.counts)+1,] = c("Flowering", "A", "Bacteria", length(DE.results.bact.F.AvsD$DEfound)) #append to DE.counts
rm(DE.results.bact.F.AvsD)
rm(MT.bact.F.A)

#B vs D
#Load data
MT.bact.F.B <- readRDS(file = here("data", "intermediate", "MT.bact.F.B.RDS"))
#use the DE function to get results
DE.results.bact.F.BvsD <- DE(MT.bact.F.B,MT.bact.F.D)
saveRDS(DE.results.bact.F.BvsD, file = here("data", "intermediate", "DE.results.bact.F.BvsD.RDS"))
#Get number of DE transcripts
length(DE.results.bact.F.BvsD$DEfound) #8,429
DE.counts[nrow(DE.counts)+1,] = c("Flowering", "B", "Bacteria", length(DE.results.bact.F.BvsD$DEfound)) #append to DE.counts
rm(DE.results.bact.F.BvsD)
rm(MT.bact.F.B)

#C vs D
#Load data
MT.bact.F.C <- readRDS(file = here("data", "intermediate", "MT.bact.F.C.RDS"))
#use the DE function to get results
DE.results.bact.F.CvsD <- DE(MT.bact.F.C,MT.bact.F.D)
saveRDS(DE.results.bact.F.CvsD, file = here("data", "intermediate", "DE.results.bact.F.CvsD.RDS"))
#Get number of DE transcripts
length(DE.results.bact.F.CvsD$DEfound) #8,911
DE.counts[nrow(DE.counts)+1,] = c("Flowering", "C", "Bacteria", length(DE.results.bact.F.CvsD$DEfound)) #append to DE.counts
rm(DE.results.bact.F.CvsD)
rm(MT.bact.F.C)
rm(MT.bact.F.D)


##Fungal genes
#Tillering
#Compare to control (D)

#A vs D
#Load data
MT.fungi.T.A <- readRDS(file = here("data", "intermediate", "MT.fungi.T.A.RDS"))
MT.fungi.T.D <- readRDS(file = here("data", "intermediate", "MT.fungi.T.D.RDS"))
#use the DE function to get results
DE.results.fungi.T.AvsD <- DE(MT.fungi.T.A,MT.fungi.T.D)
saveRDS(DE.results.fungi.T.AvsD, file = here("data", "intermediate", "DE.results.fungi.T.AvsD.RDS"))
#Get number of DE transcripts
length(DE.results.fungi.T.AvsD$DEfound) #25
DE.counts[nrow(DE.counts)+1,] = c("Tillering", "A", "Fungi", length(DE.results.fungi.T.AvsD$DEfound)) #append to DE.counts
rm(DE.results.fungi.T.AvsD)
rm(MT.fungi.T.A)

#B vs D
#Load data
MT.fungi.T.B <- readRDS(file = here("data", "intermediate", "MT.fungi.T.B.RDS"))
#use the DE function to get results
DE.results.fungi.T.BvsD <- DE(MT.fungi.T.B,MT.fungi.T.D)
saveRDS(DE.results.fungi.T.BvsD, file = here("data", "intermediate", "DE.results.fungi.T.BvsD.RDS"))
#Get number of DE transcripts
length(DE.results.fungi.T.BvsD$DEfound) #17
DE.counts[nrow(DE.counts)+1,] = c("Tillering", "B", "Fungi", length(DE.results.fungi.T.BvsD$DEfound)) #append to DE.counts
rm(DE.results.fungi.T.BvsD)
rm(MT.fungi.T.B)

#C vs D
#Load data
MT.fungi.T.C <- readRDS(file = here("data", "intermediate", "MT.fungi.T.C.RDS"))
#use the DE function to get results
DE.results.fungi.T.CvsD <- DE(MT.fungi.T.C,MT.fungi.T.D)
saveRDS(DE.results.fungi.T.CvsD, file = here("data", "intermediate", "DE.results.fungi.T.CvsD.RDS"))
#Get number of DE transcripts
length(DE.results.fungi.T.CvsD$DEfound) #27
DE.counts[nrow(DE.counts)+1,] = c("Tillering", "C", "Fungi", length(DE.results.fungi.T.CvsD$DEfound)) #append to DE.counts
rm(DE.results.fungi.T.CvsD)
rm(MT.fungi.T.C)
rm(MT.fungi.T.D)

#Booting
#Compare to control (D)

#A vs D
#Load data
MT.fungi.B.A <- readRDS(file = here("data", "intermediate", "MT.fungi.B.A.RDS"))
MT.fungi.B.D <- readRDS(file = here("data", "intermediate", "MT.fungi.B.D.RDS"))
#use the DE function to get results
DE.results.fungi.B.AvsD <- DE(MT.fungi.B.A,MT.fungi.B.D)
saveRDS(DE.results.fungi.B.AvsD, file = here("data", "intermediate", "DE.results.fungi.B.AvsD.RDS"))
#Get number of DE transcripts
length(DE.results.fungi.B.AvsD$DEfound) #63
DE.counts[nrow(DE.counts)+1,] = c("Booting", "A", "Fungi", length(DE.results.fungi.B.AvsD$DEfound)) #append to DE.counts
rm(DE.results.fungi.B.AvsD)
rm(MT.fungi.B.A)

#B vs D
#Load data
MT.fungi.B.B <- readRDS(file = here("data", "intermediate", "MT.fungi.B.B.RDS"))
#use the DE function to get results
DE.results.fungi.B.BvsD <- DE(MT.fungi.B.B,MT.fungi.B.D)
saveRDS(DE.results.fungi.B.BvsD, file = here("data", "intermediate", "DE.results.fungi.B.BvsD.RDS"))
#Get number of DE transcripts
length(DE.results.fungi.B.BvsD$DEfound) #125
DE.counts[nrow(DE.counts)+1,] = c("Booting", "B", "Fungi", length(DE.results.fungi.B.BvsD$DEfound)) #append to DE.counts
rm(DE.results.fungi.B.BvsD)
rm(MT.fungi.B.B)

#C vs D
#Load data
MT.fungi.B.C <- readRDS(file = here("data", "intermediate", "MT.fungi.B.C.RDS"))
#use the DE function to get results
DE.results.fungi.B.CvsD <- DE(MT.fungi.B.C,MT.fungi.B.D)
saveRDS(DE.results.fungi.B.CvsD, file = here("data", "intermediate", "DE.results.fungi.B.CvsD.RDS"))
#Get number of DE transcripts
length(DE.results.fungi.B.CvsD$DEfound) #76
DE.counts[nrow(DE.counts)+1,] = c("Booting", "C", "Fungi", length(DE.results.fungi.B.CvsD$DEfound)) #append to DE.counts
rm(DE.results.fungi.B.CvsD)
rm(MT.fungi.B.C)
rm(MT.fungi.B.D)


#Stem elongation
#Compare to control (D)

#A vs D
#Load data
MT.fungi.SE.A <- readRDS(file = here("data", "intermediate", "MT.fungi.SE.A.RDS"))
MT.fungi.SE.D <- readRDS(file = here("data", "intermediate", "MT.fungi.SE.D.RDS"))
#use the DE function to get results
DE.results.fungi.SE.AvsD <- DE(MT.fungi.SE.A,MT.fungi.SE.D)
saveRDS(DE.results.fungi.SE.AvsD, file = here("data", "intermediate", "DE.results.fungi.SE.AvsD.RDS"))
#Get number of DE transcripts
length(DE.results.fungi.SE.AvsD$DEfound) #80
DE.counts[nrow(DE.counts)+1,] = c("Stem Elongation", "A", "Fungi", length(DE.results.fungi.SE.AvsD$DEfound)) #append to DE.counts
rm(DE.results.fungi.SE.AvsD)
rm(MT.fungi.SE.A)

#B vs D
#Load data
MT.fungi.SE.B <- readRDS(file = here("data", "intermediate", "MT.fungi.SE.B.RDS"))
#use the DE function to get results
DE.results.fungi.SE.BvsD <- DE(MT.fungi.SE.B,MT.fungi.SE.D)
saveRDS(DE.results.fungi.SE.BvsD, file = here("data", "intermediate", "DE.results.fungi.SE.BvsD.RDS"))
#Get number of DE transcripts
length(DE.results.fungi.SE.BvsD$DEfound) #37
DE.counts[nrow(DE.counts)+1,] = c("Stem Elongation", "B", "Fungi", length(DE.results.fungi.SE.BvsD$DEfound)) #append to DE.counts
rm(DE.results.fungi.SE.BvsD)
rm(MT.fungi.SE.B)

#C vs D
#Load data
MT.fungi.SE.C <- readRDS(file = here("data", "intermediate", "MT.fungi.SE.C.RDS"))
#use the DE function to get results
DE.results.fungi.SE.CvsD <- DE(MT.fungi.SE.C,MT.fungi.SE.D)
saveRDS(DE.results.fungi.SE.CvsD, file = here("data", "intermediate", "DE.results.fungi.SE.CvsD.RDS"))
#Get number of DE transcripts
length(DE.results.fungi.SE.CvsD$DEfound) #30
DE.counts[nrow(DE.counts)+1,] = c("Stem Elongation", "C", "Fungi", length(DE.results.fungi.SE.CvsD$DEfound)) #append to DE.counts
rm(DE.results.fungi.SE.CvsD)
rm(MT.fungi.SE.C)
rm(MT.fungi.SE.D)

#Heading
#Compare to control (D)

#A vs D
#Load data
MT.fungi.H.A <- readRDS(file = here("data", "intermediate", "MT.fungi.H.A.RDS"))
MT.fungi.H.D <- readRDS(file = here("data", "intermediate", "MT.fungi.H.D.RDS"))
#use the DE function to get results
DE.results.fungi.H.AvsD <- DE(MT.fungi.H.A,MT.fungi.H.D)
saveRDS(DE.results.fungi.H.AvsD, file = here("data", "intermediate", "DE.results.fungi.H.AvsD.RDS"))
#Get number of DE transcripts
length(DE.results.fungi.H.AvsD$DEfound) #126
DE.counts[nrow(DE.counts)+1,] = c("Heading", "A", "Fungi", length(DE.results.fungi.H.AvsD$DEfound)) #append to DE.counts
rm(DE.results.fungi.H.AvsD)
rm(MT.fungi.H.A)

#B vs D
#Load data
MT.fungi.H.B <- readRDS(file = here("data", "intermediate", "MT.fungi.H.B.RDS"))
#use the DE function to get results
DE.results.fungi.H.BvsD <- DE(MT.fungi.H.B,MT.fungi.H.D)
saveRDS(DE.results.fungi.H.BvsD, file = here("data", "intermediate", "DE.results.fungi.H.BvsD.RDS"))
#Get number of DE transcripts
length(DE.results.fungi.H.BvsD$DEfound) #108
DE.counts[nrow(DE.counts)+1,] = c("Heading", "B", "Fungi", length(DE.results.fungi.H.BvsD$DEfound)) #append to DE.counts
rm(DE.results.fungi.H.BvsD)
rm(MT.fungi.H.B)

#C vs D
#Load data
MT.fungi.H.C <- readRDS(file = here("data", "intermediate", "MT.fungi.H.C.RDS"))
#use the DE function to get results
DE.results.fungi.H.CvsD <- DE(MT.fungi.H.C,MT.fungi.H.D)
saveRDS(DE.results.fungi.H.CvsD, file = here("data", "intermediate", "DE.results.fungi.H.CvsD.RDS"))
#Get number of DE transcripts
length(DE.results.fungi.H.CvsD$DEfound) #170
DE.counts[nrow(DE.counts)+1,] = c("Heading", "C", "Fungi", length(DE.results.fungi.H.CvsD$DEfound)) #append to DE.counts
rm(DE.results.fungi.H.CvsD)
rm(MT.fungi.H.C)
rm(MT.fungi.H.D)

#Flowering
#Compare to control (D)

#A vs D
#Load data
MT.fungi.F.A <- readRDS(file = here("data", "intermediate", "MT.fungi.F.A.RDS"))
MT.fungi.F.D <- readRDS(file = here("data", "intermediate", "MT.fungi.F.D.RDS"))
#use the DE function to get results
DE.results.fungi.F.AvsD <- DE(MT.fungi.F.A,MT.fungi.F.D)
saveRDS(DE.results.fungi.F.AvsD, file = here("data", "intermediate", "DE.results.fungi.F.AvsD.RDS"))
#Get number of DE transcripts
length(DE.results.fungi.F.AvsD$DEfound) #84
DE.counts[nrow(DE.counts)+1,] = c("Flowering", "A", "Fungi", length(DE.results.fungi.F.AvsD$DEfound)) #append to DE.counts
rm(DE.results.fungi.F.AvsD)
rm(MT.fungi.F.A)

#B vs D
#Load data
MT.fungi.F.B <- readRDS(file = here("data", "intermediate", "MT.fungi.F.B.RDS"))
#use the DE function to get results
DE.results.fungi.F.BvsD <- DE(MT.fungi.F.B,MT.fungi.F.D)
saveRDS(DE.results.fungi.F.BvsD, file = here("data", "intermediate", "DE.results.fungi.F.BvsD.RDS"))
#Get number of DE transcripts
length(DE.results.fungi.F.BvsD$DEfound) #60
DE.counts[nrow(DE.counts)+1,] = c("Flowering", "B", "Fungi", length(DE.results.fungi.F.BvsD$DEfound)) #append to DE.counts
rm(DE.results.fungi.F.BvsD)
rm(MT.fungi.F.B)

#C vs D
#Load data
MT.fungi.F.C <- readRDS(file = here("data", "intermediate", "MT.fungi.F.C.RDS"))
#use the DE function to get results
DE.results.fungi.F.CvsD <- DE(MT.fungi.F.C,MT.fungi.F.D)
saveRDS(DE.results.fungi.F.CvsD, file = here("data", "intermediate", "DE.results.fungi.F.CvsD.RDS"))
#Get number of DE transcripts
length(DE.results.fungi.F.CvsD$DEfound) #141
DE.counts[nrow(DE.counts)+1,] = c("Flowering", "C", "Fungi", length(DE.results.fungi.F.CvsD$DEfound)) #append to DE.counts
rm(DE.results.fungi.F.CvsD)
rm(MT.fungi.F.C)
rm(MT.fungi.F.D)

#Save the DE count file
saveRDS(DE.counts, file = here("data", "intermediate", "DE.counts.RDS"))

###Compare treatment vs. itself before and after each growth stages. Compare to difference in controls at same stages.
#All
#Drought at stem elongation (A), SE vs.B (right after)
MT.SE.A <- readRDS(file = here("data", "intermediate", "MT.SE.A.RDS"))
MT.B.A <- readRDS(file = here("data", "intermediate", "MT.B.A.RDS"))
DE.results.SEvsB.A <- DE(MT.SE.A,MT.B.A)
saveRDS(DE.results.SEvsB.A, file = here("data", "intermediate", "DE.results.SEvsB.A.RDS"))
#Get number of DE transcripts
length(DE.results.SEvsB.A$DEfound) #273,969
DE.counts.res <- data.frame(Contrast="SE vs. B", Treatment="A", Subset="All", DEcount=length(DE.results.SEvsB.A$DEfound))#create the dataframe
rm(DE.results.SEvsB.A)
rm(MT.SE.A)
rm(MT.B.A)

#Control (D), SE vs.B (right after)
MT.SE.D <- readRDS(file = here("data", "intermediate", "MT.SE.D.RDS"))
MT.B.D <- readRDS(file = here("data", "intermediate", "MT.B.D.RDS"))
DE.results.SEvsB.D <- DE(MT.SE.D,MT.B.D)
saveRDS(DE.results.SEvsB.D, file = here("data", "intermediate", "DE.results.SEvsB.D.RDS"))
#Get number of DE transcripts
length(DE.results.SEvsB.D$DEfound) #30,286
DE.counts.res[nrow(DE.counts.res)+1,] = c("SE vs. B", "D", "All", length(DE.results.SEvsB.D$DEfound)) #append to DE.counts.res
rm(DE.results.SEvsB.D)
rm(MT.SE.D)
rm(MT.B.D)

#Drought at booting (B), B vs. H (right after)
MT.H.B <- readRDS(file = here("data", "intermediate", "MT.H.B.RDS"))
MT.B.B <- readRDS(file = here("data", "intermediate", "MT.B.B.RDS"))
DE.results.HvsB.B <- DE(MT.H.B,MT.B.B)
saveRDS(DE.results.HvsB.B, file = here("data", "intermediate", "DE.results.HvsB.B.RDS"))
#Get number of DE transcripts
length(DE.results.HvsB.B$DEfound) #49,704
DE.counts.res[nrow(DE.counts.res)+1,] = c("H vs. B", "B", "All", length(DE.results.HvsB.B$DEfound)) #append to DE.counts.res
rm(DE.results.HvsB.B)
rm(MT.H.B)
rm(MT.B.B)

#Control (D), B vs. H (right after)
MT.H.D <- readRDS(file = here("data", "intermediate", "MT.H.D.RDS"))
MT.B.D <- readRDS(file = here("data", "intermediate", "MT.B.D.RDS"))
DE.results.HvsB.D <- DE(MT.H.D,MT.B.D)
saveRDS(DE.results.HvsB.D, file = here("data", "intermediate", "DE.results.HvsB.D.RDS"))
#Get number of DE transcripts
length(DE.results.HvsB.D$DEfound) #18,573
DE.counts.res[nrow(DE.counts.res)+1,] = c("H vs. B", "D", "All", length(DE.results.HvsB.D$DEfound)) #append to DE.counts.res
rm(DE.results.HvsB.D)
rm(MT.H.D)
rm(MT.B.D)

#Drought at heading (C), H vs. F (right after)
MT.H.C <- readRDS(file = here("data", "intermediate", "MT.H.C.RDS"))
MT.F.C <- readRDS(file = here("data", "intermediate", "MT.F.C.RDS"))
DE.results.HvsF.C <- DE(MT.H.C,MT.F.C)
saveRDS(DE.results.HvsF.C, file = here("data", "intermediate", "DE.results.HvsF.C.RDS"))
#Get number of DE transcripts
length(DE.results.HvsF.C$DEfound) #622,302
DE.counts.res[nrow(DE.counts.res)+1,] = c("H vs. F", "C", "All", length(DE.results.HvsF.C$DEfound)) #append to DE.counts.res
rm(DE.results.HvsF.C)
rm(MT.H.C)
rm(MT.F.C)

#Control (D), H vs. F (right after)
MT.H.D <- readRDS(file = here("data", "intermediate", "MT.H.D.RDS"))
MT.F.D <- readRDS(file = here("data", "intermediate", "MT.F.D.RDS"))
DE.results.HvsF.D <- DE(MT.H.D,MT.F.D)
saveRDS(DE.results.HvsF.D, file = here("data", "intermediate", "DE.results.HvsF.D.RDS"))
#Get number of DE transcripts
length(DE.results.HvsF.D$DEfound) #85,833
DE.counts.res[nrow(DE.counts.res)+1,] = c("H vs. F", "D", "All", length(DE.results.HvsF.D$DEfound)) #append to DE.counts.res
rm(DE.results.HvsF.D)
rm(MT.H.D)
rm(MT.F.D)

#Bacteria
#Drought at stem elongation (A), SE vs.B (right after)
MT.bact.SE.A <- readRDS(file = here("data", "intermediate", "MT.bact.SE.A.RDS"))
MT.bact.B.A <- readRDS(file = here("data", "intermediate", "MT.bact.B.A.RDS"))
DE.results.bact.SEvsB.A <- DE(MT.bact.SE.A,MT.bact.B.A)
saveRDS(DE.results.bact.SEvsB.A, file = here("data", "intermediate", "DE.results.bact.SEvsB.A.RDS"))
#Get number of DE transcripts
length(DE.results.bact.SEvsB.A$DEfound) #183,091
DE.counts.res[nrow(DE.counts.res)+1,] = c("SE vs. B", "A", "Bacteria", length(DE.results.bact.SEvsB.A$DEfound)) #append to DE.counts.res
rm(DE.results.bact.SEvsB.A)
rm(MT.bact.SE.A)
rm(MT.bact.B.A)

#Control (D), SE vs.B (right after)
MT.bact.SE.D <- readRDS(file = here("data", "intermediate", "MT.bact.SE.D.RDS"))
MT.bact.B.D <- readRDS(file = here("data", "intermediate", "MT.bact.B.D.RDS"))
DE.results.bact.SEvsB.D <- DE(MT.bact.SE.D,MT.bact.B.D)
saveRDS(DE.results.bact.SEvsB.D, file = here("data", "intermediate", "DE.results.bact.SEvsB.D.RDS"))
#Get number of DE transcripts
length(DE.results.bact.SEvsB.D$DEfound) #19,304
DE.counts.res[nrow(DE.counts.res)+1,] = c("SE vs. B", "D", "Bacteria", length(DE.results.bact.SEvsB.D$DEfound)) #append to DE.counts.res
rm(DE.results.bact.SEvsB.D)
rm(MT.bact.SE.D)
rm(MT.bact.B.D)

#Drought at booting (B), B vs. H (right after)
MT.bact.H.B <- readRDS(file = here("data", "intermediate", "MT.bact.H.B.RDS"))
MT.bact.B.B <- readRDS(file = here("data", "intermediate", "MT.bact.B.B.RDS"))
DE.results.bact.HvsB.B <- DE(MT.bact.H.B,MT.bact.B.B)
saveRDS(DE.results.bact.HvsB.B, file = here("data", "intermediate", "DE.results.bact.HvsB.B.RDS"))
#Get number of DE transcripts
length(DE.results.bact.HvsB.B$DEfound) #32,195
DE.counts.res[nrow(DE.counts.res)+1,] = c("H vs. B", "B", "Bacteria", length(DE.results.bact.HvsB.B$DEfound)) #append to DE.counts.res
rm(DE.results.bact.HvsB.B)
rm(MT.bact.H.B)
rm(MT.bact.B.B)

###MISSING in intermediate files! 20230330
#Control (D), B vs. H (right after)
MT.bact.H.D <- readRDS(file = here("data", "intermediate", "MT.bact.H.D.RDS"))
MT.bact.B.D <- readRDS(file = here("data", "intermediate", "MT.bact.B.D.RDS"))
DE.results.bact.HvsB.D <- DE(MT.bact.H.D,MT.bact.B.D)
saveRDS(DE.results.bact.HvsB.D, file = here("data", "intermediate", "DE.results.bact.HvsB.D.RDS"))
#Get number of DE transcripts
length(DE.results.bact.HvsB.D$DEfound) #11,439
DE.counts.res[nrow(DE.counts.res)+1,] = c("H vs. B", "D", "Bacteria", length(DE.results.bact.HvsB.D$DEfound)) #append to DE.counts.res
rm(DE.results.bact.HvsB.D)
rm(MT.bact.H.D)
rm(MT.bact.B.D)
####

#Drought at heading (C), H vs. F (right after)
MT.bact.H.C <- readRDS(file = here("data", "intermediate", "MT.bact.H.C.RDS"))
MT.bact.F.C <- readRDS(file = here("data", "intermediate", "MT.bact.F.C.RDS"))
DE.results.bact.HvsF.C <- DE(MT.bact.H.C,MT.bact.F.C)
saveRDS(DE.results.bact.HvsF.C, file = here("data", "intermediate", "DE.results.bact.HvsF.C.RDS"))
#Get number of DE transcripts
length(DE.results.bact.HvsF.C$DEfound) #447,881
DE.counts.res[nrow(DE.counts.res)+1,] = c("H vs. F", "C", "Bacteria", length(DE.results.bact.HvsF.C$DEfound)) #append to DE.counts.res
rm(DE.results.bact.HvsF.C)
rm(MT.bact.H.C)
rm(MT.bact.F.C)

#Control  (D), H vs. F (right after)
MT.bact.H.D <- readRDS(file = here("data", "intermediate", "MT.bact.H.D.RDS"))
MT.bact.F.D <- readRDS(file = here("data", "intermediate", "MT.bact.F.D.RDS"))
DE.results.bact.HvsF.D <- DE(MT.bact.H.D,MT.bact.F.D)
saveRDS(DE.results.bact.HvsF.D, file = here("data", "intermediate", "DE.results.bact.HvsF.D.RDS"))
#Get number of DE transcripts
length(DE.results.bact.HvsF.D$DEfound) #57,717
DE.counts.res[nrow(DE.counts.res)+1,] = c("H vs. F", "D", "Bacteria", length(DE.results.bact.HvsF.D$DEfound)) #append to DE.counts.res
rm(DE.results.bact.HvsF.D)
rm(MT.bact.H.D)
rm(MT.bact.F.D)

#Fungi
#Drought at stem elongation (A), SE vs.B (right after)
MT.fungi.SE.A <- readRDS(file = here("data", "intermediate", "MT.fungi.SE.A.RDS"))
MT.fungi.B.A <- readRDS(file = here("data", "intermediate", "MT.fungi.B.A.RDS"))
DE.results.fungi.SEvsB.A <- DE(MT.fungi.SE.A,MT.fungi.B.A)
saveRDS(DE.results.fungi.SEvsB.A, file = here("data", "intermediate", "DE.results.fungi.SEvsB.A.RDS"))
#Get number of DE transcripts
length(DE.results.fungi.SEvsB.A$DEfound) #157
DE.counts.res[nrow(DE.counts.res)+1,] = c("SE vs. B", "A", "Fungi", length(DE.results.fungi.SEvsB.A$DEfound)) #append to DE.counts.res
rm(DE.results.fungi.SEvsB.A)
rm(MT.fungi.SE.A)
rm(MT.fungi.B.A)

#Control  (D), SE vs.B (right after)
MT.fungi.SE.D <- readRDS(file = here("data", "intermediate", "MT.fungi.SE.D.RDS"))
MT.fungi.B.D <- readRDS(file = here("data", "intermediate", "MT.fungi.B.D.RDS"))
DE.results.fungi.SEvsB.D <- DE(MT.fungi.SE.D,MT.fungi.B.D)
saveRDS(DE.results.fungi.SEvsB.D, file = here("data", "intermediate", "DE.results.fungi.SEvsB.D.RDS"))
#Get number of DE transcripts
length(DE.results.fungi.SEvsB.D$DEfound) #114
DE.counts.res[nrow(DE.counts.res)+1,] = c("SE vs. B", "D", "Fungi", length(DE.results.fungi.SEvsB.D$DEfound)) #append to DE.counts.res
rm(DE.results.fungi.SEvsB.D)
rm(MT.fungi.SE.D)
rm(MT.fungi.B.D)

#Drought at booting (B), B vs. H (right after)
MT.fungi.H.B <- readRDS(file = here("data", "intermediate", "MT.fungi.H.B.RDS"))
MT.fungi.B.B <- readRDS(file = here("data", "intermediate", "MT.fungi.B.B.RDS"))
DE.results.fungi.HvsB.B <- DE(MT.fungi.H.B,MT.fungi.B.B)
saveRDS(DE.results.fungi.HvsB.B, file = here("data", "intermediate", "DE.results.fungi.HvsB.B.RDS"))
#Get number of DE transcripts
length(DE.results.fungi.HvsB.B$DEfound) #132
DE.counts.res[nrow(DE.counts.res)+1,] = c("H vs. B", "B", "Fungi", length(DE.results.fungi.HvsB.B$DEfound)) #append to DE.counts.res
rm(DE.results.fungi.SEvsB.D)
rm(MT.fungi.H.B)
rm(MT.fungi.B.B)

#Control (D), B vs. H (right after)
MT.fungi.H.D <- readRDS(file = here("data", "intermediate", "MT.fungi.H.D.RDS"))
MT.fungi.B.D <- readRDS(file = here("data", "intermediate", "MT.fungi.B.D.RDS"))
DE.results.fungi.HvsB.D <- DE(MT.fungi.H.D,MT.fungi.B.D)
saveRDS(DE.results.fungi.HvsB.D, file = here("data", "intermediate", "DE.results.fungi.HvsB.D.RDS"))
#Get number of DE transcripts
length(DE.results.fungi.HvsB.D$DEfound) #46
DE.counts.res[nrow(DE.counts.res)+1,] = c("H vs. B", "D", "Fungi", length(DE.results.fungi.HvsB.D$DEfound)) #append to DE.counts.res
rm(DE.results.fungi.HvsB.D)
rm(MT.fungi.H.D)
rm(MT.fungi.B.D)

#Drought at heading (C), H vs. F (right after)
MT.fungi.H.C <- readRDS(file = here("data", "intermediate", "MT.fungi.H.C.RDS"))
MT.fungi.F.C <- readRDS(file = here("data", "intermediate", "MT.fungi.F.C.RDS"))
DE.results.fungi.HvsF.C <- DE(MT.fungi.H.C,MT.fungi.F.C)
saveRDS(DE.results.fungi.HvsF.C, file = here("data", "intermediate", "DE.results.fungi.HvsF.C.RDS"))
#Get number of DE transcripts
length(DE.results.fungi.HvsF.C$DEfound) #245
DE.counts.res[nrow(DE.counts.res)+1,] = c("H vs. F", "C", "Fungi", length(DE.results.fungi.HvsF.C$DEfound)) #append to DE.counts.res
rm(DE.results.fungi.HvsF.C)
rm(MT.fungi.H.C)
rm(MT.fungi.F.C)

#Control (D), H vs. F (right after)
MT.fungi.H.D <- readRDS(file = here("data", "intermediate", "MT.fungi.H.D.RDS"))
MT.fungi.F.D <- readRDS(file = here("data", "intermediate", "MT.fungi.F.D.RDS"))
DE.results.fungi.HvsF.D <- DE(MT.fungi.H.D,MT.fungi.F.D)
saveRDS(DE.results.fungi.HvsF.D, file = here("data", "intermediate", "DE.results.fungi.HvsF.D.RDS"))
#Get number of DE transcripts
length(DE.results.fungi.HvsF.D$DEfound) #155
DE.counts.res[nrow(DE.counts.res)+1,] = c("H vs. F", "D", "Fungi", length(DE.results.fungi.HvsF.D$DEfound)) #append to DE.counts.res
rm(DE.results.fungi.HvsF.D)
rm(MT.fungi.H.D)
rm(MT.fungi.F.D)

##Fungi
#Drought at SE (A), T vs SE
MT.fungi.T.A <- readRDS(file = here("data", "intermediate", "MT.fungi.T.A.RDS"))
MT.fungi.SE.A <- readRDS(file = here("data", "intermediate", "MT.fungi.SE.A.RDS"))
DE.results.fungi.TvsSE.A <- DE(MT.fungi.T.A,MT.fungi.SE.A)
saveRDS(DE.results.fungi.TvsSE.A, file = here("data", "intermediate", "DE.results.fungi.TvsSE.A.RDS"))
#Get number of DE transcripts
length(DE.results.fungi.TvsSE.A$DEfound) #96
DE.counts.res[nrow(DE.counts.res)+1,] = c("T vs. SE", "A", "Fungi", length(DE.results.fungi.TvsSE.A$DEfound)) #append to DE.counts.res
rm(DE.results.fungi.TvsSE.A)
rm(MT.fungi.T.A)
rm(MT.fungi.SE.A)

#Drought at SE (A), B vs H
MT.fungi.B.A <- readRDS(file = here("data", "intermediate", "MT.fungi.B.A.RDS"))
MT.fungi.H.A <- readRDS(file = here("data", "intermediate", "MT.fungi.H.A.RDS"))
DE.results.fungi.BvsH.A <- DE(MT.fungi.B.A,MT.fungi.H.A)
saveRDS(DE.results.fungi.BvsH.A, file = here("data", "intermediate", "DE.results.fungi.BvsH.A.RDS"))
#Get number of DE transcripts
length(DE.results.fungi.BvsH.A$DEfound) #114
DE.counts.res[nrow(DE.counts.res)+1,] = c("B vs. H", "A", "Fungi", length(DE.results.fungi.BvsH.A$DEfound)) #append to DE.counts.res
rm(DE.results.fungi.BvsH.A)
rm(MT.fungi.B.A)
rm(MT.fungi.H.A)

#Drought at SE (A), H vs F
MT.fungi.H.A <- readRDS(file = here("data", "intermediate", "MT.fungi.H.A.RDS"))
MT.fungi.F.A <- readRDS(file = here("data", "intermediate", "MT.fungi.F.A.RDS"))
DE.results.fungi.HvsF.A <- DE(MT.fungi.H.A,MT.fungi.F.A)
saveRDS(DE.results.fungi.HvsF.A, file = here("data", "intermediate", "DE.results.fungi.HvsF.A.RDS"))
#Get number of DE transcripts
length(DE.results.fungi.HvsF.A$DEfound) #155
DE.counts.res[nrow(DE.counts.res)+1,] = c("H vs. F", "A", "Fungi", length(DE.results.fungi.HvsF.A$DEfound)) #append to DE.counts.res
rm(DE.results.fungi.HvsF.A)
rm(MT.fungi.H.A)
rm(MT.fungi.F.A)

#Drought at B (B), T vs SE
MT.fungi.T.B <- readRDS(file = here("data", "intermediate", "MT.fungi.T.B.RDS"))
MT.fungi.SE.B <- readRDS(file = here("data", "intermediate", "MT.fungi.SE.B.RDS"))
DE.results.fungi.TvsSE.B <- DE(MT.fungi.T.B,MT.fungi.SE.B)
saveRDS(DE.results.fungi.TvsSE.B, file = here("data", "intermediate", "DE.results.fungi.TvsSE.B.RDS"))
#Get number of DE transcripts
length(DE.results.fungi.TvsSE.B$DEfound) #67
DE.counts.res[nrow(DE.counts.res)+1,] = c("T vs. SE", "B", "Fungi", length(DE.results.fungi.TvsSE.B$DEfound)) #append to DE.counts.res
rm(DE.results.fungi.TvsSE.B)
rm(MT.fungi.T.B)
rm(MT.fungi.SE.B)

#Drought at B (B), SE vs B
MT.fungi.B.B <- readRDS(file = here("data", "intermediate", "MT.fungi.B.B.RDS"))
MT.fungi.SE.B <- readRDS(file = here("data", "intermediate", "MT.fungi.SE.B.RDS"))
DE.results.fungi.SEvsB.B <- DE(MT.fungi.B.B,MT.fungi.SE.B)
saveRDS(DE.results.fungi.SEvsB.B, file = here("data", "intermediate", "DE.results.fungi.SEvsB.B.RDS"))
#Get number of DE transcripts
length(DE.results.fungi.SEvsB.B$DEfound) #174
DE.counts.res[nrow(DE.counts.res)+1,] = c("SE vs. B", "B", "Fungi", length(DE.results.fungi.SEvsB.B$DEfound)) #append to DE.counts.res
rm(DE.results.fungi.SEvsB.B)
rm(MT.fungi.B.B)
rm(MT.fungi.SE.B)

#Drought at B (B), H vs F
MT.fungi.H.B <- readRDS(file = here("data", "intermediate", "MT.fungi.H.B.RDS"))
MT.fungi.F.B <- readRDS(file = here("data", "intermediate", "MT.fungi.F.B.RDS"))
DE.results.fungi.HvsF.B <- DE(MT.fungi.H.B,MT.fungi.F.B)
saveRDS(DE.results.fungi.HvsF.B, file = here("data", "intermediate", "DE.results.fungi.HvsF.B.RDS"))
#Get number of DE transcripts
length(DE.results.fungi.HvsF.B$DEfound) #134
DE.counts.res[nrow(DE.counts.res)+1,] = c("H vs. F", "B", "Fungi", length(DE.results.fungi.HvsF.B$DEfound)) #append to DE.counts.res
rm(DE.results.fungi.HvsF.B)
rm(MT.fungi.H.B)
rm(MT.fungi.F.B)

#Drought at H (C), T vs SE
MT.fungi.T.C <- readRDS(file = here("data", "intermediate", "MT.fungi.T.C.RDS"))
MT.fungi.SE.C <- readRDS(file = here("data", "intermediate", "MT.fungi.SE.C.RDS"))
DE.results.fungi.TvsSE.C <- DE(MT.fungi.T.C,MT.fungi.SE.C)
saveRDS(DE.results.fungi.TvsSE.C, file = here("data", "intermediate", "DE.results.fungi.TvsSE.C.RDS"))
#Get number of DE transcripts
length(DE.results.fungi.TvsSE.C$DEfound) #111
DE.counts.res[nrow(DE.counts.res)+1,] = c("T vs. SE", "C", "Fungi", length(DE.results.fungi.TvsSE.C$DEfound)) #append to DE.counts.res
rm(DE.results.fungi.TvsSE.C)
rm(MT.fungi.T.C)
rm(MT.fungi.SE.C)

#Drought at H (C), SE vs B
MT.fungi.B.C <- readRDS(file = here("data", "intermediate", "MT.fungi.B.C.RDS"))
MT.fungi.SE.C <- readRDS(file = here("data", "intermediate", "MT.fungi.SE.C.RDS"))
DE.results.fungi.SEvsB.C <- DE(MT.fungi.B.C,MT.fungi.SE.C)
saveRDS(DE.results.fungi.SEvsB.C, file = here("data", "intermediate", "DE.results.fungi.SEvsB.C.RDS"))
#Get number of DE transcripts
length(DE.results.fungi.SEvsB.C$DEfound) #202
DE.counts.res[nrow(DE.counts.res)+1,] = c("SE vs. B", "C", "Fungi", length(DE.results.fungi.SEvsB.C$DEfound)) #append to DE.counts.res
rm(DE.results.fungi.SEvsB.C)
rm(MT.fungi.B.C)
rm(MT.fungi.SE.C)

#Drought at H (C), B vs H
MT.fungi.B.C <- readRDS(file = here("data", "intermediate", "MT.fungi.B.C.RDS"))
MT.fungi.H.C <- readRDS(file = here("data", "intermediate", "MT.fungi.H.C.RDS"))
DE.results.fungi.BvsH.C <- DE(MT.fungi.B.C,MT.fungi.H.C)
saveRDS(DE.results.fungi.BvsH.C, file = here("data", "intermediate", "DE.results.fungi.BvsH.C.RDS"))
#Get number of DE transcripts
length(DE.results.fungi.BvsH.C$DEfound) #163
DE.counts.res[nrow(DE.counts.res)+1,] = c("B vs. H", "C", "Fungi", length(DE.results.fungi.BvsH.C$DEfound)) #append to DE.counts.res
rm(DE.results.fungi.BvsH.C)
rm(MT.fungi.B.C)
rm(MT.fungi.H.C)

#Control (D), T vs SE
MT.fungi.T.D <- readRDS(file = here("data", "intermediate", "MT.fungi.T.D.RDS"))
MT.fungi.SE.D <- readRDS(file = here("data", "intermediate", "MT.fungi.SE.D.RDS"))
DE.results.fungi.TvsSE.D <- DE(MT.fungi.T.D,MT.fungi.SE.D)
saveRDS(DE.results.fungi.TvsSE.D, file = here("data", "intermediate", "DE.results.fungi.TvsSE.D.RDS"))
#Get number of DE transcripts
length(DE.results.fungi.TvsSE.D$DEfound) #142
DE.counts.res[nrow(DE.counts.res)+1,] = c("T vs. SE", "D", "Fungi", length(DE.results.fungi.TvsSE.D$DEfound)) #append to DE.counts.res
rm(DE.results.fungi.TvsSE.D)
rm(MT.fungi.T.D)
rm(MT.fungi.SE.D)

##Bacteria
#Drought at SE (A), T vs SE
MT.bact.T.A <- readRDS(file = here("data", "intermediate", "MT.bact.T.A.RDS"))
MT.bact.SE.A <- readRDS(file = here("data", "intermediate", "MT.bact.SE.A.RDS"))
DE.results.bact.TvsSE.A <- DE(MT.bact.T.A,MT.bact.SE.A)
saveRDS(DE.results.bact.TvsSE.A, file = here("data", "intermediate", "DE.results.bact.TvsSE.A.RDS"))
#Get number of DE transcripts
length(DE.results.bact.TvsSE.A$DEfound) #17604
DE.counts.res[nrow(DE.counts.res)+1,] = c("T vs. SE", "A", "Bacteria", length(DE.results.bact.TvsSE.A$DEfound)) #append to DE.counts.res
rm(DE.results.bact.TvsSE.A)
rm(MT.bact.T.A)
rm(MT.bact.SE.A)

#Drought at SE (A), B vs H
MT.bact.B.A <- readRDS(file = here("data", "intermediate", "MT.bact.B.A.RDS"))
MT.bact.H.A <- readRDS(file = here("data", "intermediate", "MT.bact.H.A.RDS"))
DE.results.bact.BvsH.A <- DE(MT.bact.B.A,MT.bact.H.A)
saveRDS(DE.results.bact.BvsH.A, file = here("data", "intermediate", "DE.results.bact.BvsH.A.RDS"))
#Get number of DE transcripts
length(DE.results.bact.BvsH.A$DEfound) #224125
DE.counts.res[nrow(DE.counts.res)+1,] = c("B vs. H", "A", "Bacteria", length(DE.results.bact.BvsH.A$DEfound)) #append to DE.counts.res
rm(DE.results.bact.BvsH.A)
rm(MT.bact.B.A)
rm(MT.bact.H.A)

#Drought at SE (A), H vs F
MT.bact.H.A <- readRDS(file = here("data", "intermediate", "MT.bact.H.A.RDS"))
MT.bact.F.A <- readRDS(file = here("data", "intermediate", "MT.bact.F.A.RDS"))
DE.results.bact.HvsF.A <- DE(MT.bact.H.A,MT.bact.F.A)
saveRDS(DE.results.bact.HvsF.A, file = here("data", "intermediate", "DE.results.bact.HvsF.A.RDS"))
#Get number of DE transcripts
length(DE.results.bact.HvsF.A$DEfound) #98532
DE.counts.res[nrow(DE.counts.res)+1,] = c("H vs. F", "A", "Bacteria", length(DE.results.bact.HvsF.A$DEfound)) #append to DE.counts.res
rm(DE.results.bact.HvsF.A)
rm(MT.bact.H.A)
rm(MT.bact.F.A)

#Drought at B (B), T vs SE
MT.bact.T.B <- readRDS(file = here("data", "intermediate", "MT.bact.T.B.RDS"))
MT.bact.SE.B <- readRDS(file = here("data", "intermediate", "MT.bact.SE.B.RDS"))
DE.results.bact.TvsSE.B <- DE(MT.bact.T.B,MT.bact.SE.B)
saveRDS(DE.results.bact.TvsSE.B, file = here("data", "intermediate", "DE.results.bact.TvsSE.B.RDS"))
#Get number of DE transcripts
length(DE.results.bact.TvsSE.B$DEfound) #14187
DE.counts.res[nrow(DE.counts.res)+1,] = c("T vs. SE", "B", "Bacteria", length(DE.results.bact.TvsSE.B$DEfound)) #append to DE.counts.res
rm(DE.results.bact.TvsSE.B)
rm(MT.bact.T.B)
rm(MT.bact.SE.B)

#Drought at B (B), SE vs B
MT.bact.B.B <- readRDS(file = here("data", "intermediate", "MT.bact.B.B.RDS"))
MT.bact.SE.B <- readRDS(file = here("data", "intermediate", "MT.bact.SE.B.RDS"))
DE.results.bact.SEvsB.B <- DE(MT.bact.B.B,MT.bact.SE.B)
saveRDS(DE.results.bact.SEvsB.B, file = here("data", "intermediate", "DE.results.bact.SEvsB.B.RDS"))
#Get number of DE transcripts
length(DE.results.bact.SEvsB.B$DEfound) #43717
DE.counts.res[nrow(DE.counts.res)+1,] = c("SE vs. B", "B", "Bacteria", length(DE.results.bact.SEvsB.B$DEfound)) #append to DE.counts.res
rm(DE.results.bact.SEvsB.B)
rm(MT.bact.B.B)
rm(MT.bact.SE.B)

#Drought at B (B), H vs F
MT.bact.H.B <- readRDS(file = here("data", "intermediate", "MT.bact.H.B.RDS"))
MT.bact.F.B <- readRDS(file = here("data", "intermediate", "MT.bact.F.B.RDS"))
DE.results.bact.HvsF.B <- DE(MT.bact.H.B,MT.bact.F.B)
saveRDS(DE.results.bact.HvsF.B, file = here("data", "intermediate", "DE.results.bact.HvsF.B.RDS"))
#Get number of DE transcripts
length(DE.results.bact.HvsF.B$DEfound) #32845
DE.counts.res[nrow(DE.counts.res)+1,] = c("H vs. F", "B", "Bacteria", length(DE.results.bact.HvsF.B$DEfound)) #append to DE.counts.res
rm(DE.results.bact.HvsF.B)
rm(MT.bact.H.B)
rm(MT.bact.F.B)

#Drought at H (C), T vs SE
MT.bact.T.C <- readRDS(file = here("data", "intermediate", "MT.bact.T.C.RDS"))
MT.bact.SE.C <- readRDS(file = here("data", "intermediate", "MT.bact.SE.C.RDS"))
DE.results.bact.TvsSE.C <- DE(MT.bact.T.C,MT.bact.SE.C)
saveRDS(DE.results.bact.TvsSE.C, file = here("data", "intermediate", "DE.results.bact.TvsSE.C.RDS"))
#Get number of DE transcripts
length(DE.results.bact.TvsSE.C$DEfound) #15427
DE.counts.res[nrow(DE.counts.res)+1,] = c("T vs. SE", "C", "Bacteria", length(DE.results.bact.TvsSE.C$DEfound)) #append to DE.counts.res
rm(DE.results.bact.TvsSE.C)
rm(MT.bact.T.C)
rm(MT.bact.SE.C)

#Drought at H (C), SE vs B
MT.bact.B.C <- readRDS(file = here("data", "intermediate", "MT.bact.B.C.RDS"))
MT.bact.SE.C <- readRDS(file = here("data", "intermediate", "MT.bact.SE.C.RDS"))
DE.results.bact.SEvsB.C <- DE(MT.bact.B.C,MT.bact.SE.C)
saveRDS(DE.results.bact.SEvsB.C, file = here("data", "intermediate", "DE.results.bact.SEvsB.C.RDS"))
#Get number of DE transcripts
length(DE.results.bact.SEvsB.C$DEfound) #18010
DE.counts.res[nrow(DE.counts.res)+1,] = c("SE vs. B", "C", "Bacteria", length(DE.results.bact.SEvsB.C$DEfound)) #append to DE.counts.res
rm(DE.results.bact.SEvsB.C)
rm(MT.bact.B.C)
rm(MT.bact.SE.C)

#Drought at H (C), B vs H
MT.bact.B.C <- readRDS(file = here("data", "intermediate", "MT.bact.B.C.RDS"))
MT.bact.H.C <- readRDS(file = here("data", "intermediate", "MT.bact.H.C.RDS"))
DE.results.bact.BvsH.C <- DE(MT.bact.B.C,MT.bact.H.C)
saveRDS(DE.results.bact.BvsH.C, file = here("data", "intermediate", "DE.results.bact.BvsH.C.RDS"))
#Get number of DE transcripts
length(DE.results.bact.BvsH.C$DEfound) #40009
DE.counts.res[nrow(DE.counts.res)+1,] = c("B vs. H", "C", "Bacteria", length(DE.results.bact.BvsH.C$DEfound)) #append to DE.counts.res
rm(DE.results.bact.BvsH.C)
rm(MT.bact.B.C)
rm(MT.bact.H.C)

#Control (D), T vs SE
MT.bact.T.D <- readRDS(file = here("data", "intermediate", "MT.bact.T.D.RDS"))
MT.bact.SE.D <- readRDS(file = here("data", "intermediate", "MT.bact.SE.D.RDS"))
DE.results.bact.TvsSE.D <- DE(MT.bact.T.D,MT.bact.SE.D)
saveRDS(DE.results.bact.TvsSE.D, file = here("data", "intermediate", "DE.results.bact.TvsSE.D.RDS"))
#Get number of DE transcripts
length(DE.results.bact.TvsSE.D$DEfound) #14296
DE.counts.res[nrow(DE.counts.res)+1,] = c("T vs. SE", "D", "Bacteria", length(DE.results.bact.TvsSE.D$DEfound)) #append to DE.counts.res
rm(DE.results.bact.TvsSE.D)
rm(MT.bact.T.D)
rm(MT.bact.SE.D)

##All
#Drought at SE (A), T vs SE
MT.T.A <- readRDS(file = here("data", "intermediate", "MT.T.A.RDS"))
MT.SE.A <- readRDS(file = here("data", "intermediate", "MT.SE.A.RDS"))
DE.results.TvsSE.A <- DE(MT.T.A,MT.SE.A)
saveRDS(DE.results.TvsSE.A, file = here("data", "intermediate", "DE.results.TvsSE.A.RDS"))
#Get number of DE transcripts
length(DE.results.TvsSE.A$DEfound) #28296
DE.counts.res[nrow(DE.counts.res)+1,] = c("T vs. SE", "A", "All", length(DE.results.TvsSE.A$DEfound)) #append to DE.counts.res
rm(DE.results.TvsSE.A)
rm(MT.T.A)
rm(MT.SE.A)

#Drought at SE (A), B vs H
MT.B.A <- readRDS(file = here("data", "intermediate", "MT.B.A.RDS"))
MT.H.A <- readRDS(file = here("data", "intermediate", "MT.H.A.RDS"))
DE.results.BvsH.A <- DE(MT.B.A,MT.H.A)
saveRDS(DE.results.BvsH.A, file = here("data", "intermediate", "DE.results.BvsH.A.RDS"))
#Get number of DE transcripts
length(DE.results.BvsH.A$DEfound) #287091
DE.counts.res[nrow(DE.counts.res)+1,] = c("B vs. H", "A", "All", length(DE.results.BvsH.A$DEfound)) #append to DE.counts.res
rm(DE.results.BvsH.A)
rm(MT.B.A)
rm(MT.H.A)

#Drought at SE (A), H vs F
MT.H.A <- readRDS(file = here("data", "intermediate", "MT.H.A.RDS"))
MT.F.A <- readRDS(file = here("data", "intermediate", "MT.F.A.RDS"))
DE.results.HvsF.A <- DE(MT.H.A,MT.F.A)
saveRDS(DE.results.HvsF.A, file = here("data", "intermediate", "DE.results.HvsF.A.RDS"))
#Get number of DE transcripts
length(DE.results.HvsF.A$DEfound) #150083
DE.counts.res[nrow(DE.counts.res)+1,] = c("H vs. F", "A", "All", length(DE.results.HvsF.A$DEfound)) #append to DE.counts.res
rm(DE.results.HvsF.A)
rm(MT.H.A)
rm(MT.F.A)

#Drought at B (B), T vs SE
MT.T.B <- readRDS(file = here("data", "intermediate", "MT.T.B.RDS"))
MT.SE.B <- readRDS(file = here("data", "intermediate", "MT.SE.B.RDS"))
DE.results.TvsSE.B <- DE(MT.T.B,MT.SE.B)
saveRDS(DE.results.TvsSE.B, file = here("data", "intermediate", "DE.results.TvsSE.B.RDS"))
#Get number of DE transcripts
length(DE.results.TvsSE.B$DEfound) #21554
DE.counts.res[nrow(DE.counts.res)+1,] = c("T vs. SE", "B", "All", length(DE.results.TvsSE.B$DEfound)) #append to DE.counts.res
rm(DE.results.TvsSE.B)
rm(MT.T.B)
rm(MT.SE.B)

#Drought at B (B), SE vs B
MT.B.B <- readRDS(file = here("data", "intermediate", "MT.B.B.RDS"))
MT.SE.B <- readRDS(file = here("data", "intermediate", "MT.SE.B.RDS"))
DE.results.SEvsB.B <- DE(MT.B.B,MT.SE.B)
saveRDS(DE.results.SEvsB.B, file = here("data", "intermediate", "DE.results.SEvsB.B.RDS"))
#Get number of DE transcripts
length(DE.results.SEvsB.B$DEfound) #68720
DE.counts.res[nrow(DE.counts.res)+1,] = c("SE vs. B", "B", "All", length(DE.results.SEvsB.B$DEfound)) #append to DE.counts.res
rm(DE.results.SEvsB.B)
rm(MT.B.B)
rm(MT.SE.B)

#Drought at B (B), H vs F
MT.H.B <- readRDS(file = here("data", "intermediate", "MT.H.B.RDS"))
MT.F.B <- readRDS(file = here("data", "intermediate", "MT.F.B.RDS"))
DE.results.HvsF.B <- DE(MT.H.B,MT.F.B)
saveRDS(DE.results.HvsF.B, file = here("data", "intermediate", "DE.results.HvsF.B.RDS"))
#Get number of DE transcripts
length(DE.results.HvsF.B$DEfound) #51672
DE.counts.res[nrow(DE.counts.res)+1,] = c("H vs. F", "B", "All", length(DE.results.HvsF.B$DEfound)) #append to DE.counts.res
rm(DE.results.HvsF.B)
rm(MT.H.B)
rm(MT.F.B)

#Drought at H (C), T vs SE
MT.T.C <- readRDS(file = here("data", "intermediate", "MT.T.C.RDS"))
MT.SE.C <- readRDS(file = here("data", "intermediate", "MT.SE.C.RDS"))
DE.results.TvsSE.C <- DE(MT.T.C,MT.SE.C)
saveRDS(DE.results.TvsSE.C, file = here("data", "intermediate", "DE.results.TvsSE.C.RDS"))
#Get number of DE transcripts
length(DE.results.TvsSE.C$DEfound) #22991
DE.counts.res[nrow(DE.counts.res)+1,] = c("T vs. SE", "C", "All", length(DE.results.TvsSE.C$DEfound)) #append to DE.counts.res
rm(DE.results.TvsSE.C)
rm(MT.T.C)
rm(MT.SE.C)

#Drought at H (C), SE vs B
MT.B.C <- readRDS(file = here("data", "intermediate", "MT.B.C.RDS"))
MT.SE.C <- readRDS(file = here("data", "intermediate", "MT.SE.C.RDS"))
DE.results.SEvsB.C <- DE(MT.B.C,MT.SE.C)
saveRDS(DE.results.SEvsB.C, file = here("data", "intermediate", "DE.results.SEvsB.C.RDS"))
#Get number of DE transcripts
length(DE.results.SEvsB.C$DEfound) #29475
DE.counts.res[nrow(DE.counts.res)+1,] = c("SE vs. B", "C", "All", length(DE.results.SEvsB.C$DEfound)) #append to DE.counts.res
rm(DE.results.SEvsB.C)
rm(MT.B.C)
rm(MT.SE.C)

#Drought at H (C), B vs H
MT.B.C <- readRDS(file = here("data", "intermediate", "MT.B.C.RDS"))
MT.H.C <- readRDS(file = here("data", "intermediate", "MT.H.C.RDS"))
DE.results.BvsH.C <- DE(MT.B.C,MT.H.C)
saveRDS(DE.results.BvsH.C, file = here("data", "intermediate", "DE.results.BvsH.C.RDS"))
#Get number of DE transcripts
length(DE.results.BvsH.C$DEfound) #62413
DE.counts.res[nrow(DE.counts.res)+1,] = c("B vs. H", "C", "All", length(DE.results.BvsH.C$DEfound)) #append to DE.counts.res
rm(DE.results.BvsH.C)
rm(MT.B.C)
rm(MT.H.C)

#Control (D), T vs SE
MT.T.D <- readRDS(file = here("data", "intermediate", "MT.T.D.RDS"))
MT.SE.D <- readRDS(file = here("data", "intermediate", "MT.SE.D.RDS"))
DE.results.TvsSE.D <- DE(MT.T.D,MT.SE.D)
saveRDS(DE.results.TvsSE.D, file = here("data", "intermediate", "DE.results.TvsSE.D.RDS"))
#Get number of DE transcripts
length(DE.results.TvsSE.D$DEfound) #22633
DE.counts.res[nrow(DE.counts.res)+1,] = c("T vs. SE", "D", "All", length(DE.results.TvsSE.D$DEfound)) #append to DE.counts.res
rm(DE.results.TvsSE.D)
rm(MT.T.D)
rm(MT.SE.D)

#Save the DE count file
saveRDS(DE.counts.res, file = here("data", "intermediate", "DE.counts.res.RDS"))

