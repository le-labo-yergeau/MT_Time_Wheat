###DE analyses for all pairs of treatments x stage
#Run on the cluster using DE.sh: from the src folder: sbatch DE.sh
#To work on cluster, need to load packages here (packages need to be installed: use interactive mode)
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
length(DE.results.bact.T.AvsD$DEfound) #0000
rm(MT.bact.T.A)

#B vs D
#Load data
MT.bact.T.B <- readRDS(file = here("data", "intermediate", "MT.bact.T.B.RDS"))
#use the DE function to get results
DE.results.bact.T.BvsD <- DE(MT.bact.T.B,MT.bact.T.D)
saveRDS(DE.results.bact.T.BvsD, file = here("data", "intermediate", "DE.results.bact.T.BvsD.RDS"))
#Get number of DE transcripts
length(DE.results.bact.T.BvsD$DEfound) #0000
rm(MT.bact.T.B)

#C vs D
#Load data
MT.bact.T.C <- readRDS(file = here("data", "intermediate", "MT.bact.T.C.RDS"))
#use the DE function to get results
DE.results.bact.T.CvsD <- DE(MT.bact.T.C,MT.bact.T.D)
saveRDS(DE.results.bact.T.CvsD, file = here("data", "intermediate", "DE.results.bact.T.CvsD.RDS"))
#Get number of DE transcripts
length(DE.results.bact.T.CvsD$DEfound) #0000
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
length(DE.results.bact.B.AvsD$DEfound) #0000
rm(MT.bact.B.A)

#B vs D
#Load data
MT.bact.B.B <- readRDS(file = here("data", "intermediate", "MT.bact.B.B.RDS"))
#use the DE function to get results
DE.results.bact.B.BvsD <- DE(MT.bact.B.B,MT.bact.B.D)
saveRDS(DE.results.bact.B.BvsD, file = here("data", "intermediate", "DE.results.bact.B.BvsD.RDS"))
#Get number of DE transcripts
length(DE.results.bact.B.BvsD$DEfound) #0000
rm(MT.bact.B.B)

#C vs D
#Load data
MT.bact.B.C <- readRDS(file = here("data", "intermediate", "MT.bact.B.C.RDS"))
#use the DE function to get results
DE.results.bact.B.CvsD <- DE(MT.bact.B.C,MT.bact.B.D)
saveRDS(DE.results.bact.B.CvsD, file = here("data", "intermediate", "DE.results.bact.B.CvsD.RDS"))
#Get number of DE transcripts
length(DE.results.bact.B.CvsD$DEfound) #0000
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
length(DE.results.bact.SE.AvsD$DEfound) #0000
rm(MT.bact.SE.A)

#B vs D
#Load data
MT.bact.SE.B <- readRDS(file = here("data", "intermediate", "MT.bact.SE.B.RDS"))
#use the DE function to get results
DE.results.bact.SE.BvsD <- DE(MT.bact.SE.B,MT.bact.SE.D)
saveRDS(DE.results.bact.SE.BvsD, file = here("data", "intermediate", "DE.results.bact.SE.BvsD.RDS"))
#Get number of DE transcripts
length(DE.results.bact.SE.BvsD$DEfound) #0000
rm(MT.bact.SE.B)

#C vs D
#Load data
MT.bact.SE.C <- readRDS(file = here("data", "intermediate", "MT.bact.SE.C.RDS"))
#use the DE function to get results
DE.results.bact.SE.CvsD <- DE(MT.bact.SE.C,MT.bact.SE.D)
saveRDS(DE.results.bact.SE.CvsD, file = here("data", "intermediate", "DE.results.bact.SE.CvsD.RDS"))
#Get number of DE transcripts
length(DE.results.bact.SE.CvsD$DEfound) #0000
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
length(DE.results.bact.H.AvsD$DEfound) #0000
rm(MT.bact.H.A)

#B vs D
#Load data
MT.bact.H.B <- readRDS(file = here("data", "intermediate", "MT.bact.H.B.RDS"))
#use the DE function to get results
DE.results.bact.H.BvsD <- DE(MT.bact.H.B,MT.bact.H.D)
saveRDS(DE.results.bact.H.BvsD, file = here("data", "intermediate", "DE.results.bact.H.BvsD.RDS"))
#Get number of DE transcripts
length(DE.results.bact.H.BvsD$DEfound) #0000
rm(MT.bact.H.B)

#C vs D
#Load data
MT.bact.H.C <- readRDS(file = here("data", "intermediate", "MT.bact.H.C.RDS"))
#use the DE function to get results
DE.results.bact.H.CvsD <- DE(MT.bact.H.C,MT.bact.H.D)
saveRDS(DE.results.bact.H.CvsD, file = here("data", "intermediate", "DE.results.bact.H.CvsD.RDS"))
#Get number of DE transcripts
length(DE.results.bact.H.CvsD$DEfound) #0000
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
length(DE.results.bact.F.AvsD$DEfound) #0000
rm(MT.bact.F.A)

#B vs D
#Load data
MT.bact.F.B <- readRDS(file = here("data", "intermediate", "MT.bact.F.B.RDS"))
#use the DE function to get results
DE.results.bact.F.BvsD <- DE(MT.bact.F.B,MT.bact.F.D)
saveRDS(DE.results.bact.F.BvsD, file = here("data", "intermediate", "DE.results.bact.F.BvsD.RDS"))
#Get number of DE transcripts
length(DE.results.bact.F.BvsD$DEfound) #0000
rm(MT.bact.F.B)

#C vs D
#Load data
MT.bact.F.C <- readRDS(file = here("data", "intermediate", "MT.bact.F.C.RDS"))
#use the DE function to get results
DE.results.bact.F.CvsD <- DE(MT.bact.F.C,MT.bact.F.D)
saveRDS(DE.results.bact.F.CvsD, file = here("data", "intermediate", "DE.results.bact.F.CvsD.RDS"))
#Get number of DE transcripts
length(DE.results.bact.F.CvsD$DEfound) #0000
rm(MT.bact.F.C)
rm(MT.bact.F.D)


