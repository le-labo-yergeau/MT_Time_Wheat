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

##All genes
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
length(DE.results.SE.AvsD$DEfound) #0000
rm(MT.SE.A)

#B vs D
#Load data
MT.SE.B <- readRDS(file = here("data", "intermediate", "MT.SE.B.RDS"))
#use the DE function to get results
DE.results.SE.BvsD <- DE(MT.SE.B,MT.SE.D)
saveRDS(DE.results.SE.BvsD, file = here("data", "intermediate", "DE.results.SE.BvsD.RDS"))
#Get number of DE transcripts
length(DE.results.SE.BvsD$DEfound) #0000
rm(MT.SE.B)

#C vs D
#Load data
MT.SE.C <- readRDS(file = here("data", "intermediate", "MT.SE.C.RDS"))
#use the DE function to get results
DE.results.SE.CvsD <- DE(MT.SE.C,MT.SE.D)
saveRDS(DE.results.SE.CvsD, file = here("data", "intermediate", "DE.results.SE.CvsD.RDS"))
#Get number of DE transcripts
length(DE.results.SE.CvsD$DEfound) #0000
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
length(DE.results.H.AvsD$DEfound) #0000
rm(MT.H.A)

#B vs D
#Load data
MT.H.B <- readRDS(file = here("data", "intermediate", "MT.H.B.RDS"))
#use the DE function to get results
DE.results.H.BvsD <- DE(MT.H.B,MT.H.D)
saveRDS(DE.results.H.BvsD, file = here("data", "intermediate", "DE.results.H.BvsD.RDS"))
#Get number of DE transcripts
length(DE.results.H.BvsD$DEfound) #0000
rm(MT.H.B)

#C vs D
#Load data
MT.H.C <- readRDS(file = here("data", "intermediate", "MT.H.C.RDS"))
#use the DE function to get results
DE.results.H.CvsD <- DE(MT.H.C,MT.H.D)
saveRDS(DE.results.H.CvsD, file = here("data", "intermediate", "DE.results.H.CvsD.RDS"))
#Get number of DE transcripts
length(DE.results.H.CvsD$DEfound) #0000
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
length(DE.results.F.AvsD$DEfound) #0000
rm(MT.F.A)

#B vs D
#Load data
MT.F.B <- readRDS(file = here("data", "intermediate", "MT.F.B.RDS"))
#use the DE function to get results
DE.results.F.BvsD <- DE(MT.F.B,MT.F.D)
saveRDS(DE.results.F.BvsD, file = here("data", "intermediate", "DE.results.F.BvsD.RDS"))
#Get number of DE transcripts
length(DE.results.F.BvsD$DEfound) #0000
rm(MT.F.B)

#C vs D
#Load data
MT.F.C <- readRDS(file = here("data", "intermediate", "MT.F.C.RDS"))
#use the DE function to get results
DE.results.F.CvsD <- DE(MT.F.C,MT.F.D)
saveRDS(DE.results.F.CvsD, file = here("data", "intermediate", "DE.results.F.CvsD.RDS"))
#Get number of DE transcripts
length(DE.results.F.CvsD$DEfound) #0000
rm(MT.F.C)
rm(MT.F.D)
