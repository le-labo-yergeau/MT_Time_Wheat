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
length(DE.results.B.AvsD$DEfound) #0000
rm(MT.B.A)

#B vs D
#Load data
MT.B.B <- readRDS(file = here("data", "intermediate", "MT.B.B.RDS"))
#use the DE function to get results
DE.results.B.BvsD <- DE(MT.B.B,MT.B.D)
saveRDS(DE.results.B.BvsD, file = here("data", "intermediate", "DE.results.B.BvsD.RDS"))
#Get number of DE transcripts
length(DE.results.B.BvsD$DEfound) #0000
rm(MT.B.B)

#C vs D
#Load data
MT.B.C <- readRDS(file = here("data", "intermediate", "MT.B.C.RDS"))
#use the DE function to get results
DE.results.B.CvsD <- DE(MT.B.C,MT.B.D)
saveRDS(DE.results.B.CvsD, file = here("data", "intermediate", "DE.results.B.CvsD.RDS"))
#Get number of DE transcripts
length(DE.results.B.CvsD$DEfound) #0000
rm(MT.B.C)
rm(MT.B.D)


