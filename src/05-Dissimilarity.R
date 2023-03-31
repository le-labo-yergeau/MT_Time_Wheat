###Test resilience vs. resistance
##Resistance = equal or less change in similarity between before and after drying than control
##Resilience = larger change in similarity between before and after drying than control, but return to normal after
##Alternative state = larger change in similarity between before and after drying than control, no return to normal
##If alternative state, does it mean change in functionality? (OTHER SCRIPT, MAYBE?)

#Import objects
map <- readRDS(file = here("data","intermediate", "map.RDS"))
bray.s <- readRDS(file = here("data","intermediate", "bray.s.RDS"))
bray.fungi.s <-readRDS(file = here("data","intermediate", "bray.fungi.s.RDS"))
bray.bact.s <- readRDS(file = here("data","intermediate", "bray.bact.s.RDS"))

#Sort and double check
map.s <- map[order(row.names(map)),]
sum(row.names(bray.s) == row.names(map.s))#120
sum(row.names(bray.s) == colnames(bray.s))#120
sum(row.names(bray.bact.s) == row.names(map.s))#120
sum(row.names(bray.bact.s) == colnames(bray.bact.s))#120
sum(row.names(bray.fungi.s) == row.names(map.s))#120
sum(row.names(bray.fungi.s) == colnames(bray.fungi.s))#120

##All
#Create a dataframe comparing the similarity within blocks of different growth stages.
#Control (D)  (compare within block)
i<-1
rm(TSE)
TSE<-vector()
while (i<7){
  TSE<-append(TSE,bray.s[(map.s$treatment2=="D" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEB)
SEB<-vector()
while (i<7){
  SEB<-append(SEB,bray.s[(map.s$treatment2=="D" & map.s$growthstage=="booting" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BH)
BH<-vector()
while (i<7){
  BH<-append(BH,bray.s[(map.s$treatment2=="D" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="booting" & map.s$block==i)]);
  i<-i+1;
}


i<-1
rm(HF)
HF<-vector()
while (i<7){
  HF<-append(HF,bray.s[(map.s$treatment2=="D" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}


i<-1
rm(TB)
TB<-vector()
while (i<7){
  TB<-append(TB,bray.s[(map.s$treatment2=="D" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="booting" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(TH)
TH<-vector()
while (i<7){
  TH<-append(TH,bray.s[(map.s$treatment2=="D" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="heading" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(TF)
TF<-vector()
while (i<7){
  TF<-append(TF,bray.s[(map.s$treatment2=="D" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEH)
SEH<-vector()
while (i<7){
  SEH<-append(SEH,bray.s[(map.s$treatment2=="D" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEF)
SEF<-vector()
while (i<7){
  SEF<-append(SEF,bray.s[(map.s$treatment2=="D" & map.s$growthstage=="flowering" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BF)
BF<-vector()
while (i<7){
  BF<-append(BF,bray.s[(map.s$treatment2=="D" & map.s$growthstage=="booting" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}

bray.treat<-data.frame(c(rep("TSE",6), rep("TB",6), rep("TH",6), rep("TF",6), rep("SEB",6), rep("SEH",6), rep("SEF",6), rep("BH",6), rep("BF",6), rep("HF",6)), c(rep(1:6,10)), c(TSE, TB, TH, TF, SEB, SEH, SEF, BH, BF, HF))

#Stem elongation drought (A)  (compare within block)
i<-1
rm(TSE)
TSE<-vector()
while (i<7){
  TSE<-append(TSE,bray.s[(map.s$treatment2=="A" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="A" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEB)
SEB<-vector()
while (i<7){
  SEB<-append(SEB,bray.s[(map.s$treatment2=="A" & map.s$growthstage=="booting" & map.s$block==i), (map.s$treatment2=="A" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BH)
BH<-vector()
while (i<7){
  BH<-append(BH,bray.s[(map.s$treatment2=="A" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="A" & map.s$growthstage=="booting" & map.s$block==i)]);
  i<-i+1;
}


i<-1
rm(HF)
HF<-vector()
while (i<7){
  HF<-append(HF,bray.s[(map.s$treatment2=="A" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="A" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}


i<-1
rm(TB)
TB<-vector()
while (i<7){
  TB<-append(TB,bray.s[(map.s$treatment2=="A" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="A" & map.s$growthstage=="booting" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(TH)
TH<-vector()
while (i<7){
  TH<-append(TH,bray.s[(map.s$treatment2=="A" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="A" & map.s$growthstage=="heading" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(TF)
TF<-vector()
while (i<7){
  TF<-append(TF,bray.s[(map.s$treatment2=="A" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="A" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEH)
SEH<-vector()
while (i<7){
  SEH<-append(SEH,bray.s[(map.s$treatment2=="A" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="A" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEF)
SEF<-vector()
while (i<7){
  SEF<-append(SEF,bray.s[(map.s$treatment2=="A" & map.s$growthstage=="flowering" & map.s$block==i), (map.s$treatment2=="A" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BF)
BF<-vector()
while (i<7){
  BF<-append(BF,bray.s[(map.s$treatment2=="A" & map.s$growthstage=="booting" & map.s$block==i), (map.s$treatment2=="A" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}

bray.treat<-data.frame(bray.treat, c(TSE, TB, TH, TF, SEB, SEH, SEF, BH, BF, HF))

#Booting drought (B)  (compare within block)
i<-1
rm(TSE)
TSE<-vector()
while (i<7){
  TSE<-append(TSE,bray.s[(map.s$treatment2=="B" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="B" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEB)
SEB<-vector()
while (i<7){
  SEB<-append(SEB,bray.s[(map.s$treatment2=="B" & map.s$growthstage=="booting" & map.s$block==i), (map.s$treatment2=="B" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BH)
BH<-vector()
while (i<7){
  BH<-append(BH,bray.s[(map.s$treatment2=="B" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="B" & map.s$growthstage=="booting" & map.s$block==i)]);
  i<-i+1;
}


i<-1
rm(HF)
HF<-vector()
while (i<7){
  HF<-append(HF,bray.s[(map.s$treatment2=="B" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="B" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}


i<-1
rm(TB)
TB<-vector()
while (i<7){
  TB<-append(TB,bray.s[(map.s$treatment2=="B" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="B" & map.s$growthstage=="booting" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(TH)
TH<-vector()
while (i<7){
  TH<-append(TH,bray.s[(map.s$treatment2=="B" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="B" & map.s$growthstage=="heading" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(TF)
TF<-vector()
while (i<7){
  TF<-append(TF,bray.s[(map.s$treatment2=="B" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="B" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEH)
SEH<-vector()
while (i<7){
  SEH<-append(SEH,bray.s[(map.s$treatment2=="B" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="B" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEF)
SEF<-vector()
while (i<7){
  SEF<-append(SEF,bray.s[(map.s$treatment2=="B" & map.s$growthstage=="flowering" & map.s$block==i), (map.s$treatment2=="B" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BF)
BF<-vector()
while (i<7){
  BF<-append(BF,bray.s[(map.s$treatment2=="B" & map.s$growthstage=="booting" & map.s$block==i), (map.s$treatment2=="B" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}

bray.treat<-data.frame(bray.treat, c(TSE, TB, TH, TF, SEB, SEH, SEF, BH, BF, HF))

#Heading drought (C)  (compare within block)
i<-1
rm(TSE)
TSE<-vector()
while (i<7){
  TSE<-append(TSE,bray.s[(map.s$treatment2=="C" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="C" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEB)
SEB<-vector()
while (i<7){
  SEB<-append(SEB,bray.s[(map.s$treatment2=="C" & map.s$growthstage=="booting" & map.s$block==i), (map.s$treatment2=="C" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BH)
BH<-vector()
while (i<7){
  BH<-append(BH,bray.s[(map.s$treatment2=="C" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="C" & map.s$growthstage=="booting" & map.s$block==i)]);
  i<-i+1;
}


i<-1
rm(HF)
HF<-vector()
while (i<7){
  HF<-append(HF,bray.s[(map.s$treatment2=="C" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="C" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}


i<-1
rm(TB)
TB<-vector()
while (i<7){
  TB<-append(TB,bray.s[(map.s$treatment2=="C" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="C" & map.s$growthstage=="booting" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(TH)
TH<-vector()
while (i<7){
  TH<-append(TH,bray.s[(map.s$treatment2=="C" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="C" & map.s$growthstage=="heading" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(TF)
TF<-vector()
while (i<7){
  TF<-append(TF,bray.s[(map.s$treatment2=="C" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="C" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEH)
SEH<-vector()
while (i<7){
  SEH<-append(SEH,bray.s[(map.s$treatment2=="C" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="C" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEF)
SEF<-vector()
while (i<7){
  SEF<-append(SEF,bray.s[(map.s$treatment2=="C" & map.s$growthstage=="flowering" & map.s$block==i), (map.s$treatment2=="C" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BF)
BF<-vector()
while (i<7){
  BF<-append(BF,bray.s[(map.s$treatment2=="C" & map.s$growthstage=="booting" & map.s$block==i), (map.s$treatment2=="C" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}

bray.treat<-data.frame(bray.treat, c(TSE, TB, TH, TF, SEB, SEH, SEF, BH, BF, HF))
colnames(bray.treat)<-c("Treatments","Block", "CTRL-D", "SE-A", "B-B", "H-C")

#Make the comparisons with stat tests
#Drought at stem elongation (A), T vs. SE
t.test(bray.treat[1:6,3], bray.treat[1:6,4], paired=T) #t = -0.61954, df = 5, p-value = 0.5627
mean(bray.treat[1:6,3]) #0.5006703 -- control
mean(bray.treat[1:6,4]) #0.5140696 -- treatment A
simil <- data.frame(Contrast="T vs. SE", Treatment="A", Subset="All", Dissimilarity=mean(bray.treat[1:6,4]))#create the dataframe
simil[nrow(simil)+1,] = c("T vs. SE", "D", "All", mean(bray.treat[1:6,3])) #append to simil

#Drought at stem elongation (A), SE vs.B (right after)
t.test(bray.treat[25:30,3], bray.treat[25:30,4], paired=T) #t = -0.78968, df = 5, p-value = 0.4655
mean(bray.treat[25:30,3]) #0.5281895 -- control
mean(bray.treat[25:30,4]) #0.5704785 -- treatment A
simil[nrow(simil)+1,] = c("SE vs. B", "A", "All", mean(bray.treat[25:30,4])) #append to simil
simil[nrow(simil)+1,] = c("SE vs. B", "D", "All", mean(bray.treat[25:30,3])) #append to simil

#Drought at stem elongation (A), B vs. H
t.test(bray.treat[43:48,3], bray.treat[43:48,4], paired=T) #t = -0.768, df = 5, p-value = 0.4772
mean(bray.treat[43:48,3]) #0.519054 -- control
mean(bray.treat[43:48,4]) #0.5425106 -- treatment A
simil[nrow(simil)+1,] = c("B vs. H", "A", "All", mean(bray.treat[43:48,4])) #append to simil
simil[nrow(simil)+1,] = c("B vs. H", "D", "All", mean(bray.treat[43:48,3])) #append to simil

#Drought at stem elongation (A), H vs. F
t.test(bray.treat[55:60,3], bray.treat[55:60,4], paired=T) #t = 0.48494, df = 5, p-value = 0.6482
mean(bray.treat[55:60,3]) #0.6404647 -- control
mean(bray.treat[55:60,4]) #0.6182934 -- treatment A
simil[nrow(simil)+1,] = c("H vs. F", "A", "All", mean(bray.treat[55:60,4])) #append to simil
simil[nrow(simil)+1,] = c("H vs. F", "D", "All", mean(bray.treat[55:60,3])) #append to simil

#Drought at booting (B), T vs. SE
t.test(bray.treat[1:6,3], bray.treat[1:6,5], paired=T) #t = -0.20232, df = 5, p-value = 0.8476
mean(bray.treat[1:6,3]) #0.5006703 -- control
mean(bray.treat[1:6,5]) #0.503739 -- treatment B
simil[nrow(simil)+1,] = c("T vs. SE", "B", "All", mean(bray.treat[1:6,5])) #apend to simil

#Drought at booting (B), SE vs.B
t.test(bray.treat[25:30,3], bray.treat[25:30,5], paired=T) #t = -1.733, df = 5, p-value = 0.1436
mean(bray.treat[25:30,3]) #0.5281895 -- control
mean(bray.treat[25:30,5]) #0.597661 -- treatment B
simil[nrow(simil)+1,] = c("SE vs. B", "B", "All", mean(bray.treat[25:30,5])) #append to simil

#Drought at booting (B), B vs. H
t.test(bray.treat[43:48,3], bray.treat[43:48,5], paired=T) #t = -1.915, df = 5, p-value = 0.1137
mean(bray.treat[43:48,3]) #0.519054 -- control
mean(bray.treat[43:48,5]) #0.5692423 -- treatment B
simil[nrow(simil)+1,] = c("B vs. H", "B", "All", mean(bray.treat[43:48,5])) #append to simil

#Drought at booting (B), H vs. F
t.test(bray.treat[55:60,3], bray.treat[55:60,5], paired=T) #t = 1.2438, df = 5, p-value = 0.2687
mean(bray.treat[55:60,3]) #0.6404647 -- control
mean(bray.treat[55:60,5]) #0.575066 -- treatment B
simil[nrow(simil)+1,] = c("H vs. F", "B", "All", mean(bray.treat[55:60,5])) #append to simil

#Drought at heading (C), T vs. SE
t.test(bray.treat[1:6,3], bray.treat[1:6,6], paired=T) #t = -0.22615, df = 5, p-value = 0.83
mean(bray.treat[1:6,3]) #0.5006703 -- control
mean(bray.treat[1:6,6]) #0.5047335 -- treatment C
simil[nrow(simil)+1,] = c("T vs. SE", "C", "All", mean(bray.treat[1:6,6])) #apend to simil

#Drought at heading (C), SE vs.B
t.test(bray.treat[25:30,3], bray.treat[25:30,6], paired=T) #t = -1.2135, df = 5, p-value = 0.2791
mean(bray.treat[25:30,3]) #0.5281895 -- control
mean(bray.treat[25:30,6]) #0.5579076 -- treatment C
simil[nrow(simil)+1,] = c("SE vs. B", "C", "All", mean(bray.treat[25:30,6])) #append to simil

#Drought at heading (C), B vs. H
t.test(bray.treat[43:48,3], bray.treat[43:48,6], paired=T) #t = -1.8717, df = 5, p-value = 0.1201
mean(bray.treat[43:48,3]) #0.519054 -- control
mean(bray.treat[43:48,6]) #0.5614228 -- treatment C
simil[nrow(simil)+1,] = c("B vs. H", "C", "All", mean(bray.treat[43:48,6])) #append to simil

#Drought at heading (C), H vs. F
t.test(bray.treat[55:60,3], bray.treat[55:60,6], paired=T) #t = -0.26832, df = 5, p-value = 0.7992
mean(bray.treat[55:60,3]) #0.6404647 -- control
mean(bray.treat[55:60,6]) #0.6503077 -- treatment C
simil[nrow(simil)+1,] = c("H vs. F", "C", "All", mean(bray.treat[55:60,6])) #append to simil

###Lets's check for fungi and bacteria individually

##Fungi
#Create a dataframe comparing the similarity within blocks of different growth stages.
#Control (D)  (compare within block)
i<-1
rm(TSE)
TSE<-vector()
while (i<7){
  TSE<-append(TSE,bray.fungi.s[(map.s$treatment2=="D" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEB)
SEB<-vector()
while (i<7){
  SEB<-append(SEB,bray.fungi.s[(map.s$treatment2=="D" & map.s$growthstage=="booting" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BH)
BH<-vector()
while (i<7){
  BH<-append(BH,bray.fungi.s[(map.s$treatment2=="D" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="booting" & map.s$block==i)]);
  i<-i+1;
}


i<-1
rm(HF)
HF<-vector()
while (i<7){
  HF<-append(HF,bray.fungi.s[(map.s$treatment2=="D" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}


i<-1
rm(TB)
TB<-vector()
while (i<7){
  TB<-append(TB,bray.fungi.s[(map.s$treatment2=="D" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="booting" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(TH)
TH<-vector()
while (i<7){
  TH<-append(TH,bray.fungi.s[(map.s$treatment2=="D" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="heading" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(TF)
TF<-vector()
while (i<7){
  TF<-append(TF,bray.fungi.s[(map.s$treatment2=="D" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEH)
SEH<-vector()
while (i<7){
  SEH<-append(SEH,bray.fungi.s[(map.s$treatment2=="D" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEF)
SEF<-vector()
while (i<7){
  SEF<-append(SEF,bray.fungi.s[(map.s$treatment2=="D" & map.s$growthstage=="flowering" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BF)
BF<-vector()
while (i<7){
  BF<-append(BF,bray.fungi.s[(map.s$treatment2=="D" & map.s$growthstage=="booting" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}

bray.fungi.treat<-data.frame(c(rep("TSE",6), rep("TB",6), rep("TH",6), rep("TF",6), rep("SEB",6), rep("SEH",6), rep("SEF",6), rep("BH",6), rep("BF",6), rep("HF",6)), c(rep(1:6,10)), c(TSE, TB, TH, TF, SEB, SEH, SEF, BH, BF, HF))

#Stem elongation drought (A)  (compare within block)
i<-1
rm(TSE)
TSE<-vector()
while (i<7){
  TSE<-append(TSE,bray.fungi.s[(map.s$treatment2=="A" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="A" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEB)
SEB<-vector()
while (i<7){
  SEB<-append(SEB,bray.fungi.s[(map.s$treatment2=="A" & map.s$growthstage=="booting" & map.s$block==i), (map.s$treatment2=="A" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BH)
BH<-vector()
while (i<7){
  BH<-append(BH,bray.fungi.s[(map.s$treatment2=="A" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="A" & map.s$growthstage=="booting" & map.s$block==i)]);
  i<-i+1;
}


i<-1
rm(HF)
HF<-vector()
while (i<7){
  HF<-append(HF,bray.fungi.s[(map.s$treatment2=="A" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="A" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}


i<-1
rm(TB)
TB<-vector()
while (i<7){
  TB<-append(TB,bray.fungi.s[(map.s$treatment2=="A" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="A" & map.s$growthstage=="booting" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(TH)
TH<-vector()
while (i<7){
  TH<-append(TH,bray.fungi.s[(map.s$treatment2=="A" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="A" & map.s$growthstage=="heading" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(TF)
TF<-vector()
while (i<7){
  TF<-append(TF,bray.fungi.s[(map.s$treatment2=="A" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="A" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEH)
SEH<-vector()
while (i<7){
  SEH<-append(SEH,bray.fungi.s[(map.s$treatment2=="A" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="A" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEF)
SEF<-vector()
while (i<7){
  SEF<-append(SEF,bray.fungi.s[(map.s$treatment2=="A" & map.s$growthstage=="flowering" & map.s$block==i), (map.s$treatment2=="A" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BF)
BF<-vector()
while (i<7){
  BF<-append(BF,bray.fungi.s[(map.s$treatment2=="A" & map.s$growthstage=="booting" & map.s$block==i), (map.s$treatment2=="A" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}

bray.fungi.treat<-data.frame(bray.fungi.treat, c(TSE, TB, TH, TF, SEB, SEH, SEF, BH, BF, HF))

#Booting drought (B)  (compare within block)
i<-1
rm(TSE)
TSE<-vector()
while (i<7){
  TSE<-append(TSE,bray.fungi.s[(map.s$treatment2=="B" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="B" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEB)
SEB<-vector()
while (i<7){
  SEB<-append(SEB,bray.fungi.s[(map.s$treatment2=="B" & map.s$growthstage=="booting" & map.s$block==i), (map.s$treatment2=="B" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BH)
BH<-vector()
while (i<7){
  BH<-append(BH,bray.fungi.s[(map.s$treatment2=="B" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="B" & map.s$growthstage=="booting" & map.s$block==i)]);
  i<-i+1;
}


i<-1
rm(HF)
HF<-vector()
while (i<7){
  HF<-append(HF,bray.fungi.s[(map.s$treatment2=="B" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="B" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}


i<-1
rm(TB)
TB<-vector()
while (i<7){
  TB<-append(TB,bray.fungi.s[(map.s$treatment2=="B" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="B" & map.s$growthstage=="booting" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(TH)
TH<-vector()
while (i<7){
  TH<-append(TH,bray.fungi.s[(map.s$treatment2=="B" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="B" & map.s$growthstage=="heading" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(TF)
TF<-vector()
while (i<7){
  TF<-append(TF,bray.fungi.s[(map.s$treatment2=="B" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="B" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEH)
SEH<-vector()
while (i<7){
  SEH<-append(SEH,bray.fungi.s[(map.s$treatment2=="B" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="B" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEF)
SEF<-vector()
while (i<7){
  SEF<-append(SEF,bray.fungi.s[(map.s$treatment2=="B" & map.s$growthstage=="flowering" & map.s$block==i), (map.s$treatment2=="B" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BF)
BF<-vector()
while (i<7){
  BF<-append(BF,bray.fungi.s[(map.s$treatment2=="B" & map.s$growthstage=="booting" & map.s$block==i), (map.s$treatment2=="B" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}

bray.fungi.treat<-data.frame(bray.fungi.treat, c(TSE, TB, TH, TF, SEB, SEH, SEF, BH, BF, HF))

#Heading drought (C)  (compare within block)
i<-1
rm(TSE)
TSE<-vector()
while (i<7){
  TSE<-append(TSE,bray.fungi.s[(map.s$treatment2=="C" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="C" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEB)
SEB<-vector()
while (i<7){
  SEB<-append(SEB,bray.fungi.s[(map.s$treatment2=="C" & map.s$growthstage=="booting" & map.s$block==i), (map.s$treatment2=="C" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BH)
BH<-vector()
while (i<7){
  BH<-append(BH,bray.fungi.s[(map.s$treatment2=="C" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="C" & map.s$growthstage=="booting" & map.s$block==i)]);
  i<-i+1;
}


i<-1
rm(HF)
HF<-vector()
while (i<7){
  HF<-append(HF,bray.fungi.s[(map.s$treatment2=="C" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="C" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}


i<-1
rm(TB)
TB<-vector()
while (i<7){
  TB<-append(TB,bray.fungi.s[(map.s$treatment2=="C" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="C" & map.s$growthstage=="booting" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(TH)
TH<-vector()
while (i<7){
  TH<-append(TH,bray.fungi.s[(map.s$treatment2=="C" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="C" & map.s$growthstage=="heading" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(TF)
TF<-vector()
while (i<7){
  TF<-append(TF,bray.fungi.s[(map.s$treatment2=="C" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="C" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEH)
SEH<-vector()
while (i<7){
  SEH<-append(SEH,bray.fungi.s[(map.s$treatment2=="C" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="C" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEF)
SEF<-vector()
while (i<7){
  SEF<-append(SEF,bray.fungi.s[(map.s$treatment2=="C" & map.s$growthstage=="flowering" & map.s$block==i), (map.s$treatment2=="C" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BF)
BF<-vector()
while (i<7){
  BF<-append(BF,bray.fungi.s[(map.s$treatment2=="C" & map.s$growthstage=="booting" & map.s$block==i), (map.s$treatment2=="C" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}

bray.fungi.treat<-data.frame(bray.fungi.treat, c(TSE, TB, TH, TF, SEB, SEH, SEF, BH, BF, HF))
colnames(bray.fungi.treat)<-c("Treatments","Block", "CTRL-D", "SE-A", "B-B", "H-C")

#Make the comparisons with stat tests
#Drought at stem elongation (A), T vs. SE
t.test(bray.fungi.treat[1:6,3], bray.fungi.treat[1:6,4], paired=T) #t = 0.46259, df = 5, p-value = 0.6631
mean(bray.fungi.treat[1:6,3]) #0.0.4741679 -- control
mean(bray.fungi.treat[1:6,4]) #0.4414427 -- treatment A
simil[nrow(simil)+1,] = c("T vs. SE", "A", "Fungi", mean(bray.fungi.treat[1:6,4])) #append to simil
simil[nrow(simil)+1,] = c("T vs. SE", "D", "Fungi", mean(bray.fungi.treat[1:6,3])) #append to simil

#Drought at stem elongation (A), SE vs.B (right after)
t.test(bray.fungi.treat[25:30,3], bray.fungi.treat[25:30,4], paired=T) #t = 0.043072, df = 5, p-value = 0.9673
mean(bray.fungi.treat[25:30,3]) #0.4795656 -- control
mean(bray.fungi.treat[25:30,4]) #0.4757664 -- treatment A
simil[nrow(simil)+1,] = c("SE vs. B", "A", "Fungi", mean(bray.fungi.treat[25:30,4])) #append to simil
simil[nrow(simil)+1,] = c("SE vs. B", "D", "Fungi", mean(bray.fungi.treat[25:30,3])) #append to simil

#Drought at stem elongation (A), B vs. H
t.test(bray.fungi.treat[43:48,3], bray.fungi.treat[43:48,4], paired=T) #t = -0.7454, df = 5, p-value = 0.4896
mean(bray.fungi.treat[43:48,3]) #0.4102386 -- control
mean(bray.fungi.treat[43:48,4]) #0.4477166 -- treatment A
simil[nrow(simil)+1,] = c("B vs. H", "A", "Fungi", mean(bray.fungi.treat[43:48,4])) #append to simil
simil[nrow(simil)+1,] = c("B vs. H", "D", "Fungi", mean(bray.fungi.treat[43:48,3])) #append to simil

#Drought at stem elongation (A), H vs. F
t.test(bray.fungi.treat[55:60,3], bray.fungi.treat[55:60,4], paired=T) #t = 0.95916, df = 5, p-value = 0.3815
mean(bray.fungi.treat[55:60,3]) #0.5715401 -- control
mean(bray.fungi.treat[55:60,4]) #0.4776593 -- treatment A
simil[nrow(simil)+1,] = c("H vs. F", "A", "Fungi", mean(bray.fungi.treat[55:60,4])) #append to simil
simil[nrow(simil)+1,] = c("H vs. F", "D", "Fungi", mean(bray.fungi.treat[55:60,3])) #append to simil

#Drought at booting (B), T vs. SE
t.test(bray.fungi.treat[1:6,3], bray.fungi.treat[1:6,5], paired=T) #t = -0.19582, df = 5, p-value = 0.8525
mean(bray.fungi.treat[1:6,3]) #0.4741679 -- control
mean(bray.fungi.treat[1:6,5]) #0.4892679 -- treatment B
simil[nrow(simil)+1,] = c("T vs. SE", "B", "Fungi", mean(bray.fungi.treat[1:6,5])) #apend to simil

#Drought at booting (B), SE vs.B
t.test(bray.fungi.treat[25:30,3], bray.fungi.treat[25:30,5], paired=T) #t = -1.1347, df = 5, p-value = 0.308
mean(bray.fungi.treat[25:30,3]) #0.4795656 -- control
mean(bray.fungi.treat[25:30,5]) #0.5915673 -- treatment B
simil[nrow(simil)+1,] = c("SE vs. B", "B", "Fungi", mean(bray.fungi.treat[25:30,5])) #append to simil

#Drought at booting (B), B vs. H
t.test(bray.fungi.treat[43:48,3], bray.fungi.treat[43:48,5], paired=T) #t = -2.5899, df = 5, p-value = 0.04884
mean(bray.fungi.treat[43:48,3]) #0.4102386 -- control
mean(bray.fungi.treat[43:48,5]) #0.5707059 -- treatment B
simil[nrow(simil)+1,] = c("B vs. H", "B", "Fungi", mean(bray.fungi.treat[43:48,5])) #append to simil

#Drought at booting (B), H vs. F
t.test(bray.fungi.treat[55:60,3], bray.fungi.treat[55:60,5], paired=T) #t = 0.69888, df = 5, p-value = 0.5158
mean(bray.fungi.treat[55:60,3]) #0.5715401 -- control
mean(bray.fungi.treat[55:60,5]) #0.505496 -- treatment B
simil[nrow(simil)+1,] = c("H vs. F", "B", "Fungi", mean(bray.fungi.treat[55:60,5])) #append to simil

#Drought at heading (C), T vs. SE
t.test(bray.fungi.treat[1:6,3], bray.fungi.treat[1:6,6], paired=T) #t = -1.4379, df = 5, p-value = 0.21
mean(bray.fungi.treat[1:6,3]) #0.4741679 -- control
mean(bray.fungi.treat[1:6,6]) #0.3925047 -- treatment C
simil[nrow(simil)+1,] = c("T vs. SE", "C", "Fungi", mean(bray.fungi.treat[1:6,6])) #apend to simil

#Drought at heading (C), SE vs.B
t.test(bray.fungi.treat[25:30,3], bray.fungi.treat[25:30,6], paired=T) #t = -1.8787, df = 5, p-value = 0.1191
mean(bray.fungi.treat[25:30,3]) #0.4795656 -- control
mean(bray.fungi.treat[25:30,6]) #0.5806325 -- treatment C
simil[nrow(simil)+1,] = c("SE vs. B", "C", "Fungi", mean(bray.fungi.treat[25:30,6])) #append to simil

#Drought at heading (C), B vs. H
t.test(bray.fungi.treat[43:48,3], bray.fungi.treat[43:48,6], paired=T) #t = -1.4691, df = 5, p-value = 0.2018
mean(bray.fungi.treat[43:48,3]) #0.4102386 -- control
mean(bray.fungi.treat[43:48,6]) #0.4543742 -- treatment C
simil[nrow(simil)+1,] = c("B vs. H", "C", "Fungi", mean(bray.fungi.treat[43:48,6])) #append to simil

#Drought at heading (C), H vs. F
t.test(bray.fungi.treat[55:60,3], bray.fungi.treat[55:60,6], paired=T) #t = -1.5158, df = 5, p-value = 0.19
mean(bray.fungi.treat[55:60,3]) #0.5715401 -- control
mean(bray.fungi.treat[55:60,6]) #0.4768609 -- treatment C
simil[nrow(simil)+1,] = c("H vs. F", "C", "Fungi", mean(bray.fungi.treat[55:60,6])) #append to simil

##Bacteria
#Create a dataframe comparing the similarity within blocks of different growth stages.
#Control (D)  (compare within block)
i<-1
rm(TSE)
TSE<-vector()
while (i<7){
  TSE<-append(TSE,bray.bact.s[(map.s$treatment2=="D" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEB)
SEB<-vector()
while (i<7){
  SEB<-append(SEB,bray.bact.s[(map.s$treatment2=="D" & map.s$growthstage=="booting" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BH)
BH<-vector()
while (i<7){
  BH<-append(BH,bray.bact.s[(map.s$treatment2=="D" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="booting" & map.s$block==i)]);
  i<-i+1;
}


i<-1
rm(HF)
HF<-vector()
while (i<7){
  HF<-append(HF,bray.bact.s[(map.s$treatment2=="D" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}


i<-1
rm(TB)
TB<-vector()
while (i<7){
  TB<-append(TB,bray.bact.s[(map.s$treatment2=="D" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="booting" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(TH)
TH<-vector()
while (i<7){
  TH<-append(TH,bray.bact.s[(map.s$treatment2=="D" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="heading" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(TF)
TF<-vector()
while (i<7){
  TF<-append(TF,bray.bact.s[(map.s$treatment2=="D" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEH)
SEH<-vector()
while (i<7){
  SEH<-append(SEH,bray.bact.s[(map.s$treatment2=="D" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEF)
SEF<-vector()
while (i<7){
  SEF<-append(SEF,bray.bact.s[(map.s$treatment2=="D" & map.s$growthstage=="flowering" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BF)
BF<-vector()
while (i<7){
  BF<-append(BF,bray.bact.s[(map.s$treatment2=="D" & map.s$growthstage=="booting" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}

bray.bact.treat<-data.frame(c(rep("TSE",6), rep("TB",6), rep("TH",6), rep("TF",6), rep("SEB",6), rep("SEH",6), rep("SEF",6), rep("BH",6), rep("BF",6), rep("HF",6)), c(rep(1:6,10)), c(TSE, TB, TH, TF, SEB, SEH, SEF, BH, BF, HF))

#Stem elongation drought (A)  (compare within block)
i<-1
rm(TSE)
TSE<-vector()
while (i<7){
  TSE<-append(TSE,bray.bact.s[(map.s$treatment2=="A" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="A" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEB)
SEB<-vector()
while (i<7){
  SEB<-append(SEB,bray.bact.s[(map.s$treatment2=="A" & map.s$growthstage=="booting" & map.s$block==i), (map.s$treatment2=="A" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BH)
BH<-vector()
while (i<7){
  BH<-append(BH,bray.bact.s[(map.s$treatment2=="A" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="A" & map.s$growthstage=="booting" & map.s$block==i)]);
  i<-i+1;
}


i<-1
rm(HF)
HF<-vector()
while (i<7){
  HF<-append(HF,bray.bact.s[(map.s$treatment2=="A" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="A" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}


i<-1
rm(TB)
TB<-vector()
while (i<7){
  TB<-append(TB,bray.bact.s[(map.s$treatment2=="A" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="A" & map.s$growthstage=="booting" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(TH)
TH<-vector()
while (i<7){
  TH<-append(TH,bray.bact.s[(map.s$treatment2=="A" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="A" & map.s$growthstage=="heading" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(TF)
TF<-vector()
while (i<7){
  TF<-append(TF,bray.bact.s[(map.s$treatment2=="A" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="A" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEH)
SEH<-vector()
while (i<7){
  SEH<-append(SEH,bray.bact.s[(map.s$treatment2=="A" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="A" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEF)
SEF<-vector()
while (i<7){
  SEF<-append(SEF,bray.bact.s[(map.s$treatment2=="A" & map.s$growthstage=="flowering" & map.s$block==i), (map.s$treatment2=="A" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BF)
BF<-vector()
while (i<7){
  BF<-append(BF,bray.bact.s[(map.s$treatment2=="A" & map.s$growthstage=="booting" & map.s$block==i), (map.s$treatment2=="A" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}

bray.bact.treat<-data.frame(bray.bact.treat, c(TSE, TB, TH, TF, SEB, SEH, SEF, BH, BF, HF))

#Booting drought (B)  (compare within block)
i<-1
rm(TSE)
TSE<-vector()
while (i<7){
  TSE<-append(TSE,bray.bact.s[(map.s$treatment2=="B" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="B" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEB)
SEB<-vector()
while (i<7){
  SEB<-append(SEB,bray.bact.s[(map.s$treatment2=="B" & map.s$growthstage=="booting" & map.s$block==i), (map.s$treatment2=="B" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BH)
BH<-vector()
while (i<7){
  BH<-append(BH,bray.bact.s[(map.s$treatment2=="B" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="B" & map.s$growthstage=="booting" & map.s$block==i)]);
  i<-i+1;
}


i<-1
rm(HF)
HF<-vector()
while (i<7){
  HF<-append(HF,bray.bact.s[(map.s$treatment2=="B" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="B" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}


i<-1
rm(TB)
TB<-vector()
while (i<7){
  TB<-append(TB,bray.bact.s[(map.s$treatment2=="B" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="B" & map.s$growthstage=="booting" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(TH)
TH<-vector()
while (i<7){
  TH<-append(TH,bray.bact.s[(map.s$treatment2=="B" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="B" & map.s$growthstage=="heading" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(TF)
TF<-vector()
while (i<7){
  TF<-append(TF,bray.bact.s[(map.s$treatment2=="B" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="B" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEH)
SEH<-vector()
while (i<7){
  SEH<-append(SEH,bray.bact.s[(map.s$treatment2=="B" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="B" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEF)
SEF<-vector()
while (i<7){
  SEF<-append(SEF,bray.bact.s[(map.s$treatment2=="B" & map.s$growthstage=="flowering" & map.s$block==i), (map.s$treatment2=="B" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BF)
BF<-vector()
while (i<7){
  BF<-append(BF,bray.bact.s[(map.s$treatment2=="B" & map.s$growthstage=="booting" & map.s$block==i), (map.s$treatment2=="B" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}

bray.bact.treat<-data.frame(bray.bact.treat, c(TSE, TB, TH, TF, SEB, SEH, SEF, BH, BF, HF))

#Heading drought (C)  (compare within block)
i<-1
rm(TSE)
TSE<-vector()
while (i<7){
  TSE<-append(TSE,bray.bact.s[(map.s$treatment2=="C" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="C" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEB)
SEB<-vector()
while (i<7){
  SEB<-append(SEB,bray.bact.s[(map.s$treatment2=="C" & map.s$growthstage=="booting" & map.s$block==i), (map.s$treatment2=="C" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BH)
BH<-vector()
while (i<7){
  BH<-append(BH,bray.bact.s[(map.s$treatment2=="C" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="C" & map.s$growthstage=="booting" & map.s$block==i)]);
  i<-i+1;
}


i<-1
rm(HF)
HF<-vector()
while (i<7){
  HF<-append(HF,bray.bact.s[(map.s$treatment2=="C" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="C" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}


i<-1
rm(TB)
TB<-vector()
while (i<7){
  TB<-append(TB,bray.bact.s[(map.s$treatment2=="C" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="C" & map.s$growthstage=="booting" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(TH)
TH<-vector()
while (i<7){
  TH<-append(TH,bray.bact.s[(map.s$treatment2=="C" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="C" & map.s$growthstage=="heading" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(TF)
TF<-vector()
while (i<7){
  TF<-append(TF,bray.bact.s[(map.s$treatment2=="C" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="C" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEH)
SEH<-vector()
while (i<7){
  SEH<-append(SEH,bray.bact.s[(map.s$treatment2=="C" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="C" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(SEF)
SEF<-vector()
while (i<7){
  SEF<-append(SEF,bray.bact.s[(map.s$treatment2=="C" & map.s$growthstage=="flowering" & map.s$block==i), (map.s$treatment2=="C" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BF)
BF<-vector()
while (i<7){
  BF<-append(BF,bray.bact.s[(map.s$treatment2=="C" & map.s$growthstage=="booting" & map.s$block==i), (map.s$treatment2=="C" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}

bray.bact.treat<-data.frame(bray.bact.treat, c(TSE, TB, TH, TF, SEB, SEH, SEF, BH, BF, HF))
colnames(bray.bact.treat)<-c("Treatments","Block", "CTRL-D", "SE-A", "B-B", "H-C")

#Make the comparisons with stat tests
#Drought at stem elongation (A), T vs. SE
t.test(bray.bact.treat[1:6,3], bray.bact.treat[1:6,4], paired=T) #t = -0.97289, df = 5, p-value = 0.3753
mean(bray.bact.treat[1:6,3]) #0.5481377 -- control
mean(bray.bact.treat[1:6,4]) #0.5616037 -- treatment A
simil[nrow(simil)+1,] = c("T vs. SE", "A", "Bacteria", mean(bray.bact.treat[1:6,4])) #append to simil
simil[nrow(simil)+1,] = c("T vs. SE", "D", "Bacteria", mean(bray.bact.treat[1:6,3])) #append to simil

#Drought at stem elongation (A), SE vs.B (right after)
t.test(bray.bact.treat[25:30,3], bray.bact.treat[25:30,4], paired=T) #t = -0.99847, df = 5, p-value = 0.3639
mean(bray.bact.treat[25:30,3]) #0.5657589 -- control
mean(bray.bact.treat[25:30,4]) #0.6131434 -- treatment A
simil[nrow(simil)+1,] = c("SE vs. B", "A", "Bacteria", mean(bray.bact.treat[25:30,4])) #append to simil
simil[nrow(simil)+1,] = c("SE vs. B", "D", "Bacteria", mean(bray.bact.treat[25:30,3])) #append to simil

#Drought at stem elongation (A), B vs. H
t.test(bray.bact.treat[43:48,3], bray.bact.treat[43:48,4], paired=T) #t = -0.87589, df = 5, p-value = 0.4212
mean(bray.bact.treat[43:48,3]) #0.5704153 -- control
mean(bray.bact.treat[43:48,4]) #0.589036 -- treatment A
simil[nrow(simil)+1,] = c("B vs. H", "A", "Bacteria", mean(bray.bact.treat[43:48,4])) #append to simil
simil[nrow(simil)+1,] = c("B vs. H", "D", "Bacteria", mean(bray.bact.treat[43:48,3])) #append to simil

#Drought at stem elongation (A), H vs. F
t.test(bray.bact.treat[55:60,3], bray.bact.treat[55:60,4], paired=T) #t = 0.095634, df = 5, p-value = 0.9275
mean(bray.bact.treat[55:60,3]) #0.662159 -- control
mean(bray.bact.treat[55:60,4]) #0.6583808 -- treatment A
simil[nrow(simil)+1,] = c("H vs. F", "A", "Bacteria", mean(bray.bact.treat[55:60,4])) #append to simil
simil[nrow(simil)+1,] = c("H vs. F", "D", "Bacteria", mean(bray.bact.treat[55:60,3])) #append to simil

#Drought at booting (B), T vs. SE
t.test(bray.bact.treat[1:6,3], bray.bact.treat[1:6,5], paired=T) #t = 0.16719, df = 5, p-value = 0.8738
mean(bray.bact.treat[1:6,3]) #0.5481377 -- control
mean(bray.bact.treat[1:6,5]) #0.5449769 -- treatment B
simil[nrow(simil)+1,] = c("T vs. SE", "B", "Bacteria", mean(bray.bact.treat[1:6,5])) #apend to simil

#Drought at booting (B), SE vs.B
t.test(bray.bact.treat[25:30,3], bray.bact.treat[25:30,5], paired=T) #t = -2.1396, df = 5, p-value = 0.08536
mean(bray.bact.treat[25:30,3]) #0.5657589 -- control
mean(bray.bact.treat[25:30,5]) #0.6254401 -- treatment B
simil[nrow(simil)+1,] = c("SE vs. B", "B", "Bacteria", mean(bray.bact.treat[25:30,5])) #append to simil

#Drought at booting (B), B vs. H
t.test(bray.bact.treat[43:48,3], bray.bact.treat[43:48,5], paired=T) #t = -1.5782, df = 5, p-value = 0.1753
mean(bray.bact.treat[43:48,3]) #0.5704153 -- control
mean(bray.bact.treat[43:48,5]) #0.6042808 -- treatment B
simil[nrow(simil)+1,] = c("B vs. H", "B", "Bacteria", mean(bray.bact.treat[43:48,5])) #append to simil

#Drought at booting (B), H vs. F
t.test(bray.bact.treat[55:60,3], bray.bact.treat[55:60,5], paired=T) #t = 1.2669, df = 5, p-value = 0.261
mean(bray.bact.treat[55:60,3]) #0.662159 -- control
mean(bray.bact.treat[55:60,5]) #0.6134286 -- treatment B
simil[nrow(simil)+1,] = c("H vs. F", "B", "Bacteria", mean(bray.bact.treat[55:60,5])) #append to simil

#Drought at heading (C), T vs. SE
t.test(bray.bact.treat[1:6,3], bray.bact.treat[1:6,6], paired=T) #t = -0.53524, df = 5, p-value = 0.6154
mean(bray.bact.treat[1:6,3]) #0.5481377 -- control
mean(bray.bact.treat[1:6,6]) #0.5559889 -- treatment C
simil[nrow(simil)+1,] = c("T vs. SE", "C", "Bacteria", mean(bray.bact.treat[1:6,6])) #apend to simil

#Drought at heading (C), SE vs.B
t.test(bray.bact.treat[25:30,3], bray.bact.treat[25:30,6], paired=T) #t = -0.88552, df = 5, p-value = 0.4164
mean(bray.bact.treat[25:30,3]) #0.5657589 -- control
mean(bray.bact.treat[25:30,6]) #0.5847815 -- treatment C
simil[nrow(simil)+1,] = c("SE vs. B", "C", "Bacteria", mean(bray.bact.treat[25:30,6])) #append to simil

#Drought at heading (C), B vs. H
t.test(bray.bact.treat[43:48,3], bray.bact.treat[43:48,6], paired=T) #t = -1.9292, df = 5, p-value = 0.1116
mean(bray.bact.treat[43:48,3]) #0.5704153 -- control
mean(bray.bact.treat[43:48,6]) #0.605897 -- treatment C
simil[nrow(simil)+1,] = c("B vs. H", "C", "Bacteria", mean(bray.bact.treat[43:48,6])) #append to simil

#Drought at heading (C), H vs. F
t.test(bray.bact.treat[55:60,3], bray.bact.treat[55:60,6], paired=T) #t = -0.097543, df = 5, p-value = 0.9261
mean(bray.bact.treat[55:60,3]) #0.662159 -- control
mean(bray.bact.treat[55:60,6]) #0.6653317 -- treatment C
simil[nrow(simil)+1,] = c("H vs. F", "C", "Bacteria", mean(bray.bact.treat[55:60,6])) #append to simil

#save simil - to be used in figure genration
saveRDS(simil, file = here("data", "intermediate", "simil-stages.RDS")) #But only has mean, not the reps... no good Go back to original objects

#Create file with all reps for plotting
#Keep only the adjacent stages
diss.all <- bray.treat[bray.treat$Treatments %in% c("TSE", "SEB", "BH", "HF"),]
diss.bact <- bray.bact.treat[bray.bact.treat$Treatments %in% c("TSE", "SEB", "BH", "HF"),]
diss.fungi <- bray.fungi.treat[bray.fungi.treat$Treatments %in% c("TSE", "SEB", "BH", "HF"),]
diss.reps <-rbind(diss.all,diss.bact,diss.fungi)
diss.reps$Subset <- c(rep("All",24), rep("Bacteria",24), rep("Fungi",24))
diss.reps <- gather(diss.reps, key="Treatment", value="Dissimilarity", 3:6) #transform in long format
colnames(diss.reps) <- c("GrowthStages", "Block", "Subset", "Treatment","Dissimilarity")

#Save  - to be used in figure generation
saveRDS(diss.reps, file = here("data", "intermediate", "diss.reps.RDS"))

###Compare dissimilarity to control across all treatments and stages
##All
#Create a dataframe 
#Treatment A  (compare within block)
i<-1
rm(AT)
AT<-vector()
while (i<7){
  AT<-append(AT,bray.s[(map.s$treatment2=="A" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="tillering" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(ASE)
ASE<-vector()
while (i<7){
  ASE<-append(ASE,bray.s[(map.s$treatment2=="A" & map.s$growthstage=="stemelongation" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(AB)
AB<-vector()
while (i<7){
  AB<-append(AB,bray.s[(map.s$treatment2=="A" & map.s$growthstage=="booting" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="booting" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(AH)
AH<-vector()
while (i<7){
  AH<-append(AH,bray.s[(map.s$treatment2=="A" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="heading" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(AF)
AF<-vector()
while (i<7){
  AF<-append(AF,bray.s[(map.s$treatment2=="A" & map.s$growthstage=="flowering" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}

#Treatment B  (compare within block)
i<-1
rm(BT)
BT<-vector()
while (i<7){
  BT<-append(BT,bray.s[(map.s$treatment2=="B" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="tillering" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BSE)
BSE<-vector()
while (i<7){
  BSE<-append(BSE,bray.s[(map.s$treatment2=="B" & map.s$growthstage=="stemelongation" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BB)
BB<-vector()
while (i<7){
  BB<-append(BB,bray.s[(map.s$treatment2=="B" & map.s$growthstage=="booting" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="booting" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BH)
BH<-vector()
while (i<7){
  BH<-append(BH,bray.s[(map.s$treatment2=="B" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="heading" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BF)
BF<-vector()
while (i<7){
  BF<-append(BF,bray.s[(map.s$treatment2=="B" & map.s$growthstage=="flowering" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}

#Treatment C  (compare within block)
i<-1
rm(CT)
CT<-vector()
while (i<7){
  CT<-append(CT,bray.s[(map.s$treatment2=="C" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="tillering" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(CSE)
CSE<-vector()
while (i<7){
  CSE<-append(CSE,bray.s[(map.s$treatment2=="C" & map.s$growthstage=="stemelongation" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(CB)
CB<-vector()
while (i<7){
  CB<-append(CB,bray.s[(map.s$treatment2=="C" & map.s$growthstage=="booting" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="booting" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(CH)
CH<-vector()
while (i<7){
  CH<-append(CH,bray.s[(map.s$treatment2=="C" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="heading" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(CF)
CF<-vector()
while (i<7){
  CF<-append(CF,bray.s[(map.s$treatment2=="C" & map.s$growthstage=="flowering" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}

bray.control.all <- data.frame("Treatment"=c(rep("A",30), rep("B",30), rep("C",30)), "GrowthStage"=c(rep(c(rep("Tillering",6), rep("Stem Elongation",6), rep("Booting", 6), rep("Heading",6), rep("Flowering",6)),3)), "Block"=rep(1:6,15), "Subset"= rep("All",90), "Dissimilarity"=c(AT, ASE, AB, AH, AF, BT, BSE, BB, BH, BF, CT, CSE, CB, CH, CF))

##Fungi
#Create a dataframe 
#Treatment A  (compare within block)
i<-1
rm(AT)
AT<-vector()
while (i<7){
  AT<-append(AT,bray.fungi.s[(map.s$treatment2=="A" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="tillering" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(ASE)
ASE<-vector()
while (i<7){
  ASE<-append(ASE,bray.fungi.s[(map.s$treatment2=="A" & map.s$growthstage=="stemelongation" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(AB)
AB<-vector()
while (i<7){
  AB<-append(AB,bray.fungi.s[(map.s$treatment2=="A" & map.s$growthstage=="booting" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="booting" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(AH)
AH<-vector()
while (i<7){
  AH<-append(AH,bray.fungi.s[(map.s$treatment2=="A" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="heading" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(AF)
AF<-vector()
while (i<7){
  AF<-append(AF,bray.fungi.s[(map.s$treatment2=="A" & map.s$growthstage=="flowering" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}

#Treatment B  (compare within block)
i<-1
rm(BT)
BT<-vector()
while (i<7){
  BT<-append(BT,bray.fungi.s[(map.s$treatment2=="B" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="tillering" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BSE)
BSE<-vector()
while (i<7){
  BSE<-append(BSE,bray.fungi.s[(map.s$treatment2=="B" & map.s$growthstage=="stemelongation" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BB)
BB<-vector()
while (i<7){
  BB<-append(BB,bray.fungi.s[(map.s$treatment2=="B" & map.s$growthstage=="booting" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="booting" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BH)
BH<-vector()
while (i<7){
  BH<-append(BH,bray.fungi.s[(map.s$treatment2=="B" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="heading" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BF)
BF<-vector()
while (i<7){
  BF<-append(BF,bray.fungi.s[(map.s$treatment2=="B" & map.s$growthstage=="flowering" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}

#Treatment C  (compare within block)
i<-1
rm(CT)
CT<-vector()
while (i<7){
  CT<-append(CT,bray.fungi.s[(map.s$treatment2=="C" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="tillering" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(CSE)
CSE<-vector()
while (i<7){
  CSE<-append(CSE,bray.fungi.s[(map.s$treatment2=="C" & map.s$growthstage=="stemelongation" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(CB)
CB<-vector()
while (i<7){
  CB<-append(CB,bray.fungi.s[(map.s$treatment2=="C" & map.s$growthstage=="booting" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="booting" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(CH)
CH<-vector()
while (i<7){
  CH<-append(CH,bray.fungi.s[(map.s$treatment2=="C" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="heading" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(CF)
CF<-vector()
while (i<7){
  CF<-append(CF,bray.fungi.s[(map.s$treatment2=="C" & map.s$growthstage=="flowering" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}

bray.control.fungi<-data.frame("Treatment"=c(rep("A",30), rep("B",30), rep("C",30)), "GrowthStage"=c(rep(c(rep("Tillering",6), rep("Stem Elongation",6), rep("Booting", 6), rep("Heading",6), rep("Flowering",6)),3)), "Block"=rep(1:6,15), "Subset"= rep("Fungi",90), "Dissimilarity"=c(AT, ASE, AB, AH, AF, BT, BSE, BB, BH, BF, CT, CSE, CB, CH, CF))

##Bacteria
#Create a dataframe 
#Treatment A  (compare within block)
i<-1
rm(AT)
AT<-vector()
while (i<7){
  AT<-append(AT,bray.bact.s[(map.s$treatment2=="A" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="tillering" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(ASE)
ASE<-vector()
while (i<7){
  ASE<-append(ASE,bray.bact.s[(map.s$treatment2=="A" & map.s$growthstage=="stemelongation" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(AB)
AB<-vector()
while (i<7){
  AB<-append(AB,bray.bact.s[(map.s$treatment2=="A" & map.s$growthstage=="booting" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="booting" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(AH)
AH<-vector()
while (i<7){
  AH<-append(AH,bray.bact.s[(map.s$treatment2=="A" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="heading" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(AF)
AF<-vector()
while (i<7){
  AF<-append(AF,bray.bact.s[(map.s$treatment2=="A" & map.s$growthstage=="flowering" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}

#Treatment B  (compare within block)
i<-1
rm(BT)
BT<-vector()
while (i<7){
  BT<-append(BT,bray.bact.s[(map.s$treatment2=="B" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="tillering" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BSE)
BSE<-vector()
while (i<7){
  BSE<-append(BSE,bray.bact.s[(map.s$treatment2=="B" & map.s$growthstage=="stemelongation" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BB)
BB<-vector()
while (i<7){
  BB<-append(BB,bray.bact.s[(map.s$treatment2=="B" & map.s$growthstage=="booting" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="booting" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BH)
BH<-vector()
while (i<7){
  BH<-append(BH,bray.bact.s[(map.s$treatment2=="B" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="heading" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(BF)
BF<-vector()
while (i<7){
  BF<-append(BF,bray.bact.s[(map.s$treatment2=="B" & map.s$growthstage=="flowering" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}

#Treatment C  (compare within block)
i<-1
rm(CT)
CT<-vector()
while (i<7){
  CT<-append(CT,bray.bact.s[(map.s$treatment2=="C" & map.s$growthstage=="tillering" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="tillering" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(CSE)
CSE<-vector()
while (i<7){
  CSE<-append(CSE,bray.bact.s[(map.s$treatment2=="C" & map.s$growthstage=="stemelongation" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="stemelongation" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(CB)
CB<-vector()
while (i<7){
  CB<-append(CB,bray.bact.s[(map.s$treatment2=="C" & map.s$growthstage=="booting" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="booting" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(CH)
CH<-vector()
while (i<7){
  CH<-append(CH,bray.bact.s[(map.s$treatment2=="C" & map.s$growthstage=="heading" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="heading" & map.s$block==i)]);
  i<-i+1;
}

i<-1
rm(CF)
CF<-vector()
while (i<7){
  CF<-append(CF,bray.bact.s[(map.s$treatment2=="C" & map.s$growthstage=="flowering" & map.s$block==i), (map.s$treatment2=="D" & map.s$growthstage=="flowering" & map.s$block==i)]);
  i<-i+1;
}

bray.control.bact<-data.frame("Treatment"=c(rep("A",30), rep("B",30), rep("C",30)), "GrowthStage"=c(rep(c(rep("Tillering",6), rep("Stem Elongation",6), rep("Booting", 6), rep("Heading",6), rep("Flowering",6)),3)), "Block"=rep(1:6,15), "Subset"= rep("Bacteria",90), "Dissimilarity"=c(AT, ASE, AB, AH, AF, BT, BSE, BB, BH, BF, CT, CSE, CB, CH, CF))

bray.control <- rbind(bray.control.all, bray.control.bact, bray.control.fungi) 

#save for plotting
saveRDS(bray.control, file = here("data", "intermediate", "bray.control.RDS"))

##Anova and mean
#all
#ANOVAs
#A
summary(aov(Dissimilarity ~ GrowthStage+Block, data=bray.control.all[bray.control.all$Treatment=="A",])) #F=2.271, P=0.0912
TukeyHSD(aov(Dissimilarity ~ GrowthStage+Block, data=bray.control.all[bray.control.all$Treatment=="A",])) #Tillering-Flowering: 0.0625766
#B
summary(aov(Dissimilarity ~ GrowthStage+Block, data=bray.control.all[bray.control.all$Treatment=="B",])) #F=2.948, P=0.0409
TukeyHSD(aov(Dissimilarity ~ GrowthStage+Block, data=bray.control.all[bray.control.all$Treatment=="B",])) #Tillering-Heading: 0.0973375
#C
summary(aov(Dissimilarity ~ GrowthStage+Block, data=bray.control.all[bray.control.all$Treatment=="C",])) #F=0.717, P=0.5884
#Means
mean(bray.control.all[bray.control.all$Treatment=="A" & bray.control.all$GrowthStage == "Tillering",5])
mean(bray.control.all[bray.control.all$Treatment=="B" & bray.control.all$GrowthStage == "Tillering",5])
mean(bray.control.all[bray.control.all$Treatment=="C" & bray.control.all$GrowthStage == "Tillering",5])
mean(bray.control.all[bray.control.all$Treatment=="A" & bray.control.all$GrowthStage == "Stem Elongation",5])
mean(bray.control.all[bray.control.all$Treatment=="B" & bray.control.all$GrowthStage == "Stem Elongation",5])
mean(bray.control.all[bray.control.all$Treatment=="C" & bray.control.all$GrowthStage == "Stem Elongation",5])
mean(bray.control.all[bray.control.all$Treatment=="A" & bray.control.all$GrowthStage == "Booting",5])
mean(bray.control.all[bray.control.all$Treatment=="B" & bray.control.all$GrowthStage == "Booting",5])
mean(bray.control.all[bray.control.all$Treatment=="C" & bray.control.all$GrowthStage == "Booting",5])
mean(bray.control.all[bray.control.all$Treatment=="A" & bray.control.all$GrowthStage == "Heading",5])
mean(bray.control.all[bray.control.all$Treatment=="B" & bray.control.all$GrowthStage == "Heading",5])
mean(bray.control.all[bray.control.all$Treatment=="C" & bray.control.all$GrowthStage == "Heading",5])
mean(bray.control.all[bray.control.all$Treatment=="A" & bray.control.all$GrowthStage == "Flowering",5])
mean(bray.control.all[bray.control.all$Treatment=="B" & bray.control.all$GrowthStage == "Flowering",5])
mean(bray.control.all[bray.control.all$Treatment=="C" & bray.control.all$GrowthStage == "Flowering",5])

##Bacteria
#ANOVAs
#A
summary(aov(Dissimilarity ~ GrowthStage+Block, data=bray.control.bact[bray.control.bact$Treatment=="A",])) #F=1.614, P=0.203
#B
summary(aov(Dissimilarity ~ GrowthStage+Block, data=bray.control.bact[bray.control.bact$Treatment=="B",])) #F=3.158, P=0.0321
TukeyHSD(aov(Dissimilarity ~ GrowthStage+Block, data=bray.control.bact[bray.control.bact$Treatment=="B",])) #none
#C
summary(aov(Dissimilarity ~ GrowthStage+Block, data=bray.control.bact[bray.control.bact$Treatment=="C",])) #F=0.782, P=0.5478
#Means
mean(bray.control.bact[bray.control.bact$Treatment=="A" & bray.control.bact$GrowthStage == "Tillering",5])
mean(bray.control.bact[bray.control.bact$Treatment=="B" & bray.control.bact$GrowthStage == "Tillering",5])
mean(bray.control.bact[bray.control.bact$Treatment=="C" & bray.control.bact$GrowthStage == "Tillering",5])
mean(bray.control.bact[bray.control.bact$Treatment=="A" & bray.control.bact$GrowthStage == "Stem Elongation",5])
mean(bray.control.bact[bray.control.bact$Treatment=="B" & bray.control.bact$GrowthStage == "Stem Elongation",5])
mean(bray.control.bact[bray.control.bact$Treatment=="C" & bray.control.bact$GrowthStage == "Stem Elongation",5])
mean(bray.control.bact[bray.control.bact$Treatment=="A" & bray.control.bact$GrowthStage == "Booting",5])
mean(bray.control.bact[bray.control.bact$Treatment=="B" & bray.control.bact$GrowthStage == "Booting",5])
mean(bray.control.bact[bray.control.bact$Treatment=="C" & bray.control.bact$GrowthStage == "Booting",5])
mean(bray.control.bact[bray.control.bact$Treatment=="A" & bray.control.bact$GrowthStage == "Heading",5])
mean(bray.control.bact[bray.control.bact$Treatment=="B" & bray.control.bact$GrowthStage == "Heading",5])
mean(bray.control.bact[bray.control.bact$Treatment=="C" & bray.control.bact$GrowthStage == "Heading",5])
mean(bray.control.bact[bray.control.bact$Treatment=="A" & bray.control.bact$GrowthStage == "Flowering",5])
mean(bray.control.bact[bray.control.bact$Treatment=="B" & bray.control.bact$GrowthStage == "Flowering",5])
mean(bray.control.bact[bray.control.bact$Treatment=="C" & bray.control.bact$GrowthStage == "Flowering",5])

##Fungi
#ANOVAs
#A
summary(aov(Dissimilarity ~ GrowthStage+Block, data=bray.control.fungi[bray.control.fungi$Treatment=="A",])) #F=2.422, P=0.076
TukeyHSD(aov(Dissimilarity ~ GrowthStage+Block, data=bray.control.fungi[bray.control.fungi$Treatment=="A",])) #Tillering-Heading: 0.0408981
#B
summary(aov(Dissimilarity ~ GrowthStage+Block, data=bray.control.fungi[bray.control.fungi$Treatment=="B",])) #F=3.327, P=0.0265
TukeyHSD(aov(Dissimilarity ~ GrowthStage+Block, data=bray.control.fungi[bray.control.fungi$Treatment=="B",])) #Tillering-Heading: 0.0351593, Heading-Flowering:0.0743142
#C
summary(aov(Dissimilarity ~ GrowthStage+Block, data=bray.control.fungi[bray.control.fungi$Treatment=="C",])) #F=0.096, P=0.983
#Means
mean(bray.control.fungi[bray.control.fungi$Treatment=="A" & bray.control.fungi$GrowthStage == "Tillering",5])
mean(bray.control.fungi[bray.control.fungi$Treatment=="B" & bray.control.fungi$GrowthStage == "Tillering",5])
mean(bray.control.fungi[bray.control.fungi$Treatment=="C" & bray.control.fungi$GrowthStage == "Tillering",5])
mean(bray.control.fungi[bray.control.fungi$Treatment=="A" & bray.control.fungi$GrowthStage == "Stem Elongation",5])
mean(bray.control.fungi[bray.control.fungi$Treatment=="B" & bray.control.fungi$GrowthStage == "Stem Elongation",5])
mean(bray.control.fungi[bray.control.fungi$Treatment=="C" & bray.control.fungi$GrowthStage == "Stem Elongation",5])
mean(bray.control.fungi[bray.control.fungi$Treatment=="A" & bray.control.fungi$GrowthStage == "Booting",5])
mean(bray.control.fungi[bray.control.fungi$Treatment=="B" & bray.control.fungi$GrowthStage == "Booting",5])
mean(bray.control.fungi[bray.control.fungi$Treatment=="C" & bray.control.fungi$GrowthStage == "Booting",5])
mean(bray.control.fungi[bray.control.fungi$Treatment=="A" & bray.control.fungi$GrowthStage == "Heading",5])
mean(bray.control.fungi[bray.control.fungi$Treatment=="B" & bray.control.fungi$GrowthStage == "Heading",5])
mean(bray.control.fungi[bray.control.fungi$Treatment=="C" & bray.control.fungi$GrowthStage == "Heading",5])
mean(bray.control.fungi[bray.control.fungi$Treatment=="A" & bray.control.fungi$GrowthStage == "Flowering",5])
mean(bray.control.fungi[bray.control.fungi$Treatment=="B" & bray.control.fungi$GrowthStage == "Flowering",5])
mean(bray.control.fungi[bray.control.fungi$Treatment=="C" & bray.control.fungi$GrowthStage == "Flowering",5])
