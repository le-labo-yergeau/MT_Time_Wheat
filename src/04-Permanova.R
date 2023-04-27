##All
#Import objects
bray.s <- readRDS(file = here("data","intermediate", "bray.s.RDS"))
map <- readRDS(file = here("data","intermediate", "map.RDS"))

#Sort
map.s <- map[order(row.names(map)),]
sum(row.names(bray.s) == row.names(map.s))#120
sum(row.names(bray.s) == colnames(bray.s))#120

#Check for betadisp
anova(betadisper(as.dist(bray.s), group = map.s$treatment2)) #F=0.6876, P=0.5614
anova(betadisper(as.dist(bray.s), group = map.s$growthstage)) #F=13.194, P=7.057e-09

set.seed(69)

#Permanova
perm <-how(nperm = 999)
setBlocks (perm) <- with(map.s, block)

#Treatment1
set.seed(69)
permanova.all.1 <- adonis2(as.dist(bray.s)~treatment*growthstage, data = map.s, permutations = perm)
permanova.all.1
adonis2(formula = as.dist(bray.s) ~ treatment * growthstage, data = map.s, permutations = perm)

#Df SumOfSqs      R2      F Pr(>F)    
#treatment               2   1.4195 0.06902 4.9258  0.001 ***
#  growthstage             4   2.6908 0.13084 4.6688  0.001 ***
#  treatment:growthstage   4   0.7505 0.03649 1.3021  0.038 *  
#  Residual              109  15.7052 0.76365                  
#Total                 119  20.5659 1.00000          


#Treatment2
set.seed(69)
permanova.all.2 <- adonis2(as.dist(bray.s)~treatment2*growthstage, data = map.s, permutations = perm)
permanova.all.2

#adonis2(formula = as.dist(bray.s) ~ treatment2 * growthstage, data = map.s, permutations = perm)
#Df SumOfSqs      R2      F Pr(>F)    
#treatment2               3   0.6522 0.03171 1.5180  0.008 ** 
#  growthstage              4   3.3078 0.16084 5.7741  0.001 ***
#  treatment2:growthstage  12   2.2841 0.11106 1.3290  0.001 ***
#  Residual               100  14.3218 0.69638                  
#Total                  119  20.5659 1.00000             

##Fungi
#Import objects
bray.fungi.s <- readRDS(file = here("data","intermediate", "bray.fungi.s.RDS"))

#Check sorting
sum(row.names(bray.fungi.s) == row.names(map.s))#120
sum(row.names(bray.fungi.s) == colnames(bray.fungi.s))#120

#Check for betadisp
anova(betadisper(as.dist(bray.fungi.s), group = map.s$treatment2)) #F=3.7404; P=0.01312
anova(betadisper(as.dist(bray.fungi.s), group = map.s$growthstage)) #F=4.488; P=0.002091

#Permanova
perm <-how(nperm = 999)
setBlocks (perm) <- with(map.s, block)
set.seed(69)
permanova.fungi <- adonis2(as.dist(bray.fungi.s)~treatment2*growthstage, data = map.s, permutations = perm)
permanova.fungi

#adonis2(formula = as.dist(bray.fungi.s) ~ treatment2 * growthstage, data = map.s, permutations = perm)
#Df SumOfSqs      R2      F Pr(>F)    
#treatment2               3   0.7849 0.04667 2.3190  0.001 ***
#  growthstage              4   2.8753 0.17095 6.3709  0.001 ***
#  treatment2:growthstage  12   1.8761 0.11155 1.3857  0.022 *  
#  Residual               100  11.2828 0.67083                  
#Total                  119  16.8191 1.00000 

##Bacteria
#Import objects
bray.bact.s <- readRDS(file = here("data","intermediate", "bray.bact.s.RDS"))

#Check sorting
sum(row.names(bray.bact.s) == row.names(map.s))#120
sum(row.names(bray.bact.s) == colnames(bray.bact.s))#120

#Check for betadisp
anova(betadisper(as.dist(bray.bact.s), group = map.s$treatment2)) #F=0.4764; P=0.6993
anova(betadisper(as.dist(bray.bact.s), group = map.s$growthstage)) #F=11.555; P=6.404e-08

#Permanova
perm <-how(nperm = 999)
setBlocks (perm) <- with(map.s, block)
set.seed(69)
permanova.bact <- adonis2(as.dist(bray.bact.s)~treatment2*growthstage, data = map.s, permutations = perm)
permanova.bact

#adonis2(formula = as.dist(bray.bact.s) ~ treatment2 * growthstage, data = map.s, permutations = perm)
#Df SumOfSqs      R2      F Pr(>F)    
#treatment2               3   0.6618 0.02895 1.3475  0.026 *  
#  growthstage              4   3.2681 0.14293 4.9904  0.001 ***
#  treatment2:growthstage  12   2.5629 0.11209 1.3045  0.001 ***
#  Residual               100  16.3717 0.71603                  
#Total                  119  22.8645 1.00000  