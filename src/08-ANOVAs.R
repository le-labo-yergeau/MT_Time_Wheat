###ANOVAs on the abundant tax aand COGs of 07-StakcBarChart
##Taxa
#Load data
tax.map <- readRDS(file = here("data", "intermediate", "tax.map.RDS"))
#At each growth stages look for differences between treatment and control using paired t-test
#Sort data by growth stage, then treatment and then block
tax.map.s <- tax.map[order(tax.map$growthstage,tax.map$treatment2,tax.map$block),]
#Loop for t.test
k<-19
m <- 0
rm(ttest.tax)
rm(ttest.vector)
ttest.tax<-data.frame(matrix(NA, nrow = 30, ncol = 15))
ttest.vector <- vector()
while (k<120){
  print(tax.map.s$growthstage[k])
  j <- 1
  while(j<14){
    print(tax.map.s$treatment2[j])
    rm(ttest.vector)
    ttest.vector <- vector()
    i <- 6
    while(i<21){
      ttest <- t.test(tax.map.s[(j+k-19):(j+k-19+5),i], tax.map.s[k:(k+5),i], paired = T)
      print(colnames(tax.map.s)[i]); print(ttest$statistic); print(ttest$p.value)
      ttest.vector<-append(ttest.vector, c(ttest$statistic, ttest$p.value))
      i <- i+1
    }
    j <- j+6
    m <- m+1
    ttest.tax[,m] <- data.frame(ttest.vector)
  }

k <- k+24
}

ttest.tax$parameter <- rep(c("t","p"),15)
ttest.tax$Phylum <- rep(colnames(tax.map)[6:20],each=2)
colnames(ttest.tax) <- c("Booting-A", "Booting-B","Booting-C",  "Flowering-A", "Flowering-B","Flowering-C", "Heading-A", "Heading-B","Heading-C", "StemElongation-A", "StemElongation-B","StemElongation-C", "Tillering-A", "Tillering-B", "Tillering-C", "Parameter", "Phylum")
ttest.tax <- ttest.tax[, c( "Phylum", "Parameter", "Tillering-A", "Tillering-B", "Tillering-C", "StemElongation-A", "StemElongation-B","StemElongation-C", "Booting-A", "Booting-B","Booting-C",  "Heading-A", "Heading-B","Heading-C", "Flowering-A", "Flowering-B","Flowering-C")]
#Export table
write.table(ttest.tax ,file= here("output", "tables","T-test-Phylum.txt"), sep = "\t")

##Functions
#Load data
fun.map <- readRDS(file = here("data", "intermediate", "fun.map.RDS"))
#At each growth stages look for differences between treatment and control using paired t-test
#Sort data by growth stage, then treatment and then block
fun.map.s <- fun.map[order(fun.map$growthstage,fun.map$treatment2,fun.map$block),]
#Loop for t.test
k<-19
m <- 0
rm(ttest.fun)
rm(ttest.vector)
ttest.fun<-data.frame(matrix(NA, nrow = 30, ncol = 15))
ttest.vector <- vector()
while (k<120){
  print(fun.map.s$growthstage[k])
  j <- 1
  while(j<14){
    print(fun.map.s$treatment2[j])
    rm(ttest.vector)
    ttest.vector <- vector()
    i <- 6
    while(i<21){
      ttest <- t.test(fun.map.s[(j+k-19):(j+k-19+5),i], fun.map.s[k:(k+5),i], paired = T)
      print(colnames(fun.map.s)[i]); print(ttest$statistic); print(ttest$p.value)
      ttest.vector<-append(ttest.vector, c(ttest$statistic, ttest$p.value))
      i <- i+1
    }
    j <- j+6
    m <- m+1
    ttest.fun[,m] <- data.frame(ttest.vector)
  }
  
  k <- k+24
}

ttest.fun$parameter <- rep(c("t","p"),15)
ttest.fun$COG <- rep(colnames(fun.map)[6:20],each=2)
colnames(ttest.fun) <- c("Booting-A", "Booting-B","Booting-C",  "Flowering-A", "Flowering-B","Flowering-C", "Heading-A", "Heading-B","Heading-C", "StemElongation-A", "StemElongation-B","StemElongation-C", "Tillering-A", "Tillering-B", "Tillering-C", "Parameter", "COG")
ttest.fun <- ttest.fun[, c( "COG", "Parameter", "Tillering-A", "Tillering-B", "Tillering-C", "StemElongation-A", "StemElongation-B","StemElongation-C", "Booting-A", "Booting-B","Booting-C",  "Heading-A", "Heading-B","Heading-C", "Flowering-A", "Flowering-B","Flowering-C")]
#Export table
write.table(ttest.fun ,file= here("output", "tables","T-test-COG.txt"), sep = "\t")
