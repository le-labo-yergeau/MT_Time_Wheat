###Plots for DE counts and dissimilarity - contrast to control
##DE
#Import
DE.counts <- readRDS(file = here("data", "intermediate", "DE.counts.RDS"))

#Make sure last column is numeric
DE.counts$DEcount <- as.numeric(DE.counts$DEcount)

#Reorder manually
DE.counts$GrowthStage <- factor(DE.counts$GrowthStage, c("Tillering", "Stem Elongation", "Booting", "Heading", "Flowering" ))

#Plot
DE.line <- ggplot(data = DE.counts, aes(x = GrowthStage, y = log10(DEcount), color = Subset, group = Subset))+
  geom_point()+
  geom_line()+
  ylab("DA counts (log 10)")+
  #Weird glitch: geom_rect is adding the layer x number of obs. To have the exact same alpha, had to repeat 6 times here (45 obs vs. 270). did not find another workaround for faceted images (annotate could be use when not facet)
  geom_rect(data=filter(DE.counts, Treatment=="A"), aes(xmin = 2, xmax=3, ymin=-Inf, ymax=Inf), alpha=0.01, fill="grey", color = NA, show.legend = F)+
  geom_rect(data=filter(DE.counts, Treatment=="B"), aes(xmin = 3, xmax=4, ymin=-Inf, ymax=Inf), alpha=0.01, fill="grey", color = NA, show.legend = F)+
  geom_rect(data=filter(DE.counts, Treatment=="C"), aes(xmin = 4, xmax=5, ymin=-Inf, ymax=Inf), alpha=0.01, fill="grey", color = NA, show.legend = F)+
  geom_rect(data=filter(DE.counts, Treatment=="A"), aes(xmin = 2, xmax=3, ymin=-Inf, ymax=Inf), alpha=0.01, fill="grey", color = NA, show.legend = F)+
  geom_rect(data=filter(DE.counts, Treatment=="B"), aes(xmin = 3, xmax=4, ymin=-Inf, ymax=Inf), alpha=0.01, fill="grey", color = NA, show.legend = F)+
  geom_rect(data=filter(DE.counts, Treatment=="C"), aes(xmin = 4, xmax=5, ymin=-Inf, ymax=Inf), alpha=0.01, fill="grey", color = NA, show.legend = F)+
  geom_rect(data=filter(DE.counts, Treatment=="A"), aes(xmin = 2, xmax=3, ymin=-Inf, ymax=Inf), alpha=0.01, fill="grey", color = NA, show.legend = F)+
  geom_rect(data=filter(DE.counts, Treatment=="B"), aes(xmin = 3, xmax=4, ymin=-Inf, ymax=Inf), alpha=0.01, fill="grey", color = NA, show.legend = F)+
  geom_rect(data=filter(DE.counts, Treatment=="C"), aes(xmin = 4, xmax=5, ymin=-Inf, ymax=Inf), alpha=0.01, fill="grey", color = NA, show.legend = F)+
  geom_rect(data=filter(DE.counts, Treatment=="A"), aes(xmin = 2, xmax=3, ymin=-Inf, ymax=Inf), alpha=0.01, fill="grey", color = NA, show.legend = F)+
  geom_rect(data=filter(DE.counts, Treatment=="B"), aes(xmin = 3, xmax=4, ymin=-Inf, ymax=Inf), alpha=0.01, fill="grey", color = NA, show.legend = F)+
  geom_rect(data=filter(DE.counts, Treatment=="C"), aes(xmin = 4, xmax=5, ymin=-Inf, ymax=Inf), alpha=0.01, fill="grey", color = NA, show.legend = F)+
  geom_rect(data=filter(DE.counts, Treatment=="A"), aes(xmin = 2, xmax=3, ymin=-Inf, ymax=Inf), alpha=0.01, fill="grey", color = NA, show.legend = F)+
  geom_rect(data=filter(DE.counts, Treatment=="B"), aes(xmin = 3, xmax=4, ymin=-Inf, ymax=Inf), alpha=0.01, fill="grey", color = NA, show.legend = F)+
  geom_rect(data=filter(DE.counts, Treatment=="C"), aes(xmin = 4, xmax=5, ymin=-Inf, ymax=Inf), alpha=0.01, fill="grey", color = NA, show.legend = F)+
  geom_rect(data=filter(DE.counts, Treatment=="A"), aes(xmin = 2, xmax=3, ymin=-Inf, ymax=Inf), alpha=0.01, fill="grey", color = NA, show.legend = F)+
  geom_rect(data=filter(DE.counts, Treatment=="B"), aes(xmin = 3, xmax=4, ymin=-Inf, ymax=Inf), alpha=0.01, fill="grey", color = NA, show.legend = F)+
  geom_rect(data=filter(DE.counts, Treatment=="C"), aes(xmin = 4, xmax=5, ymin=-Inf, ymax=Inf), alpha=0.01, fill="grey", color = NA, show.legend = F)+
  facet_wrap(vars(Treatment), labeller = labeller(Treatment = c("A" = "Drought at Stem Elongation", "B" = "Drought at Booting", "C" = "Drought at Heading", "D" = "Control")))+
  theme_bw()+
  theme( axis.title.x = element_blank(), axis.text.x = element_blank(), legend.title = element_blank() )

DE.line

#Only all transcripts, all lines one panel
DE.line.all <- ggplot(data = DE.counts[DE.counts$Subset=="All",], aes(x = GrowthStage, y = DEcount, color = Treatment, group = Treatment))+
  geom_point()+
  geom_line()+
  ylab("DA transcripts counts")+
  scale_color_brewer(name = "Treatment", labels = c("Drought at Stem Elongation", "Drought at Booting", "Drought at Heading", "Control"), palette = "Set1")+
  scale_y_log10(name = "DA transcripts count",  labels = scales::label_log(base=10, digits=2))+
  theme_bw()#+
  #theme( axis.title.x = element_blank(), axis.text.x = element_blank(), legend.title = element_blank() )

DE.line.all

##Dissimilarity
#Import
bray.control <- readRDS(file = here("data", "intermediate", "bray.control.RDS"))

#Make sure last column is numeric
bray.control$Dissimilarity <- as.numeric(bray.control$Dissimilarity)

#Reorder manually
bray.control$GrowthStage <- factor(bray.control$GrowthStage, c("Tillering", "Stem Elongation", "Booting", "Heading", "Flowering" ))

#Plot
diss.control.line <- ggplot(data = bray.control, aes(x = GrowthStage, y = Dissimilarity, color = Subset, group = Subset))+
  geom_point()+
  geom_line(stat = "summary", fun = "mean")+
  ylab("Bray-Curtis dissimilarity")+
  geom_rect(data=filter(bray.control, Treatment=="A"), aes(xmin = 2, xmax=3, ymin=-Inf, ymax=Inf), alpha=0.01, fill="grey", color = NA, show.legend = F)+
  geom_rect(data=filter(bray.control, Treatment=="B"), aes(xmin = 3, xmax=4, ymin=-Inf, ymax=Inf), alpha=0.01, fill="grey", color = NA, show.legend = F)+
  geom_rect(data=filter(bray.control, Treatment=="C"), aes(xmin = 4, xmax=5, ymin=-Inf, ymax=Inf), alpha=0.01, fill="grey", color = NA, show.legend = F)+
  facet_wrap(vars(Treatment), labeller = labeller(Treatment = c("A" = "Drought at Stem Elongation", "B" = "Drought at Booting", "C" = "Drought at Heading", "D" = "Control")))+
  theme_bw()+
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1), legend.title = element_blank())

diss.control.line

###Plots for DE counts and dissimilarity - growth stages against previous

##Dissimilarity
#Import
diss.reps <- readRDS(file = here("data", "intermediate", "diss.reps.RDS"))

#Make sure last column is numeric
diss.reps$Dissimilarity <- as.numeric(diss.reps$Dissimilarity)

#Reorder manually
diss.reps$GrowthStages <- factor(diss.reps$GrowthStages, c("TSE", "SEB", "BH", "HF" ))
diss.reps$Treatment <- factor(diss.reps$Treatment, c("SE-A", "B-B", "H-C", "CTRL-D"))

#Plot
diss.stages.line <- ggplot(data = diss.reps, aes(x = GrowthStages, y = Dissimilarity, fill = Treatment))+
  geom_boxplot(outlier.color = NA, outlier.size = 0, outlier.shape = NA)+
  facet_wrap(vars(Subset))+
  geom_point(position = position_jitterdodge(), aes(color = Treatment))+
  scale_x_discrete(name = element_blank(), labels = c("T vs. SE", "SE vs. B", "B vs. H", "H vs. F")) +
  scale_fill_discrete(labels = c("Drought at SE", "Drought at B", "Drought at H", "CTRL"))+
  guides(color = "none")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1), legend.title = element_blank())
  
diss.stages.line  

#DE
#Import
DE.counts.res <- readRDS(file = here("data", "intermediate", "DE.counts.res.RDS"))

#Make sure last column is numeric
DE.counts.res$DEcount <- as.numeric(DE.counts.res$DEcount)

#Fix problem
DE.counts.res$Contrast <- gsub("H vs. B", "B vs. H", DE.counts.res$Contrast)

#Reorder manually
DE.counts.res$Contrast <- factor(DE.counts.res$Contrast, c("T vs. SE", "SE vs. B", "B vs. H", "H vs. F" ))
DE.counts.res$Treatment <- factor(DE.counts.res$Treatment, c("A", "B", "C", "D"))

#Plot
DE.stages.bar <- ggplot(data = DE.counts.res, aes(x = Contrast, y = DEcount, fill = Treatment))+
  facet_wrap(vars(Subset), scales = "free")+
  geom_bar(stat="identity", position=position_dodge())+
  scale_x_discrete(name = element_blank()) +
  scale_fill_discrete(labels = c("Drought at SE", "Drought at B", "Drought at H", "CTRL"))+
  scale_y_log10(name = "DA transcripts count")+
  guides(color = "none")+
  theme_bw()+
  theme(axis.text.x = element_blank(), legend.title = element_blank())

DE.stages.bar

#Plot -- only ALL
DE.counts.res.all <- DE.counts.res[DE.counts.res$Subset == "All",] 
DE.stages.bar.all <- ggplot(data = DE.counts.res.all, aes(x = Contrast, y = DEcount, fill = Treatment))+
  geom_bar(stat="identity", position=position_dodge(), color = "black")+
  scale_x_discrete(name = "Contrast") +
  scale_fill_brewer(palette="Set1", labels = c("Drought at SE", "Drought at B", "Drought at H", "CTRL"))+
  scale_y_log10(name = "DA transcripts count", limits = c(1,1e+6), expand = c(0,0), labels = scales::label_log(base=10, digits=1))+
  theme_bw()
DE.stages.bar.all
