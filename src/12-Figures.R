###Create figures 

##FIGURE1
fig1 <- ggarrange(pcoa.plot.all.1, pcoa.plot.all.2, labels = c("A", "B", "C"), common.legend = T, legend = "right")
fig1
ggsave(fig1, filename = here("output", "figs", "fig1.tiff"), device = "tiff", compression = "lzw", dpi = 600)
