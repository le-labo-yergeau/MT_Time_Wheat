###Create figures 

##FIGURE1
fig1a <- ggarrange(pcoa.plot.all.1, pcoa.plot.all.2, labels = c("A", "B"), common.legend = F, legend = "right")
fig1a
fig1b <- ggarrange(fig1a, stack.phylum.2, labels = c("", "C"), common.legend = F, legend = "right", nrow = 2, heights = c(1,2))
fig1b
ggsave(fig1b, filename = here("output", "figs", "fig1.tiff"), device = "tiff", compression = "lzw", dpi = 600, width = 8, height =  10, units = "in")
ggsave(fig1b, filename = here("output", "figs", "fig1.pdf"), device = "pdf", width = 8, height =  10, units = "in")


##FIGURE2
fig2 <- stack.cog.2
ggsave(fig2, filename = here("output", "figs", "fig2.tiff"), device = "tiff", compression = "lzw", dpi = 600, width = 7, height =  7, units = "in")
ggsave(fig2, filename = here("output", "figs", "fig2.pdf"), device = "pdf",  width = 7, height =  7, units = "in")


##FIGURE3
fig3a <- ggarrange(DE.line.all, DE.stages.bar.all, labels = c("A", "B"), common.legend = T, legend.grob = get_legend(DE.stages.bar.all), legend = "right", nrow = 1)
fig3a
fig3b <- ggarrange(fig3a, upset.plot, labels = c("", "C"), common.legend = F, nrow = 2)
fig3b
ggsave(fig3b, filename = here("output", "figs", "fig3.tiff"), device = "tiff", compression = "lzw", dpi = 600, width = 12, height =  7, units = "in")
ggsave(fig3b, filename = here("output", "figs", "fig3.pdf"), device = "pdf", width = 12, height =  7, units = "in")

##FIGURE4
#create Fig4
fig4 <- ggarrange(stack.genus.DA, stack.fun.DA, labels = c("A","B"), common.legend = F, nrow=2)
fig4
ggsave(fig4, filename = here("output", "figs", "fig4.tiff"), compression = "lzw", dpi = 600, device = "tiff", height = 14, width = 14, units = "in")
ggsave(fig4, filename = here("output", "figs", "fig4.pdf"), device = "pdf", height = 14, width = 14, units = "in")

##FIGURE5
#Create Fig5
fig5 <- ggarrange(upset.plot.TSE, upset.plot.SEB, upset.plot.BH, upset.plot.HF, labels = c("A","B", "C", "D"), common.legend = F, nrow=2, ncol = 2)
fig5
ggsave(fig5, filename = here("output", "figs", "fig5.tiff"), compression = "lzw", dpi = 600, device = "tiff", height = 7, width = 14, units = "in")
ggsave(fig5, filename = here("output", "figs", "fig5.pdf"), device = "pdf", height = 7, width = 14, units = "in")

##FIGURE6
#Create Fig6
fig6 <- ggarrange(stack.genus.stage.DA, stack.fun.stage.DA, labels = c("A","B"), common.legend = F, nrow=2)
fig6
ggsave(fig6, filename = here("output", "figs", "fig6.tiff"), compression = "lzw", dpi = 600, device = "tiff", height = 14, width = 14, units = "in")
ggsave(fig6, filename = here("output", "figs", "fig6.pdf"), device = "pdf", height = 14, width = 14, units = "in")
