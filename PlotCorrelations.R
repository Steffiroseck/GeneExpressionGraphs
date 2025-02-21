# This code assumes that input data is already loaded.

#pdf("Anova_boxplot_EDA_24Lambs.pdf")
ggboxplot(DMI_data, x = "Treatment", y = "DMI",
          color = "Treatment", palette = "jco",
          add="jitter", shape = "Treatment", labelOutliers = TRUE,
  outlierColor = "red",xlab="Microalgae oil intake (g/kg dry matter)", ylab = "Dry matter intake (kg/day)")+
  stat_compare_means(method = "anova", aes(label = paste0("ANOVA, p = ", ..p.format..)))
#dev.off()

# Plot of DMI against unadjusted microalgae oil intake (continuous)
ggscatter(DMI_data, x = "Ave_MO", y = "DMI", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Microalgae oil (g/kg)", ylab = "Dry matter intake")

# adding point ;labels - LambID
ggscatter(DMI_data, x = "Ave_MO", y = "DMI", 
          add = "reg.line", conf.int = TRUE,  add.params = list(color = "blue",
                            fill = "lightgray"),cor.coef = TRUE, cor.method = "pearson",
          xlab = "Microalgae oil intake (g/kg dry matter)", ylab = "Dry matter intake (g/day)")

# Plot of DMI against initial and final bw intake (continuous)
ggscatter(DMI_data, x = "DMI", y = "Initial_BW", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Dry matter intake (g/day)", ylab = "Initial_BW (kg)")

# Plot of DMI against initial and final bw intake (continuous)
ggscatter(DMI_data, x = "DMI", y = "Final_BW", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Dry matter intake (g/day)", ylab = "Final_BW (kg)")

# Plot of DMI against Total BWG (continuous)
ggscatter(DMI_data, x = "DMI", y = "Total_BWG", 
          add = "reg.line", conf.int = TRUE,  add.params = list(color = "blue",
                            fill = "lightgray"),cor.coef = TRUE, cor.method = "pearson",
          xlab = "Dry matter intake (g/day)", ylab = "Total body weight gain (kg)")

# Plot of DMI against ADG (continuous)
ggscatter(DMI_data, x = "DMI", y = "ADG", 
          add = "reg.line", conf.int = TRUE,  add.params = list(color = "blue",
                            fill = "lightgray"),cor.coef = TRUE, cor.method = "pearson",
          xlab = "Dry matter intake (g/day)", ylab = "TAverage daily gain")

# Plot of Ave MO against initial and final bw intake (continuous)
ggscatter(DMI_data, x = "Ave_MO", y = "DMI", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "", ylab = "Dry matter intake (Kg/day)")

ggscatter(DMI_data, x = "Ave_MO", y = "Total_BWG", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Microalgae oil intake (kg/dry matter)", ylab = "Total body weight gain (kg)")

# ggline(DMI_data, x = "Ave_MO", y = "Initial_BW",
#           add = "reg.line", conf.int = TRUE, 
#           cor.coef = TRUE, cor.method = "pearson",
#           xlab = "Microalgae oil intake (kg/dry matter)", ylab = "Initial body weight (kg)")

ggscatter(DMI_data, x = "Ave_MO", y = "Final_BW", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Microalgae oil intake (kg/dry matter)", ylab = "Final body weight (kg)")
          
