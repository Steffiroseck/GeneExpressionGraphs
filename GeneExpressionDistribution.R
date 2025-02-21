# Assuming DESeq2 has already been run and you are comparing gene expression w.r.t different conditions (eg; Treatments: control, low, medium, high etc)

# get the normalized counts from deseq2
dds <- estimateSizeFactors(deseq2Data)
norma <- counts(dds, normalized=TRUE)
# save significant genes from deseq2 into another list
sig_gene_list <- rownames(resSig) 
# extract normalized counts for the significant genes
selected_counts <- norma[rownames(norma) %in% sig_gene_list, ]
# transpose and print the results as we want the gene names as columns and samples as rownames. Then save the transposed file into another variable for further analysis.
print(t(selected_counts)) 
resSig_normalized_counts <- as.data.frame(t(selected_counts))
resSig_normalized_counts$LambID <- rownames(resSig_normalized_counts)
MergedDF = merge(resSig_normalized_counts, metaData, by ="LambID")

# If there are differences in the plots when compared to deseq2 log2foldchange values. This is because, DESEq2 uses GLM an uses normalized counts, while linear model assumes data is normally distributed which wont be the case for all RNAseq data sets.

# Plotting gene counts against treatments to see if gene expression increased or decreased
# I reorder the groups order : I change the order of the factor metaData$Treatment
siggenes = rownames(resSig)
dds$Treatment <- factor(dds$Treatment, levels=c("Control", "Low", "Medium", "High"))
for ( i in 1: length(siggenes)){
  plot=plotCounts(dds, gene = i, intgroup = "Treatment", returnData = TRUE)
    print(ggboxplot(plot, x="Treatment", y="count", add="jitter", fill = "Treatment")+
           xlab("microalgae oil intake levels")+ ylab(siggenes[i]))
}

# Visualization of ANOVA and post-hoc tests on the same plot
MergedDF$Treatment <- as.character(MergedDF$Treatment)
MergedDF$Treatment <- factor(MergedDF$Treatment, levels=c("Control", "Low", "Medium", "High"))
x <- which(names(MergedDF) == "Treatment") # name of grouping variable
y <- names(MergedDF[,2:111])
method1 <- "anova" # one of "anova" or "kruskal.test"
method2 <- "t.test" # one of "wilcox.test" or "t.test"
my_comparisons <- list(c("Control", "High"), c("Control", "Medium"), c("Control", "Low"), c("Low", "High"), c("Medium", "High")) # comparisons for post-hoc tests
for (i in y) {
  for (j in x) {
    p <- ggboxplot(MergedDF,
                   x = colnames(MergedDF[j]), y = colnames(MergedDF[i]),
                   color = colnames(MergedDF[j]),
                   legend = "none",
                   palette = "jco", add="jitter", shape ="Treatment", labelOutliers=TRUE,
                   outlierColor = "red")
                   #,xlab="Microalgae oil intake (g/kg dry matter)", ylab = "Dry matter intake (kg/day)")
    # Add p-value
    print(
      p + stat_compare_means(aes(label = paste0(after_stat(method1), ", p-value = ", after_stat(p.format))),
                             method = method1, label.y = max(MergedDF[, i], na.rm = TRUE))
      + stat_compare_means(comparisons = my_comparisons, method = method2, label = "p.format"))
  }
}
