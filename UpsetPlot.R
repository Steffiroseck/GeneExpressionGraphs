library(UpSetR)
library(tidyverse)

# load data
data <- read.table("C:/Users/afbi-roses/Steffi_sheep_transcriptomics/Output/DMI_genes_for_upsetPlot.csv",sep=",",
                 header=TRUE, check.names = FALSE,  na.strings = c("", "NA"))

# convert to long format. One column in my data contain more items than other columns leading to add NA in the end 
# while running upset plot. So all NAs need to be dropped.
long_data <- data %>%
  pivot_longer(cols = everything(), names_to = "Condition", values_to = "Gene") %>%
  drop_na()

# create binary matrix
bin_mat <- table(long_data$Gene, long_data$Condition) %>%
  as.data.frame.matrix()

# generate plot and save it
pdf("C:/Users/afbi-roses/Steffi_sheep_transcriptomics/Output/Upset_plot.pdf", width=10, height = 6)
upset(bin_mat,
      sets = colnames(bin_mat),
      sets.bar.color = "dodgerblue",
      order.by = "freq",
      text.scale = 1.5,
      matrix.color = "red",
      #min.bar.color = "purple",
      keep.order = TRUE)
dev.off()

# extract shared_genes
shared_genes <- bin_mat[rowSums(bin_mat)>1, ]
shared_genes
