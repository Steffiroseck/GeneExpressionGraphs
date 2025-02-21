#select covariates
covariates <- DMI_data[,8:22]
# scale the data(standardization: mean =0, sd=1)
covariates_scaled <- scale(covariates)
#perform PCA
pca_result <- prcomp(covariates_scaled, center = TRUE, scale.=TRUE)
summary(pca_result)#proportion of variance explained
# examine loadings
pca_result$rotation #high abs value in otation indicates that a variable is strongly contributing to PC
# visualize
library(ggplot2)
library(ggfortify)
autoplot(pca_result, data = DMI_data, loadings=TRUE, loadings.label = TRUE,
                                               loadings.colour ='blue', loadings.label.colour = 'red', loadings.label.repel=T)

# extract PC1 and PC2 
DMI_data$PC1 <- pca_result$x[,1]
DMI_data$PC2 <- pca_result$x[,2]
