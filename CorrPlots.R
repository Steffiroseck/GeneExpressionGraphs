```{r corr_plots}
# Plotting correlation plots to see which variables are correlated with DMI
library(corrplot)
library(RColorBrewer)

# corr plot requires data to be numeric, hence we select only the columns with values
M <-cor(DMI_data[,8:22])
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

#corrplot(M, method="number")

# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp <- cor.test(mat[, i], mat[, j], ...)
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        }
    }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(DMI_data[,8:22])
#head(p.mat[, 1:5])
# Specialized the insignificant value according to the significant level
corrplot(M, type="upper", order="hclust", 
         p.mat = p.mat, sig.level = 0.05)
