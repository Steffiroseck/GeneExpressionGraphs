library(ggplot2)
library(tidyverse)
library(car)
library(ggpubr)
library(rstatix)
library(gridExtra)
library(car)
library(report)
library(plotrix)
library(corrplot)
library(Hmisc)
library(kableExtra)
library(reshape2)

# Read the input file
DMI_data=read.csv("C:/Users/afbi-roses/Steffi_sheep_transcriptomics/Data/DMI/36lambs_all_phenotypes.csv")

# Barplot to see number of treatment samples
barplot(table(DMI_data$Treatment),
        xlab="Treatments",
        ylab="frequency")
# From the barplot it is clear that all groups have equal samples.

# get the table of counts using the code below:
DMI_data %>% count(Treatment)

## Calculate the IQR
IQR <- IQR(DMI_data$DMI)

# Calculate the bin width using the Freedman-Diaconis rule
bin_width_fd <- 2 * IQR / length(DMI_data$DMI)^(1/3)

# Calculate the bin width using Sturges' rule
bin_width_sturgess <- (max(DMI_data$DMI) - min(DMI_data$DMI)) / (1 + 3.3 * log10(length(DMI_data$DMI)))

# Calculate the bin width using Scott's rule
bin_width_scott <- 3.49 * sd(DMI_data$DMI) / length(DMI_data$DMI)^(1/3)

# # Create histograms using the different bin widths
# hist(DMI_data$DMI, breaks = seq(min(DMI_data$DMI), max(DMI_data$DMI), by = bin_width_fd))
# hist(DMI_data$DMI, breaks = seq(min(DMI_data$DMI), max(DMI_data$DMI), by = bin_width_sturgess))
# hist(DMI_data$DMI, breaks = seq(min(DMI_data$DMI), max(DMI_data$DMI), by = bin_width_scott))
# To look distribution of a continuous variable, you can plot a histogram; for example DMI
ggplot(data = DMI_data) +
  geom_histogram(mapping = aes(x = DMI), binwidth = 0.18)

# if you want to compute this by hand;
DMI_data %>% 
  count(cut_width(DMI, 0.18))

# See how the DMI varies with increasing levels of microalgae oil
ggplot(data = DMI_data, mapping = aes(x = DMI, colour = Treatment)) +
  geom_freqpoly(binwidth = 0.18)

temp<-density(table(DMI_data$Ave_MO))
plot(temp, type="n",
     main="Amount of microalgae oil intake by lambs")
polygon(temp, col="lightgray",
        border="gray")

boxplot(DMI_data$Ave_MO,
        main="Amount of microalgae oil intake by lambs")

# check normality of data and distribution
DMI_data$Treatment <- factor(DMI_data$Treatment, levels=c("Control", "Low", "Medium", "High"))

# analysis of variance
anova <- aov(DMI ~ Treatment, data = DMI_data)
summary(anova)

# Tukey's test
tukey <- TukeyHSD(anova)
print(tukey)

# check normality. If P-value is greater than 0.05, data is normally distributed

shapiro.test(DMI_data$DMI) # normally distributed
shapiro.test(DMI_data$Ave_MO) #not normally distributed


# compute MANOVA
ibw = DMI_data$DMI 
fbw= DMI_data$LWG
res.man <- manova(cbind(ibw, fbw) ~ Treatment, data = DMI_data)
summary(res.man)
