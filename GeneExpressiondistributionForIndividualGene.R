#Assuming DESEq2 has already been run and metadata has been loaded

# codes to get normalized counts for individual gene

 IL12RB1 <- norma["IL12RB1",]
 BBS12 <- norma["BBS12",]
 LOC101119041 <- norma["LOC101119041",]
 IL12RB1 = as.data.frame(IL12RB1)
 BBS12 = as.data.frame(BBS12)
 LOC101119041 = as.data.frame(LOC101119041)
 IL12RB1$LambID = rownames(IL12RB1)
 BBS12$LambID = rownames(BBS12)
 LOC101119041$LambID = rownames(LOC101119041)
 head(metaData)
 merge1 = merge(IL12RB1,metaData, by = "LambID")
 merge2= merge(BBS12, metaData, by = "LambID")
 merge3 = merge(LOC101119041, metaData, by ="LambID")
 # join these data frames together
 MergedDF <- merge(merge1, merge2) %>%
               merge(merge3)

# Another method to plot gene counts with increasing DMI
ggplotRegression <- function (fit) {
require(ggplot2)
ggplot(fit$model, aes_string(x = names(fit$model)[1], y = names(fit$model)[2])) + 
  xlab("scaled Dry matter intake") +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                     "Intercept =",signif(fit$coef[[1]],5 ),
                     " Slope =",signif(fit$coef[[2]], 5),
                     " P =",signif(summary(fit)$coef[2,4], 5)))
}

fit1 <- lm(scaled_DMI ~ IL12RB1, data = merge1)
p1=ggplotRegression(fit1) + ylab("Normalized IL12RB1 counts")
genecounts_1 = file.path(out_dir, "IL12RB1_genecounts_slope.pdf")
ggsave(filename = genecounts_1, plot = p1)
fit2 <- lm(scaled_DMI ~ BBS12, data = merge2)
p2=ggplotRegression(fit2) + ylab("Normalized BBS12 counts")
genecounts_2 = file.path(out_dir, "BBS12_genecounts_slope.pdf")
ggsave(filename = genecounts_2, plot = p2)
fit3 <- lm(scaled_DMI ~ LOC101119041, data = merge3)
p3=ggplotRegression(fit3) + ylab("Normalized LOC101119041 counts")
genecounts_3 = file.path(out_dir, "LOC101119041_genecounts_slope.pdf")
ggsave(filename = genecounts_3, plot = p3)
