library(corrr)

#calculate correlation matrix for data frame
# columns 8 to 22 only contains numeric data

my_cor <- cor(DMI_data[,8:22])

#view correlation matrix
my_cor


library(caret)

#extract significant correlation coefficients
findCorrelation(my_cor, verbose=TRUE, names=TRUE, cutoff = 0.9)

# Returned [1] "ME_req"  "DM_req"  "LWG"     "ADG"     "DMI"     "FE"      "ME_dif"  "Ave_DHA" to be removed
# DMI is correlated with ME_int, so we include ME_int in our reduced data frame and also DMI, since we dont want to remove important variables

# remove these variables from data frame
library(dplyr)

DMI_reduced <- DMI_data %>%
  select(LambID, DMI, Initial_BW, Final_BW, Total_BWG, RFI, FCR, Ave_MO)
head(DMI_reduced)

# run lmStepAIC on this dataframe
# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# CARET

# keep only the variables that were not multicollinear as identified from findCorrelation()
# Train the new model
step.model <- train(DMI ~.,method = "lmStepAIC", direction = "both", data = DMI_reduced, 
                    trControl = train.control,
                    trace = FALSE,
                    preProcess=c("center", "scale")
                    )
# Model accuracy
step.model$results
# Final model coefficients
step.model$finalModel
# Summary of the model
summary(step.model$finalModel)

# check the full model again
full_model = lm(DMI ~., data = DMI_reduced)
summary(full_model)

# check multicollinearity (variance inflation factor)
vif(full_model) #vif .5 or 10 means highly correlated
