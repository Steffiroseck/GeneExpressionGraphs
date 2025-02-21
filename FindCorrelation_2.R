#assuming input data (DMI_data) is loaded.
# categorising columns names
  meta <- c("Group", "Initial.Date", "Final.Date")
  randomEffects <- c("LambID", "Animal")
  categoricalf <- c("Trial", "Treatment")
  # multiple responses available but I will focus on DMI and define individually
  target <- "DMI"
  
  
  # all remaining features
  continuousf <- names(DMI_data)[!names(DMI_data) %in% c(randomEffects, categoricalf, meta,target)]
  colSums(is.na(DMI_data[,continuousf]))
  
  ################################################################################ PAIRWISE CORRELATIONS
  # Identifying pairwise correlations in corrected dataset
  # CHP ignored during pairwise correlations
  
  library(caret)
  
  cdf <- DMI_data[, continuousf]
  #head(cdf)
  corrmat <- cor(cdf)
  
  pairwiseFeaturetoDrop <- c()
  caretPairwise <- c()
  # This will return all features with pairwise correlations to drop
  # It first finds features with pairwise correlation
  # then it gets the average correlation of each feature with all remaining features
  # the feature with the lower correlation with all remaining features is kept
  # the feature with the higher correlation with all remaining features is flagged for dropping
  # it also compares this local function to the similar caret findCorrelation function
  # caret compares columns in a slighlty different order which leads to slighlty different results
  # https://rdrr.io/cran/caret/src/R/findCorrelation.R

  
  findPairwise <- function(corrmat, cutoff = 0.75) {
    
    colnames <- colnames(corrmat)
    colnames
    
    # Selects each row
    for (r in 1:(nrow(corrmat) - 1)) {
      # Loops through every column
      for (c in (r + 1):nrow(corrmat)) {
        # Finds corr between column and row
        corr <- corrmat[r, c]
        # Check if the correlation is above the cutoff
        if (abs(corr) > cutoff) {
          # Compare the means of each feature
          meanr <- mean(corrmat[r,])
          meanc <- mean(corrmat[,c])
          
          print(paste0("col ", r, ": ", colnames[r], " and col ", c, ": ", colnames[c], " have a corr of ", round(corr, 3) ))
          print(paste0("col ", r, ": ", colnames[r], " has a mean corr of ", round(meanr, 3), " col ", c, ": ", colnames[c], " has a mean corr of ", round(meanc, 3) ))
          
          # Whichever feature has a highest average mean with remaining features is flagged for dropping
          if (meanr > meanc) {
            print(paste0("flagging ", r, " ", colnames[r], " for dropping"))
            pairwiseFeaturetoDrop <<- append(pairwiseFeaturetoDrop, colnames[r])
          } else if (meanc > meanr) {
            print(paste0("flagging ", c, " ", colnames[c], " for dropping"))
            pairwiseFeaturetoDrop <<- append(pairwiseFeaturetoDrop, colnames[c])
          }
          cat("\n")
        }
        
      }
      
    }
    pairwiseFeaturetoDrop <<- unique(pairwiseFeaturetoDrop)
    
    # this is the same function performed slighlty differently by the caret package for comparison
    # the order it compares features is different, leading to slighlty different results
    caretPairwise <<- caret::findCorrelation(corrmat, cutoff=0.9, names = TRUE)
  }
  
  findPairwise(corrmat, cutoff = 0.75)
  pairwiseFeaturetoDrop
  caretPairwise
  # Shows the features caret returned which were not in my one
  setdiff(pairwiseFeaturetoDrop, caretPairwise)
  # Shows the features pairwisefeature returned which were not in caret
  setdiff(caretPairwise, pairwiseFeaturetoDrop )
  
  # Keep only the columns which were not flagged for dropping. I am using caret results only.
  dfcp <- cdf[,!names(cdf) %in% (caretPairwise)]
  names(dfcp)
  
  # Excluded response so that it wouldnt be dropped during pairwise comparison, 
  # reattach here from original dataset
  dfcp <- dfcp %>% dplyr::mutate(DMI = DMI_data$DMI)

#  Model selection
# scale the variables
# DMI_data <- cbind(DMI_data, scale(DMI_data[,8:22], scale = T))
# names(DMI_data)[23:37] <- paste0('Z.', names(DMI_data)[23:37])

# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# CARET

# keep only the variables that were not multicollinear as identified from findCorrelation()
# Train the new model
step.model <- train(DMI ~.,method = "lmStepAIC", direction = "both", data = dfcp, 
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
full_model = lm(DMI ~., data = dfcp)
summary(full_model)
