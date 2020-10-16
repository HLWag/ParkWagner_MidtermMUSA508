MiamiProperties_below10m <-
  MiamiProperties %>%
  filter(SalePrice <10000000)


set.seed(31337)

# get index for training sample
inTrain <- caret::createDataPartition(
  y = paste(MiamiProperties_below10m$Neighborhood),
  p = .60, list = FALSE)
# split data into training and test HLW Note: square bracket mean index by row (left entry) and index by column (second entry) negative means opposite
Miami.training <- MiamiProperties_below10m[inTrain,] 
Miami.test     <- MiamiProperties_below10m[-inTrain,]  

#Regression
reg_final_train <- lm(SalePrice ~ ., data = st_drop_geometry(MiamiProperties_below10m) %>% 
                        dplyr::select(SalePrice, LotSize, Bed, Bath, Age, ActualSqFt, parks_nn1, MedHHInc, pctHis, pctBachelors, pool, 
                                      singlefamily, pctOwnerHH, pctCarCommute, pctHH200kOrMore, Halfmile_metro, luxury, elevator, dock, sexualoffenders_Buffer, estates))


plot(x = predict(reg_final_train), y = Miami.training$SalePrice)
# Run this a number of times to see Adjusted R2

## predicting on new data
reg_final_predict <- predict(reg_final_train, newdata = Miami.test)


## Mean Square Error train and test
rmse.train <- caret::MAE(predict(reg_final_train), Miami.training$SalePrice)
rmse.test  <- caret::MAE(reg_final_predict, Miami.test$SalePrice)

cat("Train MAE: ", as.integer(rmse.train), " \n","Test MAE: ", as.integer(rmse.test))

#Plotting accuracy metrics
preds.train <- data.frame(pred   = predict(reg_final_train),
                          actual = Miami.training$SalePrice,
                          source = "training data")
preds.test  <- data.frame(pred   = reg_final_predict,
                          actual = Miami.test$SalePrice,
                          source = "testing data")
preds <- rbind(preds.train, preds.test)

ggplot(preds, aes(x = pred, y = actual, color = source)) +
  geom_point() +
  geom_smooth(method = "lm", color = "green") +
  geom_abline(color = "orange") +
  coord_equal() +
  theme_bw() +
  facet_wrap(~source, ncol = 2) +
  labs(title = "Comparing predictions to actual values",
       x = "Predicted Value",
       y = "Actual Value") +
  theme(
    legend.position = "none"
  )