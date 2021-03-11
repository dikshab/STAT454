data <- read.csv("breastCancer.csv")

dim(data)
library(tidyverse)
library(caret)
library(glmnet)
library(tidyr)
library(ggplot2)
dat <- dat[c(2,11)]
dat <- na.omit(data)
tibble::as_tibble(dat)


mine.long <- pivot_longer(data = dat,
                          cols = -c(1,2), 
                          names_to = "Class", 
                          values_to = "Abundance")
head(mine.long)
mine.heatmap <- ggplot(data = mine.long, mapping = aes(x = clump_thickness,
                                                       y = Class,
                                                       fill = Abundance)) +
  geom_tile() +
  xlab(label = "Sample")

mine.heatmap

# Predictor variables
x <- model.matrix(clump_thickness~., train.data)[,-1]
# Outcome variable
y <- train.data$clump_thickness



training.samples <- dat$clump_thickness
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- dat[training.samples, ]
test.data <- dat[-training.samples, ]



lambda <- 10^seq(-3, 3, length = 100)




# Build the model
set.seed(0)
ridge <- train(
  clump_thickness ~., data = train.data, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 0, lambda = lambda)
)
plot(ridge)
# Model coefficients
coef(ridge$finalModel, ridge$bestTune$lambda)
# Make predictions
predictionsRidge <- ridge %>% predict(test.data)
plot(predictionsRidge)
wilcox.test(predictionsRidge)
# Model prediction performance
data.frame(
  RMSE = RMSE(predictionsRidge, test.data$clump_thickness),
  Rsquare = R2(predictionsRidge, test.data$clump_thickness)
)



# Build the model
set.seed(0)
lasso <- train(
  clump_thickness ~., data = train.data, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 1, lambda = lambda)
)
plot(lasso)
# Model coefficients
coef(lasso$finalModel, lasso$bestTune$lambda)
# Make predictions
predictionsLasso <- lasso %>% predict(test.data)
plot(predictionsLasso)
wilcox.test(predictionsLasso)
# Model prediction performance
data.frame(
  RMSE = RMSE(predictionsLasso, test.data$clump_thickness),
  Rsquare = R2(predictionsLasso, test.data$clump_thickness)
)

models <- list(ridge = ridge, lasso = lasso)
resamples(models) %>% summary( metric = "RMSE")


boxplot(predictionsLasso, predictionsRidge)