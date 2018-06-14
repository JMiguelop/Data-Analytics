library(ggplot2)
library(gplots)
library(caret)
library(ROSE)
library(DMwR)
library(rpart)
library(rpart.plot)
library(randomForest)
library(nnet)
library(pROC)
library(car)
library(rminer)



###  Loading dataset  ###
sales <- read.csv("~/Desktop/SalesKaggle3.csv")



###  Get a first sense of the data  ###
dim(sales)
colnames(sales)
head(sales)



###  Tidy the column names (remove "_" symbol)  ###
colnames(sales) <- gsub("_","", colnames(sales))
colnames(sales)



###  Get a more detailed sense of the data. Initial exploration of atributes considered relevant at first glance - FileType, SoldFlag, MarketingType  ###
str(sales)
summary(sales)

table(sales$FileType)                                       # QUANTIDADE de registos do tipo "Historical" e "Active"
prop.table(table(sales$FileType))*100                       # PERCENTAGEM de registos do tipo "Historical" e "Active"
table(sales$FileType, sales$SoldFlag)                       # QUANTIDADE de registos do tipo "Historical" e "Active" que foram vendidos(1) e não vendidos(2). Note-se que os "Active" estão a 0 uma vez que não possuem informação acerca se foram ou não vendidos.
prop.table(table(sales$FileType, sales$SoldFlag))*100       # PERCENTAGEM de produtos vendidos(1) e não vendidos(2) para cada tipo de registo ("Historical", "Active"). 
# Em alternativa à função "prop.table" utilizada acima podia-se efetuar o calculo de forma manual. E.g.: ((sum(sales$FileType == "Historical" & sales$SoldFlag == "0")/sum(sales$FileType == "Historical")))*100.
table(sales$MarketingType)                                  # QUANTIDADE de produtos em cada tipo de marketing. Atenção que inclui os produtos "Active" que não possuem informação acerca dos que foram vendidos e da quantidade vendida.
table(sales$MarketingType, sales$FileType == "Historical")  # QUANTIDADE de produtos em cada tipo de marketing para cada tipo de registo ("TRUE -> Historical", "FALSE -> Active")



###  Split the sales data into "Historical" and "Active"  ###
historical_sales <- subset(sales, FileType == "Historical")
active_sales <- subset(sales, FileType == "Active")



###  Plot the distribution of the attributes (barplot's and histogram's are used) of historical sales  ###
par(mfrow=c(4,2))
for (att in colnames(historical_sales)[c(5, 8, 11, 12)]){
  barplot(table(historical_sales[,att]), main = att, col = "darkorange", xlab = "Value", ylab = "Frequency", border = NA)
}
for (att in colnames(historical_sales)[c(9, 10, 13, 14)]){
  hist(historical_sales[,att], main = att, col = "darkorange", xlab = "Value", border = NA)
}
par(mfrow=c(1,1))



###  Check the correlation between attributes  ###
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(historical_sales[,c(-1,-2,-3,-6,-7)], lower.panel = panel.cor, upper.panel = panel.smooth)

# Alternative visualization where only the correlated values are shown as "heatmap"
corMatrix <- cor(historical_sales[,c(-1,-2,-3,-6,-7)])
heatmap(corMatrix)
heatmap(corMatrix, col = topo.colors(16))

# Inprove the visualization of the heatmap and save to a PDF
pdf("Correlation_Heatmap_3.pdf",w=12, h=12)
par(oma=c(10,0,0,10))
heatmap.2(corMatrix, col = redblue(16),trace="none")
dev.off()



###  Check for outliers in the variables  ###
par(mfrow = c(2, 2))
boxplot(historical_sales$SoldCount, main = "SoldCount", col = "darkorange")
boxplot(historical_sales$ReleaseYear, main = "ReleaseYear", col = "darkorange")
boxplot(historical_sales$ItemCount, main = "ItemCount", col = "darkorange")
boxplot(historical_sales$StrengthFactor, main = "StrengthFactor", col = "darkorange")
par(mfrow = c(1, 1))



###  Inpact of the variables (considered the most important) over the product selling ("SoldFlag") of Historical Sales  ###
# Impacto de "MarketingType" sobre as vendas
#table(historical_sales$MarketingType, historical_sales$SoldFlag)
ggplot(historical_sales, aes(MarketingType, fill=as.factor(SoldFlag))) + 
  geom_bar(alpha = 0.65) + 
  scale_fill_manual("SoldFlag", values = c("darkorange", "red")) +
  xlab("MarketingType") +
  ylab("Total SKU count") +
  ggtitle("Overview of MarketingType impact on sales")

# Impacto de "NewReleaseFlag" sobre as vendas
#table(historical_sales$NewReleaseFlag, historical_sales$SoldFlag) # Esquerda -> NewReleaseFlag. Cima -> SoldFlag
ggplot(historical_sales, aes(as.factor(NewReleaseFlag), fill=as.factor(SoldFlag))) +
  geom_bar(alpha = 0.65) +
  scale_fill_manual("SoldFlag", values = c("darkorange", "red")) +
  xlab("NewReleaseFlag") +
  ylab("Total SKU count") +
  ggtitle("Overview of NewReleaseFlag impact on sales")

# Impacto de "StrengthFactor" sobre as vendas. Duas representações gráficas.
ggplot(historical_sales, aes(SoldFlag, StrengthFactor, fill=as.factor(SoldFlag))) + 
  geom_boxplot(alpha = 0.65) +
  scale_fill_manual("SoldFlag", values = c("darkorange", "red")) +
  ylim(c(0, 17000000)) +
  xlab("SoldFlag") +
  ylab("StrengthFactor values") +
  ggtitle("Overview of StrengthFactor impact on sales")
ggplot(historical_sales, aes(StrengthFactor, fill=as.factor(SoldFlag))) + 
  geom_bar(alpha = 0.65) +
  scale_fill_manual("SoldFlag", values = c("darkorange", "red")) +
  facet_wrap(~SoldFlag) +
  xlab("StrengthFactor values") +
  ylab("Total SKU count") +
  ggtitle("Overview of StrengthFactor impact on sales by SoldFlag")

# Impacto de "ReleaseYear" sobre as vendas
ggplot(historical_sales, aes(as.factor(SoldFlag), ReleaseYear, fill=as.factor(SoldFlag))) + 
  geom_violin(alpha = 0.65) + 
  scale_fill_manual("SoldFlag", values = c("darkorange", "red")) +
  xlab("SoldFlag") +
  ylab("ReleaseYear") +
  ggtitle("Overview of ReleaseYear impact on sales")

# Impacto de "PriceReg" sobre as vendas
ggplot(historical_sales, aes(as.factor(SoldFlag), PriceReg, fill=as.factor(SoldFlag))) + 
  geom_boxplot(alpha = 0.65) +
  scale_fill_manual("SoldFlag", values = c("darkorange", "red")) +
  ylim(c(0, 500)) +
  xlab("SoldFlag") +
  ylab("PriceReg values") +
  ggtitle("Overview of PriceReg impact on sales")

# Impacto de "LowUserPrice" sobre as vendas
ggplot(historical_sales, aes(as.factor(SoldFlag), LowUserPrice, fill=as.factor(SoldFlag))) + 
  geom_boxplot(alpha = 0.65) +
  scale_fill_manual("SoldFlag", values = c("darkorange", "red")) +
  ylim(c(0, 500)) +
  xlab("SoldFlag") +
  ylab("LowUserPrice values") +
  ggtitle("Overview of LowUserPrice impact on sales")

# Impacto de "LowNetPrice" sobre as vendas
ggplot(historical_sales, aes(as.factor(SoldFlag), LowNetPrice, fill=as.factor(SoldFlag))) + 
  geom_boxplot(alpha = 0.65) +
  scale_fill_manual("SoldFlag", values = c("darkorange", "red")) +
  ylim(c(0, 500)) +
  xlab("SoldFlag") +
  ylab("LowNetPrice values") +
  ggtitle("Overview of LowNetPrice impact on sales")



###  A closer look at the price variables :: PriceReg, LowUserPrice, LowNetPrice  ###
# Compare the price variables based on their prices
pr <- historical_sales$PriceReg
lup <- historical_sales$LowUserPrice
lnp <- historical_sales$LowNetPrice

par(mfrow=c(1,3))
plot(pr, col = "darkorange", ylab = "PriceReg values", xlab = "", ylim = c(0,550), main = "PriceReg")
plot(lup, col = "darkblue", ylab = "LowUserPrice values", xlab = "", ylim = c(0,550), main = "LowUserPrice")
plot(lnp, col = "darkgreen", ylab = "LowNetPrice values", xlab = "", ylim = c(0,550), main = "LowNetPrice")

par(mfrow=c(1,2))
boxplot(pr, lup, lnp, col = c("darkorange", "darkblue", "darkgreen"), ylab = "Price values", ylim = c(0,550))
plot(pr, ylim = c(0,550), col = "darkorange", ylab = "Price values", xlab = "")
points(lup, col = "darkblue")
points(lnp, col = "darkgreen")
par(mfrow=c(1,1))

# Sheck if there is a significante correlation between price variables and sales (SoldFlag)
# No relatorio fazer uma referencia aos valores dos gráficos anteriores que mostram a relação entre os vários tipos de preços e as vendas (são os ultimos 3 gráficos desenhados com o ggplot2)
# PriceReg:
PriceReg.Sale = historical_sales[historical_sales$SoldFlag==1, c("PriceReg")]
PriceReg.NoSale = historical_sales[historical_sales$SoldFlag==0, c("PriceReg")]
t.test(PriceReg.Sale, PriceReg.NoSale)

# LowUserPrice:
LowUserPrice.Sale = historical_sales[historical_sales$SoldFlag==1, c("LowUserPrice")]
LowUserPrice.NoSale = historical_sales[historical_sales$SoldFlag==0, c("LowUserPrice")]
t.test(LowUserPrice.Sale, LowUserPrice.NoSale)

# LowNetPrice:
LowNetPrice.Sale = historical_sales[historical_sales$SoldFlag==1, c("LowNetPrice")]
LowNetPrice.NoSale = historical_sales[historical_sales$SoldFlag==0, c("LowNetPrice")]
t.test(LowNetPrice.Sale, LowNetPrice.NoSale)



#############################################################################################################
###                                         MODELS
#############################################################################################################

###  PROBABILIDADE DE VENDA DE CADA PRODUTO  ###
set.seed(1234)
train <- sample(1:nrow(historical_sales), size = ceiling(0.7*nrow(historical_sales)), replace = FALSE)
train_sales <- historical_sales[train,]
test_sales <- historical_sales[-train,]

train_sales$SoldFlag <- factor(train_sales$SoldFlag)
test_sales$SoldFlag <- factor(test_sales$SoldFlag)



# Classification:             ----  LOGISTIC REGRESSION MODEL  ----
logistic_model <- glm(SoldFlag ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg + ItemCount, family = binomial(link = 'logit'), data = train_sales)
summary(logistic_model)

# Obtain the probability of selling for each product
logistic_probs = predict(logistic_model, test_sales, type = "response")

# Binary classification: Class 0 -> prob <= 0.5; Class 1 -> prob > 0.5
logistic_class = rep(0, nrow(test_sales))
logistic_class[logistic_probs > 0.5] = 1

# Confusion matrix and metric's results
confusionMatrix(logistic_class, test_sales$SoldFlag)

# The results show poor Specificity because of inbanlanced SoldFlag class
# Solutions: Oversampling, Undersampling, Both Oversampling and Undersampling, SMOTE
over_train_sales <- ovun.sample(SoldFlag ~., data = train_sales, method = "over")$data
table(over_train_sales$SoldFlag)

under_train_sales <- ovun.sample(SoldFlag ~., data = train_sales, method = "under")$data
table(under_train_sales$SoldFlag)

both_train_sales <- ovun.sample(SoldFlag ~., data = train_sales, method = "both")$data
table(both_train_sales$SoldFlag)

smote_train_sales <- SMOTE(SoldFlag ~., data = train_sales)

# Logistic regression for the "balanced" data
over_logistic_model <- glm(SoldFlag ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg + ItemCount, family = binomial(link = 'logit'), data = over_train_sales)
summary(over_logistic_model)

under_logistic_model <- glm(SoldFlag ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg + ItemCount, family = binomial(link = 'logit'), data = under_train_sales)
summary(under_logistic_model)

both_logistic_model <- glm(SoldFlag ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg + ItemCount, family = binomial(link = 'logit'), data = both_train_sales)
summary(both_logistic_model)

smote_logistic_model <- glm(SoldFlag ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg + ItemCount, family = binomial(link = 'logit'), data = smote_train_sales)
summary(smote_logistic_model)

# Obtain the new probabilities of selling for each product
over_logistic_probs = predict(over_logistic_model, test_sales, type = "response")
under_logistic_probs = predict(under_logistic_model, test_sales, type = "response")
both_logistic_probs = predict(both_logistic_model, test_sales, type = "response")
smote_logistic_probs = predict(smote_logistic_model, test_sales, type = "response")

# Binary classifications: Class 0 -> prob <= 0.5; Class 1 -> prob > 0.5
over_logistic_class = rep(0, nrow(test_sales))
over_logistic_class[over_logistic_probs > 0.5] = 1
under_logistic_class = rep(0, nrow(test_sales))
under_logistic_class[under_logistic_probs > 0.5] = 1
both_logistic_class = rep(0, nrow(test_sales))
both_logistic_class[both_logistic_probs > 0.5] = 1
smote_logistic_class = rep(0, nrow(test_sales))
smote_logistic_class[smote_logistic_probs > 0.5] = 1

# New confusion matrix's and metric's result's
confusionMatrix(over_logistic_class, test_sales$SoldFlag)
confusionMatrix(under_logistic_class, test_sales$SoldFlag)
confusionMatrix(both_logistic_class, test_sales$SoldFlag)
confusionMatrix(smote_logistic_class, test_sales$SoldFlag)

# ROC curves
roc.curve(test_sales$SoldFlag, logistic_probs, col = "orange")
roc.curve(test_sales$SoldFlag, over_logistic_probs, add = TRUE, col = "blue")
roc.curve(test_sales$SoldFlag, under_logistic_probs, add = TRUE, col = "darkgreen")
roc.curve(test_sales$SoldFlag, both_logistic_probs, add = TRUE, col = "red")
roc.curve(test_sales$SoldFlag, smote_logistic_probs, add = TRUE, col = "pink")
legend("topleft", legend = c("original", "over", "under", "both", "smote"), col = c("orange", "blue", "darkgreen", "red", "pink"), lty = 1, cex = 0.7)


#levels(droplevels(train_sales$SoldFlag)) # Undo "factor" that was done before on SoldFlag

# CONCLUSION: Using resampling technics, the specificity improved drastically.
#             But sensitivity was reduced and accuracy as well (as expected).
#             Comparing the ROC areas of the original prediction and the ones 
#             with resampling, the area is pratically equal in all of them.



# Classification:             ----  DECISION TREES MODEL  ----
tree_model <- rpart(SoldFlag ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg + ItemCount, data = train_sales, method = "class")
summary(tree_model)
print(tree_model)

# Plot the generated tree
rpart.plot(tree_model, main = "Original Dataset Tree")

# NOTE THAT NOT ALL THE CONSIDERED VARIABLES WHERE USED. 
# CHECK VARIABLE IMPORTANCE IN FUNCTION "summary(tree_model)"

# Obtain the probability of selling for each product
tree_probs = predict(tree_model, test_sales, type = "class")

# Confusion matrix and metric's results
confusionMatrix(tree_probs, test_sales$SoldFlag)

# The results show poor Specificity because of inbanlanced SoldFlag class
# Solutions: Oversampling, Undersampling, Both Oversampling and Undersampling, SMOTE
# Resample data already done before. Variables: over_train_sales, under_train_sales, both_train_sales and smote_train_sales

# Decision tree for the "balanced" data
over_tree_model <- rpart(SoldFlag ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg + ItemCount, data = over_train_sales, method = "class")
summary(over_tree_model)
print(over_tree_model)
rpart.plot(over_tree_model, main = "Oversampled Dataset Tree")

under_tree_model <- rpart(SoldFlag ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg + ItemCount, data = under_train_sales, method = "class")
summary(under_tree_model)
print(under_tree_model)
rpart.plot(under_tree_model, main = "Undersampled Dataset Tree")

both_tree_model <- rpart(SoldFlag ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg + ItemCount, data = both_train_sales, method = "class")
summary(both_tree_model)
print(both_tree_model)
rpart.plot(both_tree_model, main = "Oversampled and Undersampled Dataset Tree")

smote_tree_model <- rpart(SoldFlag ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg + ItemCount, data = smote_train_sales, method = "class")
summary(smote_tree_model)
print(smote_tree_model)
rpart.plot(smote_tree_model, main = "SMOTEd Dataset Tree")

# Print all the trees at the same time
par(mfrow=c(2,2))
rpart.plot(over_tree_model, main = "Oversampled Dataset Tree")
rpart.plot(under_tree_model, main = "Undersampled Dataset Tree")
rpart.plot(both_tree_model, main = "Oversampled and Undersampled Dataset Tree")
rpart.plot(smote_tree_model, main = "SMOTEd Dataset Tree")
par(mfrow=c(1,1))

# Obtain the new probabilities of selling for each product
over_tree_probs = predict(over_tree_model, test_sales, type = "class")
under_tree_probs = predict(under_tree_model, test_sales, type = "class")
both_tree_probs = predict(both_tree_model, test_sales, type = "class")
smote_tree_probs = predict(smote_tree_model, test_sales, type = "class")

# New confusion matrix's and metric's result's
confusionMatrix(over_tree_probs, test_sales$SoldFlag)
confusionMatrix(under_tree_probs, test_sales$SoldFlag)
confusionMatrix(both_tree_probs, test_sales$SoldFlag)
confusionMatrix(smote_tree_probs, test_sales$SoldFlag)

# ROC curves
roc.curve(test_sales$SoldFlag, tree_probs, col = "orange")
roc.curve(test_sales$SoldFlag, over_tree_probs, add.roc = TRUE, col = "blue")
roc.curve(test_sales$SoldFlag, under_tree_probs, add.roc = TRUE, col = "darkgreen")
roc.curve(test_sales$SoldFlag, both_tree_probs, add.roc = TRUE, col = "red")
roc.curve(test_sales$SoldFlag, smote_tree_probs, add.roc = TRUE, col = "pink")
legend("topleft", legend = c("original", "over", "under", "both", "smote"), col = c("orange", "blue", "darkgreen", "red", "pink"), lty = 1, cex = 0.7)


# CONCLUSION: Using resampling technics, the specificity improved drastically.
#             But sensitivity was reduced and accuracy as well (as expected).
#             Comparing the ROC area of the original prediction and the ones
#             with resampling, the resampled ones have a big increase over
#             the original but are pratically equal between them.



# Classification:             ----  RANDOM FORESTS MODEL  ----
randomF_model <- randomForest(SoldFlag ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg + ItemCount, data = train_sales, ntree=1000) # Forest with 1000 trees
print(randomF_model)
plot(randomF_model, ylim = c(0, 1), main = "Original Dataset")

# Obtain the probability of selling for each product
randomF_probs = predict(randomF_model, test_sales)

# Confusion matrix and metrics results
confusionMatrix(randomF_probs, test_sales$SoldFlag)

# The results show poor Specificity because of inbanlanced SoldFlag class
# Solutions: Oversampling, Undersampling, Both Oversampling and Undersampling, SMOTE
# Resample data already done before. Variables: over_train_sales, under_train_sales, both_train_sales and smote_train_sales

# Random forests for the "balanced" data
over_randomF_model <- randomForest(SoldFlag ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg + ItemCount, data = over_train_sales, ntree=1000)
print(over_randomF_model)
plot(over_randomF_model, ylim = c(0, 1), main = "Oversampled Dataset")

under_randomF_model <- randomForest(SoldFlag ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg + ItemCount, data = under_train_sales, ntree=1000)
print(under_randomF_model)
plot(under_randomF_model, ylim = c(0, 1), main = "Undersampled Dataset")

both_randomF_model <- randomForest(SoldFlag ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg + ItemCount, data = both_train_sales, ntree=1000)
print(both_randomF_model)
plot(both_randomF_model, ylim = c(0, 1), main = "Oversampled and Undersampled Dataset")

smote_randomF_model <- randomForest(SoldFlag ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg + ItemCount, data = smote_train_sales, ntree=1000)
print(smote_randomF_model)
plot(smote_randomF_model, ylim = c(0, 1), main = "SMOTEd Dataset")

# Print all error information at the same time
par(mfrow=c(2,2))
plot(over_randomF_model, ylim = c(0, 1), main = "Oversampled Dataset")
plot(under_randomF_model, ylim = c(0, 1), main = "Undersampled Dataset")
plot(both_randomF_model, ylim = c(0, 1), main = "Oversampled and Undersampled Dataset")
plot(smote_randomF_model, ylim = c(0, 1), main = "SMOTEd Dataset")
par(mfrow=c(1,1))

# Obtain the new probabilities of selling for each product
over_randomF_probs = predict(over_randomF_model, test_sales)
under_randomF_probs = predict(under_randomF_model, test_sales)
both_randomF_probs = predict(both_randomF_model, test_sales)
smote_randomF_probs = predict(smote_randomF_model, test_sales)

# New confusion matrix's and metric's result's
confusionMatrix(over_randomF_probs, test_sales$SoldFlag)
confusionMatrix(under_randomF_probs, test_sales$SoldFlag)
confusionMatrix(both_randomF_probs, test_sales$SoldFlag)
confusionMatrix(smote_randomF_probs, test_sales$SoldFlag)

# ROC curves
roc.curve(test_sales$SoldFlag, randomF_probs, col = "orange")
roc.curve(test_sales$SoldFlag, over_randomF_probs, add.roc = TRUE, col = "blue")
roc.curve(test_sales$SoldFlag, under_randomF_probs, add.roc = TRUE, col = "darkgreen")
roc.curve(test_sales$SoldFlag, both_randomF_probs, add.roc = TRUE, col = "red")
roc.curve(test_sales$SoldFlag, smote_randomF_probs, add.roc = TRUE, col = "pink")
legend("topleft", legend = c("original", "over", "under", "both", "smote"), col = c("orange", "blue", "darkgreen", "red", "pink"), lty = 1, cex = 0.7)


# CONCLUSION: Using resampling technics, the specificity improved drastically.
#             But sensitivity was rediced and accuracy as well (as expected).
#             Comparing the ROC area of the original prediction and the ones
#             with resampling, the resampled ones have a big increase over
#             the original but are pratically equak between them.



#               Given the results of all models, in each one was chosen the ones
# CHOOSING THE  with SMOTEd resampling technics. The SMOTEd results were the ones
# BEST MODEL:   that had the higher accuracy mantaining a balanced and aceptable 
#               level of sensitivity and specificity.

# Comparing the choosing results of all models:
roc.curve(test_sales$SoldFlag, smote_logistic_probs, col = "orange")
roc.curve(test_sales$SoldFlag, smote_tree_probs, add.roc = TRUE, col = "blue")
roc.curve(test_sales$SoldFlag, smote_randomF_probs, add.roc = TRUE, col = "darkgreen")
legend("topleft", legend = c("Logistic Regression (SMOTE)", "Decision Trees (SMOTE)", "Random Forests (SMOTE)"), col = c("orange", "blue", "darkgreen"), lty = 1, cex = 0.7)

# Logistic regression has the higher area under the curve of the three models.
# Using logistic regression to calculate the probability of selling of each of the
# active inventory products.
logistic_probs_active_inventory = predict(logistic_model, active_sales, type = "response")

# Matrix representation of the probabilities and additional SKUnumber information
results_as_matrix = cbind(as.matrix(active_sales$SKUnumber), as.matrix(logistic_probs_active_inventory))


#####################################################################################################################################


###  QUANTIDADE ESPERADA DE VENDA DE CADA PRODUTO  ###

# OPTIONS: 1) Use a classification model witch requires a transformation of the "SoldCount"
#             variable into categories to be used instead of the values.
#          2) Use a regression model to obtain predicted values of "SoldCount".

# Using option 2):
set.seed(1234)
train <- sample(1:nrow(historical_sales), size = ceiling(0.7*nrow(historical_sales)), replace = FALSE)
train_sales <- historical_sales[train,]
test_sales <- historical_sales[-train,]



# Regression:             ----  LINEAR REGRESSION MODEL  ----
linear_r_model <- lm(SoldCount ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg + ItemCount, data = train_sales)
summary(linear_r_model)
par(mfrow = c(2, 2))
plot(linear_r_model, col = "darkorange")
par(mfrow = c(1, 1))

# Check for non-linearity by analysing the fit of the errors arround the linear model for each variable (Red line is the linear model and green line is the fit of the errors. The closer they are the better)
# All variables are good exceplt for "ItemCount" that as some non-linearity arround the linear model.
crPlots(linear_r_model)

# Test the model for each product in test_sales
linear_r_values <- predict(linear_r_model, test_sales)
linear_r_values <- round(linear_r_values)
linear_r_values[linear_r_values < 0] <- 0

# Actual vs Predicted for train and test datasets
par(mfrow = c(1,2))
plot(round(predict(linear_r_model)), train_sales$SoldCount, xlab = "Predicted", ylab = "Actual", ylim = c(0, 40), xlim = c(0, 40), col = c("darkorange"), main = "Actual vs Predicted on Train_DataSet")
abline(a=0, b=1, col = "darkblue")
plot(linear_r_values, test_sales$SoldCount, xlab = "Predicted", ylab = "Actual", ylim = c(0, 40), xlim = c(0, 40), col = c("darkorange"), main = "Actual vs Predicted on Test_DataSet")
abline(a=0, b=1, col = "darkblue")
par(mfrow = c(1,1))

# Check some regression metrics (errors, correlation value, r-squared value)
mmetric(test_sales$SoldCount, linear_r_values, c("MAE", "RMSE", "COR", "R2"))

# R-squared is extremely low. Try to improve the model:
# Eliminate extreme values (outliers) that can have segnificante effect on the construction of the model.
# Using cook's distance measure to analise the distance of the measures from the model.
cutoff <- 4/((nrow(train_sales) - length(linear_r_model$coefficients) - 2))
plot(linear_r_model, which = 4, cook.levels = cutoff)
plot(linear_r_model, which = 5, cook.levels = cutoff)

# Eliminate the extremes from the training data
train_sales <- train_sales[-which(rownames(train_sales) %in% c("7457", "7883", "42091", "5728", "75333")), ]

# Refit the model
linear_r_model <- lm(SoldCount ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg + ItemCount, data = train_sales)
summary(linear_r_model)
par(mfrow = c(2, 2))
plot(linear_r_model, col = "darkorange")
par(mfrow = c(1, 1))

# Check for new extreme values
cutoff <- 4/((nrow(train_sales) - length(linear_r_model$coefficients) - 2))
plot(linear_r_model, which = 4, cook.levels = cutoff)
plot(linear_r_model, which = 5, cook.levels = cutoff)

# Eliminate the new extremes from the training data
train_sales <- train_sales[-which(rownames(train_sales) %in% c("22052", "28559", "61300", "28559", "6640", "74026")), ]

# Refit the model
linear_r_model <- lm(SoldCount ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg + ItemCount, data = train_sales)
summary(linear_r_model)
par(mfrow = c(2, 2))
plot(linear_r_model, col = "darkorange")
par(mfrow = c(1, 1))

# Check for new extreme values
cutoff <- 4/((nrow(train_sales) - length(linear_r_model$coefficients) - 2))
plot(linear_r_model, which = 4, cook.levels = cutoff)
plot(linear_r_model, which = 5, cook.levels = cutoff)

# Eliminate the new extremes from the training data
train_sales <- train_sales[-which(rownames(train_sales) %in% c("25524", "42090", "38829", "26044", "29121", "24355")), ]

# Refit the model
linear_r_model <- lm(SoldCount ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg + ItemCount, data = train_sales)
summary(linear_r_model)
par(mfrow = c(2, 2))
plot(linear_r_model, col = "darkorange")
par(mfrow = c(1, 1))

# Check for new extreme values
cutoff <- 4/((nrow(train_sales) - length(linear_r_model$coefficients) - 2))
plot(linear_r_model, which = 4, cook.levels = cutoff)
plot(linear_r_model, which = 5, cook.levels = cutoff)

# Eliminate the new extremes from the training data
train_sales <- train_sales[-which(rownames(train_sales) %in% c("30829", "24548", "25184", "33204", "60111", "73630")), ]

# Refit the model
linear_r_model <- lm(SoldCount ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg + ItemCount, data = train_sales)
summary(linear_r_model)
par(mfrow = c(2, 2))
plot(linear_r_model, col = "darkorange")
par(mfrow = c(1, 1))

# Check for new extreme values
cutoff <- 4/((nrow(train_sales) - length(linear_r_model$coefficients) - 2))
plot(linear_r_model, which = 4, cook.levels = cutoff)
plot(linear_r_model, which = 5, cook.levels = cutoff)

# Eliminate the new extremes from the training data
train_sales <- train_sales[-which(rownames(train_sales) %in% c("22707", "17530", "32379", "31560", "32097", "35807")), ]

# Refit the model
linear_r_model <- lm(SoldCount ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg + ItemCount, data = train_sales)
summary(linear_r_model) # NewReleaseFlag pode ser excluido do modelo devido ao valor de t ser superior a 0.05
par(mfrow = c(2, 2))
plot(linear_r_model, col = "darkorange")
par(mfrow = c(1, 1))

# Check for new extreme values
cutoff <- 4/((nrow(train_sales) - length(linear_r_model$coefficients) - 2))
plot(linear_r_model, which = 4, cook.levels = cutoff)
plot(linear_r_model, which = 5, cook.levels = cutoff)

# Eliminate the new extremes from the training data
train_sales <- train_sales[-which(rownames(train_sales) %in% c("18182", "42324", "13621", "14800", "32815", "29890")), ]

# Refit the model.
# NOTE: NewReleaseFlag was excluded from the model due to his t value > 0.05
linear_r_model <- lm(SoldCount ~ MarketingType + StrengthFactor + ReleaseYear + PriceReg + ItemCount, data = train_sales)
summary(linear_r_model)
par(mfrow = c(2, 2))
plot(linear_r_model, col = "darkorange")
par(mfrow = c(1, 1))

# Test the final model for each product in test_sales
linear_r_values_final <- predict(linear_r_model, test_sales)
linear_r_values_final <- round(linear_r_values_final)
linear_r_values_final[linear_r_values_final < 0] <- 0

# Actual vs Predicted for train and test datasets of the final model
par(mfrow = c(1,2))
plot(round(predict(linear_r_model)), train_sales$SoldCount, xlab = "Predicted", ylab = "Actual", ylim = c(0, 40), xlim = c(0, 40), col = c("darkorange"), main = "Actual vs Predicted on Train_DataSet")
abline(a=0, b=1, col = "darkblue")
plot(linear_r_values_final, test_sales$SoldCount, xlab = "Predicted", ylab = "Actual", ylim = c(0, 40), xlim = c(0, 40), col = c("darkorange"), main = "Actual vs Predicted on Test_DataSet")
abline(a=0, b=1, col = "darkblue")
par(mfrow = c(1,1))

# Compare some regression metrics from the original and final models (errors, correlation value, r-squared value)
mmetric(test_sales$SoldCount, linear_r_values, c("MAE", "RMSE", "COR", "R2")) # Valores do modelo inicial
mmetric(test_sales$SoldCount, linear_r_values_final, c("MAE", "RMSE", "COR", "R2")) # Valores do modelo final

# MAE error decreased only 1 in the final model
# RMSE error is the same in both models
#
# CONCLUSION: Although we made a new final model in order to improve the results, the final
#             model is pratically equal to the original model.



# Regression:             ----  DECISION TREES MODEL  ----
# Reset the values of train data previous altered
set.seed(1234)
train_sales <- historical_sales[train,]

# Build the model
tree_r_model <- rpart(SoldCount ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg + ItemCount, data = train_sales, method = "anova")
summary(tree_r_model)
print(tree_r_model)

# Plot the generated tree
rpart.plot(tree_r_model, main = "Original Regression Tree")

# NOTE THAT NOT ALL THE CONSIDERED VARIABLES WHERE USED. 
# CHECK VARIABLE IMPORTANCE IN FUNCTION "summary(tree_model)"

# Test the model for each product in test_sales
tree_r_values <- predict(tree_r_model, test_sales)
tree_r_values <- round(tree_r_values)

# Actual vs Predicted for train and test datasets
par(mfrow = c(1,2))
plot(round(predict(tree_r_model)), train_sales$SoldCount, xlab = "Predicted", ylab = "Actual", ylim = c(0, 40), xlim = c(0, 40), col = c("darkorange"), main = "Actual vs Predicted on Train_DataSet")
abline(a=0, b=1, col = "darkblue")
plot(tree_r_values, test_sales$SoldCount, xlab = "Predicted", ylab = "Actual", ylim = c(0, 40), xlim = c(0, 40), col = c("darkorange"), main = "Actual vs Predicted on Test_DataSet")
abline(a=0, b=1, col = "darkblue")
par(mfrow = c(1,1))

# Check some regression metrics (errors, correlation value, r-squared value)
mmetric(test_sales$SoldCount, tree_r_values, c("MAE", "RMSE", "COR", "R2"))



# Refit the model with tuned parameters 
tree_r_model_tuned <- rpart(SoldCount ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg + ItemCount, data = train_sales, method = "anova", control = rpart.control(minsplit = 2, minbucket = 1, cp = 0.004))
summary(tree_r_model_tuned)
print(tree_r_model_tuned)

# Plot the generated tree
rpart.plot(tree_r_model_tuned, main = "Tuned Parameters Regression Tree")

# Test the model for each product in test_sales
tree_r_values_tuned <- predict(tree_r_model_tuned, test_sales)
tree_r_values_tuned <- round(tree_r_values_tuned)

# Actual vs Predicted for train and test datasets
par(mfrow = c(1,2))
plot(round(predict(tree_r_model_tuned)), train_sales$SoldCount, xlab = "Predicted", ylab = "Actual", ylim = c(0, 40), xlim = c(0, 40), col = c("darkorange"), main = "Actual vs Predicted on Train_DataSet")
abline(a=0, b=1, col = "darkblue")
plot(tree_r_values_tuned, test_sales$SoldCount, xlab = "Predicted", ylab = "Actual", ylim = c(0, 40), xlim = c(0, 40), col = c("darkorange"), main = "Actual vs Predicted on Test_DataSet")
abline(a=0, b=1, col = "darkblue")
par(mfrow = c(1,1))

# Check some regression metrics (errors, correlation value, r-squared value)
mmetric(test_sales$SoldCount, tree_r_values_tuned, c("MAE", "RMSE", "COR", "R2"))



# Prune the tree to minimise errors
plotcp(tree_r_model_tuned)

cp = min(tree_r_model_tuned$cptable[6, ])
tree_r_model_tuned_pruned <- prune(tree_r_model_tuned, cp = cp)
summary(tree_r_model_tuned_pruned)
print(tree_r_model_tuned_pruned)

# Plot the pruned tree
rpart.plot(tree_r_model_tuned_pruned, main = "Tuned Parameters Regression Tree")

# Test the model for each product in test_sales
tree_r_values_tuned_pruned <- predict(tree_r_model_tuned_pruned, test_sales)
tree_r_values_tuned_pruned <- round(tree_r_values_tuned_pruned)

# Actual vs Predicted for train and test datasets
par(mfrow = c(1,2))
plot(round(predict(tree_r_model_tuned_pruned)), train_sales$SoldCount, xlab = "Predicted", ylab = "Actual", ylim = c(0, 40), xlim = c(0, 40), col = c("darkorange"), main = "Actual vs Predicted on Train_DataSet")
abline(a=0, b=1, col = "darkblue")
plot(tree_r_values_tuned_pruned, test_sales$SoldCount, xlab = "Predicted", ylab = "Actual", ylim = c(0, 40), xlim = c(0, 40), col = c("darkorange"), main = "Actual vs Predicted on Test_DataSet")
abline(a=0, b=1, col = "darkblue")
par(mfrow = c(1,1))

# Check some regression metrics (errors, correlation value, r-squared value)
mmetric(test_sales$SoldCount, tree_r_values, c("MAE", "RMSE", "COR", "R2")) # Inicial tree model
mmetric(test_sales$SoldCount, tree_r_values_tuned_pruned, c("MAE", "RMSE", "COR", "R2")) # Final tree model

# MAE error increased 6 in the final tree
# RMSE error decreased 0.5 in the final tree
# CORRELATION and R-SQUARED increased slightly but given the nature of the data it might be significant
#
# CONCLUSION: Although we made a final tree in order to improve the results, the errors
#             increased slightly for MAE and remained pratically equal for RMSE. But,
#             correlation and r-squared increased slightly but maybe significant given the
#             distribution of the values of the target variable.
#             The last model can also predict more possible values than the original.



# Regression:             ----  RANDOM FORESTS MODEL  ----
# Random forests models adapt to classification and regression accordingly to the type of
# the values in the target class.
set.seed(1234)
randomF_r_model <- randomForest(SoldCount ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg + ItemCount, data = train_sales)
print(randomF_r_model)

# Plot errors
plot(randomF_r_model, main = "Errors Regression Random Forest")

# Plot variable importance
varImpPlot(randomF_r_model, sort = T, main = "Variable Importance")

# Test the model for each product in test_sales
randomF_r_values <- predict(randomF_r_model, test_sales)
randomF_r_values <- round(randomF_r_values)

# Actual vs Predicted for train and test datasets
par(mfrow = c(1,2))
plot(round(predict(randomF_r_model)), train_sales$SoldCount, xlab = "Predicted", ylab = "Actual", ylim = c(0, 40), xlim = c(0, 40), col = c("darkorange"), main = "Actual vs Predicted on Train_DataSet")
abline(a=0, b=1, col = "darkblue")
plot(randomF_r_values, test_sales$SoldCount, xlab = "Predicted", ylab = "Actual", ylim = c(0, 40), xlim = c(0, 40), col = c("darkorange"), main = "Actual vs Predicted on Test_DataSet")
abline(a=0, b=1, col = "darkblue")
par(mfrow = c(1,1))

# Check some regression metrics (errors, correlation value, r-squared value)
mmetric(test_sales$SoldCount, randomF_r_values, c("MAE", "RMSE", "COR", "R2"))



# CHOOSING THE  Given the results of all models, in each one was chosen the best
# BEST MODEL:   one obtained. Comparing now the best results of the three models:

# Comparing the best results of the three models:
mmetric(test_sales$SoldCount, linear_r_values_final, c("MAE", "RMSE", "COR", "R2")) # Linear regression model results
mmetric(test_sales$SoldCount, tree_r_values_tuned_pruned, c("MAE", "RMSE", "COR", "R2")) # Decision trees model results
mmetric(test_sales$SoldCount, randomF_r_values, c("MAE", "RMSE", "COR", "R2")) # Random forest model results
a <- as.matrix(mmetric(test_sales$SoldCount, linear_r_values_final, c("MAE", "RMSE", "COR", "R2")))
b <- as.matrix(mmetric(test_sales$SoldCount, tree_r_values_tuned_pruned, c("MAE", "RMSE", "COR", "R2")))
c <- as.matrix(mmetric(test_sales$SoldCount, randomF_r_values, c("MAE", "RMSE", "COR", "R2")))
d <- cbind(a, b, c)
colnames(d) <- c("Linear Regression", "Decision Trees", "Random Forest")

# Random Forest is slightly the best model.
# Using random forest to predict the selling amount of each of the active inventory products.
randomF_r_values_active_inventory = predict(randomF_r_model, active_sales)
randomF_r_values_active_inventory <- round(randomF_r_values_active_inventory)

# Matrix representation of the values and additional SKUnumber information
selling_values_as_matrix = cbind(as.matrix(active_sales$SKUnumber), as.matrix(randomF_r_values_active_inventory))



#####################################################################################################################################

###  QUANTIDADE APROPRIADA DE INVENTÁRIO DE CADA PRODUTO  ###

# Use regression to determine the quantities.
set.seed(1234)
train <- sample(1:nrow(historical_sales), size = ceiling(0.7*nrow(historical_sales)), replace = FALSE)
train_sales <- historical_sales[train,]
test_sales <- historical_sales[-train,]

# Some data visualization first:
par(mfrow = c(1, 2))
barplot(table(train_sales$SoldCount), col = "darkorange", border = "darkorange", main = "SoldCount Destribution")
barplot(table(train_sales$ItemCount), col = "darkorange", border = "darkorange", main = "ItemCount Destribution")
par(mfrow = c(1, 1))
summary(train_sales$ItemCount)

# Reducing the "ItemCount" data: using up to the maximum value of "SoldCount" plus the median of
# "ItemCount" to have a safety level of training. This way we can remove outliers and "ItemCount"
# data that is too big that can influence the predicted results.

# Function to resample training data to the maximum value of "SoldCount" plus median of "ItemCount"
itemCountDataPartition <- function(train_data) {
  # Get the maximum of "SoldCount"
  soldCountMaximum = max(train_data$SoldCount)
  
  # Get the median of "ItemCount"
  itemCountMedian = median(train_data$ItemCount)
  
  train_data <- subset(train_data, ItemCount <= (soldCountMaximum + itemCountMedian))
}

# Call function to resample the data
train_sales <- itemCountDataPartition(train_sales)

# Visualize the resampled data
barplot(table(train_sales$ItemCount), col = "darkorange", border = "darkorange", main = "ItemCount Destribution Resampled")



# Regression:             ----  LINEAR REGRESSION MODEL  ----
linear_r_model_inventario <- lm(ItemCount ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg, data = train_sales)
summary(linear_r_model_inventario)
par(mfrow = c(2, 2))
plot(linear_r_model_inventario, col = "darkorange")
par(mfrow = c(1, 1))

# Check for non-linearity by analysing the fit of the errors arround the linear model for each variable (Red line is the linear model and green line is the fit of the errors. The closer they are the better)
# StrengthFactor, ReleaseYear and PriceReg have non-linearity arround the linear model.
crPlots(linear_r_model_inventario)

# Test the model for each product in test_sales
linear_r_values_inventario <- predict(linear_r_model_inventario, test_sales)
linear_r_values_inventario <- round(linear_r_values_inventario)
linear_r_values_inventario[linear_r_values_inventario < 0] <- 0

# Actual vs Predicted for train and test datasets
par(mfrow = c(1,2))
plot(round(predict(linear_r_model_inventario)), train_sales$ItemCount, xlab = "Predicted", ylab = "Actual", ylim = c(0, 100), xlim = c(0, 100), col = c("darkorange"), main = "Actual vs Predicted on Train_DataSet")
abline(a=0, b=1, col = "darkblue")
plot(linear_r_values_inventario, test_sales$ItemCount, xlab = "Predicted", ylab = "Actual", ylim = c(0, 1500), xlim = c(0, 1500), col = c("darkorange"), main = "Actual vs Predicted on Test_DataSet")
abline(a=0, b=1, col = "darkblue")
par(mfrow = c(1,1))

# Check some regression metrics (errors, correlation value, r-squared value)
mmetric(test_sales$ItemCount, linear_r_values_inventario, c("MAE", "RMSE", "COR", "R2"))

# Try to improve the model:
# Eliminate extreme values (outliers) that can have segnificante effect on the construction of the model.
# Using cook's distance measure to analise the distance of the measures from the model.
cutoff <- 4/((nrow(train_sales) - length(linear_r_model_inventario$coefficients) - 2))
plot(linear_r_model_inventario, which = 4, cook.levels = cutoff)
plot(linear_r_model_inventario, which = 5, cook.levels = cutoff)

# Eliminate the extremes from the training data
train_sales <- train_sales[-which(rownames(train_sales) %in% c("60280", "37152", "15512", "36899")), ]

# Refit the model
linear_r_model_inventario <- lm(ItemCount ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg, data = train_sales)
summary(linear_r_model_inventario)
par(mfrow = c(2, 2))
plot(linear_r_model_inventario, col = "darkorange")
par(mfrow = c(1, 1))

# Check for new extremes
cutoff <- 4/((nrow(train_sales) - length(linear_r_model_inventario$coefficients) - 2))
plot(linear_r_model_inventario, which = 4, cook.levels = cutoff)
plot(linear_r_model_inventario, which = 5, cook.levels = cutoff)

# Eliminate the extremes from the training data
train_sales <- train_sales[-which(rownames(train_sales) %in% c("70700", "55142", "65478", "2219", "27427", "36586")), ]

# Refit the model
linear_r_model_inventario <- lm(ItemCount ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg, data = train_sales)
summary(linear_r_model_inventario)
par(mfrow = c(2, 2))
plot(linear_r_model_inventario, col = "darkorange")
par(mfrow = c(1, 1))

# Check for new extremes
cutoff <- 4/((nrow(train_sales) - length(linear_r_model_inventario$coefficients) - 2))
plot(linear_r_model_inventario, which = 4, cook.levels = cutoff)
plot(linear_r_model_inventario, which = 5, cook.levels = cutoff)

# Eliminate the extremes from the training data
train_sales <- train_sales[-which(rownames(train_sales) %in% c("35478", "55146", "73825", "47808", "52617", "29437")), ]

# Refit the model
linear_r_model_inventario <- lm(ItemCount ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg, data = train_sales)
summary(linear_r_model_inventario)
par(mfrow = c(2, 2))
plot(linear_r_model_inventario, col = "darkorange")
par(mfrow = c(1, 1))

# Check for new extremes
cutoff <- 4/((nrow(train_sales) - length(linear_r_model_inventario$coefficients) - 2))
plot(linear_r_model_inventario, which = 4, cook.levels = cutoff)
plot(linear_r_model_inventario, which = 5, cook.levels = cutoff)

# Eliminate the extremes from the training data
train_sales <- train_sales[-which(rownames(train_sales) %in% c("56805", "65922", "2973", "8430", "21949")), ]

# Refit the model
linear_r_model_inventario <- lm(ItemCount ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg, data = train_sales)
summary(linear_r_model_inventario)
par(mfrow = c(2, 2))
plot(linear_r_model_inventario, col = "darkorange")
par(mfrow = c(1, 1))

# Check for new extremes
cutoff <- 4/((nrow(train_sales) - length(linear_r_model_inventario$coefficients) - 2))
plot(linear_r_model_inventario, which = 4, cook.levels = cutoff)
plot(linear_r_model_inventario, which = 5, cook.levels = cutoff)

# Eliminate the extremes from the training data
train_sales <- train_sales[-which(rownames(train_sales) %in% c("29355", "65578", "18174", "52557", "65003")), ]

# Refit the model
linear_r_model_inventario <- lm(ItemCount ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg, data = train_sales)
summary(linear_r_model_inventario)
par(mfrow = c(2, 2))
plot(linear_r_model_inventario, col = "darkorange")
par(mfrow = c(1, 1))

# Check for new extremes
cutoff <- 4/((nrow(train_sales) - length(linear_r_model_inventario$coefficients) - 2))
plot(linear_r_model_inventario, which = 4, cook.levels = cutoff)
plot(linear_r_model_inventario, which = 5, cook.levels = cutoff)

# Eliminate the extremes from the training data
train_sales <- train_sales[-which(rownames(train_sales) %in% c("86027", "65572", "51492", "5595", "44324")), ]

# Refit the model
linear_r_model_inventario <- lm(ItemCount ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg, data = train_sales)
summary(linear_r_model_inventario)
par(mfrow = c(2, 2))
plot(linear_r_model_inventario, col = "darkorange")
par(mfrow = c(1, 1))

# Check for new extremes
cutoff <- 4/((nrow(train_sales) - length(linear_r_model_inventario$coefficients) - 2))
plot(linear_r_model_inventario, which = 4, cook.levels = cutoff)
plot(linear_r_model_inventario, which = 5, cook.levels = cutoff)

# Eliminate the extremes from the training data
train_sales <- train_sales[-which(rownames(train_sales) %in% c("65972", "36027", "29639", "2841", "30122")), ]

# Refit the model
linear_r_model_inventario <- lm(ItemCount ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg, data = train_sales)
summary(linear_r_model_inventario)
par(mfrow = c(2, 2))
plot(linear_r_model_inventario, col = "darkorange")
par(mfrow = c(1, 1))

# Check for new extremes
cutoff <- 4/((nrow(train_sales) - length(linear_r_model_inventario$coefficients) - 2))
plot(linear_r_model_inventario, which = 4, cook.levels = cutoff)
plot(linear_r_model_inventario, which = 5, cook.levels = cutoff)

# Eliminate the extremes from the training data
train_sales <- train_sales[-which(rownames(train_sales) %in% c("49628", "1381", "68607")), ]

# Refit the model
linear_r_model_inventario <- lm(ItemCount ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg, data = train_sales)
summary(linear_r_model_inventario)
par(mfrow = c(2, 2))
plot(linear_r_model_inventario, col = "darkorange")
par(mfrow = c(1, 1))

# Check for new extremes
cutoff <- 4/((nrow(train_sales) - length(linear_r_model_inventario$coefficients) - 2))
plot(linear_r_model_inventario, which = 4, cook.levels = cutoff)
plot(linear_r_model_inventario, which = 5, cook.levels = cutoff)

# Eliminate the extremes from the training data
train_sales <- train_sales[-which(rownames(train_sales) %in% c("67437", "50733", "39565", "75516", "56588")), ]

# Refit the model
linear_r_model_inventario <- lm(ItemCount ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg, data = train_sales)
summary(linear_r_model_inventario)
par(mfrow = c(2, 2))
plot(linear_r_model_inventario, col = "darkorange")
par(mfrow = c(1, 1))

# Check for new extremes
cutoff <- 4/((nrow(train_sales) - length(linear_r_model_inventario$coefficients) - 2))
plot(linear_r_model_inventario, which = 4, cook.levels = cutoff)
plot(linear_r_model_inventario, which = 5, cook.levels = cutoff)

# Eliminate the extremes from the training data
train_sales <- train_sales[-which(rownames(train_sales) %in% c("65498", "42849", "75933", "6028", "59538")), ]

# Refit the model
linear_r_model_inventario <- lm(ItemCount ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg, data = train_sales)
summary(linear_r_model_inventario)
par(mfrow = c(2, 2))
plot(linear_r_model_inventario, col = "darkorange")
par(mfrow = c(1, 1))

# Check for new extremes
cutoff <- 4/((nrow(train_sales) - length(linear_r_model_inventario$coefficients) - 2))
plot(linear_r_model_inventario, which = 4, cook.levels = cutoff)
plot(linear_r_model_inventario, which = 5, cook.levels = cutoff)

# Eliminate the extremes from the training data
train_sales <- train_sales[-which(rownames(train_sales) %in% c("68422", "53752", "3006", "33399", "39454")), ]

# Refit the model
linear_r_model_inventario <- lm(ItemCount ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg, data = train_sales)
summary(linear_r_model_inventario)
par(mfrow = c(2, 2))
plot(linear_r_model_inventario, col = "darkorange")
par(mfrow = c(1, 1))

# Check for new extremes
cutoff <- 4/((nrow(train_sales) - length(linear_r_model_inventario$coefficients) - 2))
plot(linear_r_model_inventario, which = 4, cook.levels = cutoff)
plot(linear_r_model_inventario, which = 5, cook.levels = cutoff)

# Eliminate the extremes from the training data
train_sales <- train_sales[-which(rownames(train_sales) %in% c("73768", "53177", "34102", "3372", "61802")), ]

# Refit the model
linear_r_model_inventario <- lm(ItemCount ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg, data = train_sales)
summary(linear_r_model_inventario)
par(mfrow = c(2, 2))
plot(linear_r_model_inventario, col = "darkorange")
par(mfrow = c(1, 1))

# Check for new extremes
cutoff <- 4/((nrow(train_sales) - length(linear_r_model_inventario$coefficients) - 2))
plot(linear_r_model_inventario, which = 4, cook.levels = cutoff)
plot(linear_r_model_inventario, which = 5, cook.levels = cutoff)

# Eliminate the extremes from the training data
train_sales <- train_sales[-which(rownames(train_sales) %in% c("57741", "2775", "27315", "40953")), ]

# Refit the model
linear_r_model_inventario <- lm(ItemCount ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg, data = train_sales)
summary(linear_r_model_inventario)
par(mfrow = c(2, 2))
plot(linear_r_model_inventario, col = "darkorange")
par(mfrow = c(1, 1))

# Check for new extremes
cutoff <- 4/((nrow(train_sales) - length(linear_r_model_inventario$coefficients) - 2))
plot(linear_r_model_inventario, which = 4, cook.levels = cutoff)
plot(linear_r_model_inventario, which = 5, cook.levels = cutoff)

# Eliminate the extremes from the training data
train_sales <- train_sales[-which(rownames(train_sales) %in% c("71214", "73595", "2878", "32547")), ]

# Refit the model
linear_r_model_inventario <- lm(ItemCount ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg, data = train_sales)
summary(linear_r_model_inventario)
par(mfrow = c(2, 2))
plot(linear_r_model_inventario, col = "darkorange")
par(mfrow = c(1, 1))

# Check for new extremes
cutoff <- 4/((nrow(train_sales) - length(linear_r_model_inventario$coefficients) - 2))
plot(linear_r_model_inventario, which = 4, cook.levels = cutoff)
plot(linear_r_model_inventario, which = 5, cook.levels = cutoff)

# Eliminate the extremes from the training data
train_sales <- train_sales[-which(rownames(train_sales) %in% c("45103", "46127", "25509", "1469")), ]

# Refit the model. FINAL MODEL !!!
linear_r_model_inventario <- lm(ItemCount ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg, data = train_sales)
summary(linear_r_model_inventario)
par(mfrow = c(2, 2))
plot(linear_r_model_inventario, col = "darkorange")
par(mfrow = c(1, 1))

# Test the final model for each product in test_sales
linear_r_values_inventario_final <- predict(linear_r_model_inventario, test_sales)
linear_r_values_inventario_final <- round(linear_r_values_inventario_final)
linear_r_values_inventario_final[linear_r_values_inventario_final < 0] <- 0

# Plot the predicted values to observe their range
barplot(table(linear_r_values_inventario_final), col = "darkorange", border = "darkorange", main = "Predicted Values")

# Visual representation of the original and predicted stock's amount as well as sold quantities
plot(test_sales$ItemCount, col = "darkblue", xaxt = 'n', xlab = "", ylab = "", main = "Stock Original vs Stock Previsto vs Quantidade Vendida para os dados de teste")
points(linear_r_values_inventario_final, col = "darkgreen")
points(test_sales$SoldCount, col = "darkorange")
legend("topleft", legend = c("Stock Original", "Stock Previsto", "Quantidade Vendida"), col = c("darkblue", "darkgreen", "darkorange"), lty = 1, cex = 0.7)

# Compare some regression metrics from the original and final models (errors, correlation value, r-squared value)
mmetric(test_sales$ItemCount, linear_r_values_inventario, c("MAE", "RMSE", "COR", "R2")) # Valores do modelo inicial
mmetric(test_sales$ItemCount, linear_r_values_inventario_final, c("MAE", "RMSE", "COR", "R2")) # Valores do modelo final

# CONCLUSION: Although we made a new final model in order to improve the results, the final
#             model is pratically equal to the original model.
#             Note that this metrics are important only if we assume that the resampled training
#             dataset (or the original dataset) is a good training base!!!!!!! Otherwise, the
#             only thing that matters is that the predicted values can't be too high.



# Regression:             ----  DECISION TREES MODEL  ----
# Reset the values of train data previous altered
set.seed(1234)
train_sales <- historical_sales[train,]

# Call function to resample the data
train_sales <- itemCountDataPartition(train_sales)

# Build the model
tree_r_model_inventario <- rpart(ItemCount ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg, data = train_sales, method = "anova")
summary(tree_r_model_inventario)
print(tree_r_model_inventario)

# Plot the generated tree
rpart.plot(tree_r_model_inventario, main = "Original Regression Tree")

# Test the model for each product in test_sales
tree_r_values_inventario <- predict(tree_r_model_inventario, test_sales)
tree_r_values_inventario <- round(tree_r_values_inventario)

# Visual representation of the original and predicted stock's amount as well as sold quantities
plot(test_sales$ItemCount, col = "darkblue", xaxt = 'n', xlab = "", ylab = "", main = "Stock Original vs Stock Previsto vs Quantidade Vendida para os dados de teste")
points(tree_r_values_inventario, col = "darkgreen")
points(test_sales$SoldCount, col = "darkorange")
legend("topleft", legend = c("Stock Original", "Stock Previsto", "Quantidade Vendida"), col = c("darkblue", "darkgreen", "darkorange"), lty = 1, cex = 0.7)

# Check some regression metrics (errors, correlation value, r-squared value)
mmetric(test_sales$ItemCount, tree_r_values_inventario, c("MAE", "RMSE", "COR", "R2"))



# Regression:             ----  RANDOM FORESTS MODEL  ----
# Random forests models adapt to classification and regression accordingly to the type of
# the values in the target class.
set.seed(1234)
randomF_r_model_inventario <- randomForest(ItemCount ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg, data = train_sales)
print(randomF_r_model_inventario)

# Plot errors
plot(randomF_r_model_inventario, main = "Errors Regression Random Forest")

# Refit the model to minimize errors with 17 trees
randomF_r_model_inventario <- randomForest(ItemCount ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + PriceReg, data = train_sales, ntree = 17)
print(randomF_r_model_inventario)

# Plot errors
plot(randomF_r_model_inventario, main = "Errors Regression Random Forest")

# Plot variable importance
varImpPlot(randomF_r_model_inventario, sort = T, main = "Variable Importance")

# Test the model for each product in test_sales
randomF_r_values_inventario <- predict(randomF_r_model_inventario, test_sales)
randomF_r_values_inventario <- round(randomF_r_values_inventario)

# Plot the predicted values to observe their range
barplot(table(randomF_r_values_inventario), col = "darkorange", border = "darkorange", main = "Predicted Values", ylab = "Frequency", xlab = "Predicted Values")

# Visual representation of the original and predicted stock's amount as well as sold quantities
plot(test_sales$ItemCount, col = "darkblue", xaxt = 'n', xlab = "", ylab = "", main = "Stock Original vs Stock Previsto vs Quantidade Vendida para os dados de teste")
points(randomF_r_values_inventario, col = "darkgreen")
points(test_sales$SoldCount, col = "darkorange")
legend("topleft", legend = c("Stock Original", "Stock Previsto", "Quantidade Vendida"), col = c("darkblue", "darkgreen", "darkorange"), lty = 1, cex = 0.7)

# Check some regression metrics (errors, correlation value, r-squared value)
mmetric(test_sales$ItemCount, randomF_r_values_inventario, c("MAE", "RMSE", "COR", "R2"))

# CHOOSING THE  Given the results of all models, in each one was chosen the best
# BEST MODEL:   one obtained. Comparing now the best results of the three models:

# Comparing the best results of the three models:
mmetric(test_sales$ItemCount, linear_r_values_inventario_final, c("MAE", "RMSE", "COR", "R2"))
mmetric(test_sales$ItemCount, tree_r_values_inventario, c("MAE", "RMSE", "COR", "R2"))
mmetric(test_sales$ItemCount, randomF_r_values_inventario, c("MAE", "RMSE", "COR", "R2"))
a_inventario <- as.matrix(mmetric(test_sales$ItemCount, linear_r_values_inventario_final, c("MAE", "RMSE", "COR", "R2")))
b_inventario <- as.matrix(mmetric(test_sales$ItemCount, tree_r_values_inventario, c("MAE", "RMSE", "COR", "R2")))
c_inventario <- as.matrix(mmetric(test_sales$ItemCount, randomF_r_values_inventario, c("MAE", "RMSE", "COR", "R2")))
d_inventario <- cbind(a_inventario, b_inventario, c_inventario)
colnames(d_inventario) <- c("Linear Regression", "Decision Trees", "Random Forest")

# Based only on these metrics, the Random Forest model is slightly the best.
# Comparing now the predicted range of the three models:
par(mfrow = c(1, 3))
barplot(table(linear_r_values_inventario_final), col = "darkorange", border = "darkorange", main = "Linear Regression Predicted Values", ylab = "Frequency", xlab = "Predicted Values")
barplot(table(tree_r_values_inventario), col = "darkblue", border = "darkblue", main = "Decision Trees Predicted Values", ylab = "Frequency", xlab = "Predicted Values")
barplot(table(randomF_r_values_inventario), col = "darkgreen", border = "darkgreen", main = "Random Forest Predicted Values", ylab = "Frequency", xlab = "Predicted Values")
par(mfrow = c(1, 1))

# Results based on decision trees are very limited.
# Comparing the results based on linear regression and random forest, the linear regression model is
# the one that can obtain the lowest predictions that is what we whant. Sinse the error values between
# these two models are pratically the same, linear regression model is choosen as the best model.

# Using linear regression model to predict for the active inventory products:
linear_r_values_inventario_ativo = predict(linear_r_model_inventario, active_sales)
linear_r_values_inventario_ativo <- round(linear_r_values_inventario_ativo)
linear_r_values_inventario_ativo[linear_r_values_inventario_ativo < 0] <- 0

# Matrix representation of the values and additional SKUnumber information
itemCount_values_as_matrix = cbind(as.matrix(active_sales$SKUnumber), as.matrix(linear_r_values_inventario_ativo))



#####################################################################################################################################

###  VALOR MONETÁRIO ESPERADO OBTER NA VENDA DE NOVOS PRODUTOS  ###

# Valor monetário esperado = Preço * Quantidade Vendida
# Necessário então efetuar uma previsão de:
#       - Quantidade esperada de venda (já efetuada na questão 3)
#       - Preço de venda

# Utilizar regressão para prever o preço de venda.
# Variável a utilizar: PriceReg de acordo com o estudo das variáveis de preços efetuado anteriormente

set.seed(1234)
train <- sample(1:nrow(historical_sales), size = ceiling(0.7*nrow(historical_sales)), replace = FALSE)
train_sales <- historical_sales[train,]
test_sales <- historical_sales[-train,]



# Regression:             ----  LINEAR REGRESSION MODEL  ----
linear_r_model_price <- lm(PriceReg ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + ItemCount, data = train_sales)
summary(linear_r_model_price)
par(mfrow = c(2, 2))
plot(linear_r_model_price, col = "darkorange")
par(mfrow = c(1, 1))

# Check for non-linearity by analysing the fit of the errors arround the linear model for each variable (Red line is the linear model and green line is the fit of the errors. The closer they are the better)
# There is no non-linearity in all variables
crPlots(linear_r_model_price)

# Test the model for each product in test_sales
linear_r_values_price <- predict(linear_r_model_price, test_sales)
linear_r_values_price <- round(linear_r_values_price, digits = 2)

# Actual vs Predicted for train and test datasets
par(mfrow = c(1,2))
plot(round(predict(linear_r_model_price)), train_sales$PriceReg, xlab = "Predicted", ylab = "Actual", ylim = c(0, 2500), xlim = c(0, 2500), col = c("darkorange"), main = "Actual vs Predicted on Train_DataSet")
abline(a=0, b=1, col = "darkblue")
plot(linear_r_values_price, test_sales$PriceReg, xlab = "Predicted", ylab = "Actual", ylim = c(0, 4000), xlim = c(0, 4000), col = c("darkorange"), main = "Actual vs Predicted on Test_DataSet")
abline(a=0, b=1, col = "darkblue")
par(mfrow = c(1,1))

# Check some regression metrics (errors, correlation value, r-squared value)
mmetric(test_sales$PriceReg, linear_r_values_price, c("MAE", "RMSE", "COR", "R2"))

# Try to improve the model:
# Eliminate extreme values (outliers) that can have segnificante effect on the construction of the model.
# Using cook's distance measure to analise the distance of the measures from the model.
cutoff <- 4/((nrow(train_sales) - length(linear_r_model_price$coefficients) - 2))
plot(linear_r_model_price, which = 4, cook.levels = cutoff)
plot(linear_r_model_price, which = 5, cook.levels = cutoff)

# Eliminate the extremes from the training data
train_sales <- train_sales[-which(rownames(train_sales) %in% c("15512", "37152", "36899", "75333")), ]

# Refit the model
linear_r_model_price <- lm(PriceReg ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + ItemCount, data = train_sales)
summary(linear_r_model_price)
par(mfrow = c(2, 2))
plot(linear_r_model_price, col = "darkorange")
par(mfrow = c(1, 1))

# Check for new extremes
cutoff <- 4/((nrow(train_sales) - length(linear_r_model_price$coefficients) - 2))
plot(linear_r_model_price, which = 4, cook.levels = cutoff)
plot(linear_r_model_price, which = 5, cook.levels = cutoff)

# Eliminate the extremes from the training data
train_sales <- train_sales[-which(rownames(train_sales) %in% c("36586", "27427", "29437", "2219", "18174", "40953")), ]

# Refit the model
linear_r_model_price <- lm(PriceReg ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + ItemCount, data = train_sales)
summary(linear_r_model_price)
par(mfrow = c(2, 2))
plot(linear_r_model_price, col = "darkorange")
par(mfrow = c(1, 1))

# Check for new extremes
cutoff <- 4/((nrow(train_sales) - length(linear_r_model_price$coefficients) - 2))
plot(linear_r_model_price, which = 4, cook.levels = cutoff)
plot(linear_r_model_price, which = 5, cook.levels = cutoff)

# Eliminate the extremes from the training data
train_sales <- train_sales[-which(rownames(train_sales) %in% c("52617", "21949", "47808", "73630", "15308")), ]

# Refit the model
linear_r_model_price <- lm(PriceReg ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + ItemCount, data = train_sales)
summary(linear_r_model_price)
par(mfrow = c(2, 2))
plot(linear_r_model_price, col = "darkorange")
par(mfrow = c(1, 1))

# Check for new extremes
cutoff <- 4/((nrow(train_sales) - length(linear_r_model_price$coefficients) - 2))
plot(linear_r_model_price, which = 4, cook.levels = cutoff)
plot(linear_r_model_price, which = 5, cook.levels = cutoff)

# Eliminate the extremes from the training data
train_sales <- train_sales[-which(rownames(train_sales) %in% c("51129", "17758", "6350", "31464", "1841", "74026")), ]

# Refit the model
linear_r_model_price <- lm(PriceReg ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + ItemCount, data = train_sales)
summary(linear_r_model_price)
par(mfrow = c(2, 2))
plot(linear_r_model_price, col = "darkorange")
par(mfrow = c(1, 1))

# Check for new extremes
cutoff <- 4/((nrow(train_sales) - length(linear_r_model_price$coefficients) - 2))
plot(linear_r_model_price, which = 4, cook.levels = cutoff)
plot(linear_r_model_price, which = 5, cook.levels = cutoff)

# Eliminate the extremes from the training data
train_sales <- train_sales[-which(rownames(train_sales) %in% c("5865", "12388", "34920", "26004", "2687")), ]

# Refit the model
linear_r_model_price <- lm(PriceReg ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + ItemCount, data = train_sales)
summary(linear_r_model_price)
par(mfrow = c(2, 2))
plot(linear_r_model_price, col = "darkorange")
par(mfrow = c(1, 1))

# Check for new extremes
cutoff <- 4/((nrow(train_sales) - length(linear_r_model_price$coefficients) - 2))
plot(linear_r_model_price, which = 4, cook.levels = cutoff)
plot(linear_r_model_price, which = 5, cook.levels = cutoff)

# Eliminate the extremes from the training data
train_sales <- train_sales[-which(rownames(train_sales) %in% c("48948", "50497", "2455", "32193", "14800")), ]

# Refit the model
linear_r_model_price <- lm(PriceReg ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + ItemCount, data = train_sales)
summary(linear_r_model_price)
par(mfrow = c(2, 2))
plot(linear_r_model_price, col = "darkorange")
par(mfrow = c(1, 1))

# Check for new extremes
cutoff <- 4/((nrow(train_sales) - length(linear_r_model_price$coefficients) - 2))
plot(linear_r_model_price, which = 4, cook.levels = cutoff)
plot(linear_r_model_price, which = 5, cook.levels = cutoff)

# Eliminate the extremes from the training data
train_sales <- train_sales[-which(rownames(train_sales) %in% c("36446", "47322", "13794", "35807", "52849", "50767")), ]

# Refit the model
linear_r_model_price <- lm(PriceReg ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + ItemCount, data = train_sales)
summary(linear_r_model_price)
par(mfrow = c(2, 2))
plot(linear_r_model_price, col = "darkorange")
par(mfrow = c(1, 1))

# Check for new extremes
cutoff <- 4/((nrow(train_sales) - length(linear_r_model_price$coefficients) - 2))
plot(linear_r_model_price, which = 4, cook.levels = cutoff)
plot(linear_r_model_price, which = 5, cook.levels = cutoff)

# Eliminate the extremes from the training data
train_sales <- train_sales[-which(rownames(train_sales) %in% c("31475", "40274", "33601", "2799", "13980")), ]

# Refit the model
linear_r_model_price <- lm(PriceReg ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + ItemCount, data = train_sales)
summary(linear_r_model_price)
par(mfrow = c(2, 2))
plot(linear_r_model_price, col = "darkorange")
par(mfrow = c(1, 1))

# Check for new extremes
cutoff <- 4/((nrow(train_sales) - length(linear_r_model_price$coefficients) - 2))
plot(linear_r_model_price, which = 4, cook.levels = cutoff)
plot(linear_r_model_price, which = 5, cook.levels = cutoff)

# Eliminate the extremes from the training data
train_sales <- train_sales[-which(rownames(train_sales) %in% c("55287", "36421", "49164", "32257", "11449")), ]

# Refit the model
linear_r_model_price <- lm(PriceReg ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + ItemCount, data = train_sales)
summary(linear_r_model_price)
par(mfrow = c(2, 2))
plot(linear_r_model_price, col = "darkorange")
par(mfrow = c(1, 1))

# Check for new extremes
cutoff <- 4/((nrow(train_sales) - length(linear_r_model_price$coefficients) - 2))
plot(linear_r_model_price, which = 4, cook.levels = cutoff)
plot(linear_r_model_price, which = 5, cook.levels = cutoff)

# Eliminate the extremes from the training data
train_sales <- train_sales[-which(rownames(train_sales) %in% c("65325", "26114", "49269", "52287")), ]

# Refit the model
linear_r_model_price <- lm(PriceReg ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + ItemCount, data = train_sales)
summary(linear_r_model_price)
par(mfrow = c(2, 2))
plot(linear_r_model_price, col = "darkorange")
par(mfrow = c(1, 1))

# Check for new extremes
cutoff <- 4/((nrow(train_sales) - length(linear_r_model_price$coefficients) - 2))
plot(linear_r_model_price, which = 4, cook.levels = cutoff)
plot(linear_r_model_price, which = 5, cook.levels = cutoff)

# Eliminate the extremes from the training data
train_sales <- train_sales[-which(rownames(train_sales) %in% c("8600", "53646", "43770")), ]

# Refit the model
linear_r_model_price <- lm(PriceReg ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + ItemCount, data = train_sales)
summary(linear_r_model_price)
par(mfrow = c(2, 2))
plot(linear_r_model_price, col = "darkorange")
par(mfrow = c(1, 1))

# Test the final model for each product in test_sales
linear_r_values_price_final <- predict(linear_r_model_price, test_sales)
linear_r_values_price_final <- round(linear_r_values_price_final, digits = 2)

# Compare some regression metrics from the original and final models (errors, correlation value, r-squared value)
mmetric(test_sales$PriceReg, linear_r_values_price, c("MAE", "RMSE", "COR", "R2")) # Valores do modelo inicial
mmetric(test_sales$PriceReg, linear_r_values_price_final, c("MAE", "RMSE", "COR", "R2")) # Valores do modelo final

# CONCLUSION: Although we made a new final model in order to improve the results, the final
#             model is pratically equal to the original model.



# Regression:             ----  DECISION TREES MODEL  ----
# Reset the values of train data previous altered
set.seed(1234)
train_sales <- historical_sales[train,]

# Build the model
tree_r_model_price <- rpart(PriceReg ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + ItemCount, data = train_sales, method = "anova")
summary(tree_r_model_price)
print(tree_r_model_price)

# Plot the generated tree
rpart.plot(tree_r_model_price, main = "Original Regression Tree")

# NOTE: The fitted values/possibilities are too little. Even if error is low, the range of predicted
#       values is not accepltable !!!

# Test the model for each product in test_sales
tree_r_values_price <- predict(tree_r_model_price, test_sales)
tree_r_values_price <- round(tree_r_values_price, digits = 2)

# Check some regression metrics (errors, correlation value, r-squared value)
mmetric(test_sales$PriceReg, tree_r_values_price, c("MAE", "RMSE", "COR", "R2"))



# Refit the model with tuned parameters 
tree_r_model_price_tuned <- rpart(PriceReg ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + ItemCount, data = train_sales, method = "anova", control = rpart.control(minsplit = 2, minbucket = 1, cp = 0.001))
summary(tree_r_model_price_tuned)
print(tree_r_model_price_tuned)

# Plot the generated tree
rpart.plot(tree_r_model_price_tuned, main = "Tuned Parameters Regression Tree")

# NOTE: The new tree produced an acceptable range of possible values for PriceReg

# Test the model for each product in test_sales
tree_r_values_price_tuned <- predict(tree_r_model_price_tuned, test_sales)
tree_r_values_price_tuned <- round(tree_r_values_price_tuned, digits = 2)

# Check some regression metrics (errors, correlation value, r-squared value)
mmetric(test_sales$PriceReg, tree_r_values_price_tuned, c("MAE", "RMSE", "COR", "R2"))



# Prune the tree to minimise errors
plotcp(tree_r_model_price_tuned)

cp = min(tree_r_model_price_tuned$cptable[3, ])
tree_r_model_price_tuned_pruned <- prune(tree_r_model_price_tuned, cp = cp)
summary(tree_r_model_price_tuned_pruned)
print(tree_r_model_price_tuned_pruned)

# Plot the pruned tree
rpart.plot(tree_r_model_price_tuned_pruned, main = "Tuned Parameters Regression Tree")

# NOTE: In order to minimize the errors the tree size is reduced to 3. Although this minimizes
#       the errors, the fitted values are too little to use size 3 tree.
#       This solution will be discarted.
#       Tuned tree will be considered for use.



# Regression:             ----  RANDOM FORESTS MODEL  ----
# Random forests models adapt to classification and regression accordingly to the type of
# the values in the target class.
set.seed(1234)
randomF_r_model_price <- randomForest(PriceReg ~ MarketingType + NewReleaseFlag + StrengthFactor + ReleaseYear + ItemCount, data = train_sales)
print(randomF_r_model_price)

# Plot errors
plot(randomF_r_model_price, main = "Errors Regression Random Forest")

# Plot variable importance
varImpPlot(randomF_r_model_price, sort = T, main = "Variable Importance")

# Test the model for each product in test_sales
randomF_r_values_price <- predict(randomF_r_model_price, test_sales)
randomF_r_values_price <- round(randomF_r_values_price, digits = 2)

# Check some regression metrics (errors, correlation value, r-squared value)
mmetric(test_sales$PriceReg, randomF_r_values_price, c("MAE", "RMSE", "COR", "R2"))



# CHOOSING THE  Given the results of all models, in each one was chosen the best
# BEST MODEL:   one obtained. Comparing now the best results of the three models:

# Comparing the best results of the three models:
mmetric(test_sales$PriceReg, linear_r_values_price_final, c("MAE", "RMSE", "COR", "R2"))
mmetric(test_sales$PriceReg, tree_r_values_price_tuned, c("MAE", "RMSE", "COR", "R2"))
mmetric(test_sales$PriceReg, randomF_r_values_price, c("MAE", "RMSE", "COR", "R2"))
a_price <- as.matrix(mmetric(test_sales$PriceReg, linear_r_values_price_final, c("MAE", "RMSE", "COR", "R2")))
b_price <- as.matrix(mmetric(test_sales$PriceReg, tree_r_values_price_tuned, c("MAE", "RMSE", "COR", "R2")))
c_price <- as.matrix(mmetric(test_sales$PriceReg, randomF_r_values_price, c("MAE", "RMSE", "COR", "R2")))
d_price <- cbind(a_price, b_price, c_price)
colnames(d_price) <- c("Linear Regression", "Decision Trees", "Random Forest")

# Based only on these metrics, the Random Forest model is very slightly the best.

# Valor monetário esperado = preço x quantidade
# Desenvolver uma função que recebe os valores esperados/previstos dos preços e das quantidades
# e calcula a sua multiplicação
valor_monetario_esperado <- function(dataframe_preco, dataframe_quantidade, dataframe_resultado) {
  dataframe_resultado = dataframe_preco * dataframe_quantidade
}

# Aplicar para os dados ativos. Aplicar os modelos escolhidos anteriormente para previsao de cada valor.
preco = round(predict(randomF_r_model_price, active_sales), digits = 2)
quantidade = round(predict(randomF_r_model, active_sales))
valor_monetario <- valor_monetario_esperado(preco, quantidade, valor_monetario)

# Representação em matrix
valor_monetario = as.matrix(valor_monetario)

