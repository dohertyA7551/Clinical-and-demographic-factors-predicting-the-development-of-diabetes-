

library(mlbench)
library(pROC)
library(ggplot2)
library(dplyr) 
library(caret) 
library(tidyverse)
library(tidyr)

if (!requireNamespace("BiocManager", quietly = TRUE))
install.packages("BiocManager")
BiocManager::install("epiDisplay")
library(epiDisplay)


data("PimaIndiansDiabetes")
diab <- PimaIndiansDiabetes
view(diab)
class(diab)
str(diab)
names(diab)
head(diab)

#right lets deal with implausible values
data("PimaIndiansDiabetes")
diabetes <- PimaIndiansDiabetes


cols <-c("glucose","pressure","triceps","insulin","mass","pedigree","age")
diabetes[cols] <- lapply(diabetes[cols], function(x) {
  x[x == 0] <- NA
  x
})
head(diabetes) 

diab <-na.omit(diabetes)
diab
#now onto summary stats
#strat first

diab_neg <- diab[diab$diabetes =="neg",]
diab_pos <- diab[diab$diabetes =="pos",]


library(dplyr)

diab_summary <- diab %>%
  group_by(diabetes) %>%
  summarise(
    across(
      c(pregnant, glucose, pressure, triceps, insulin, mass, pedigree, age),
      list(
        Mean = ~ mean(., na.rm = TRUE),
        SD   = ~ sd(., na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )

View(diab_summary)


#now figs for most significant predictors

diab_long <- diab_summary %>%        #into long format for gg
  pivot_longer(
    cols = -diabetes,
    names_to = c("variable", ".value"),
    names_pattern = "^(.*)_(Mean|SD)$",
  
  )

names(diab_long)

# Subset to glucose only
tmp <- diab_long[diab_long$variable == "glucose", ]

# Ensure correct order: neg then pos
tmp$diabetes <- factor(tmp$diabetes, levels = c("neg", "pos"))
tmp <- tmp[order(tmp$diabetes), ]

# Extract values
means <- tmp$Mean
sds   <- tmp$SD

# Draw bar plot
bp <- barplot(
  means,
  names.arg = c("No diabetes", "Diabetes"),
  col = c("grey70", "grey40"),
  ylim = c(0, max(means + sds) * 1.15),
  ylab = "Mean glucose",
  main = "Glucose by diabetes status",
  border = "black"
)

# Add SD error bars
arrows(
  x0 = bp, y0 = means - sds,
  x1 = bp, y1 = means + sds,
  angle = 90,
  code = 3,
  length = 0.08,
  lwd = 2
)


tmp <- diab_long[diab_long$variable == "pregnant", ]
tmp$diabetes <- factor(tmp$diabetes, levels = c("neg", "pos"))
tmp <- tmp[order(tmp$diabetes), ]

means <- tmp$Mean
sds   <- tmp$SD

bp <- barplot(
  means,
  names.arg = c("No diabetes", "Diabetes"),
  col = c("grey70", "grey40"),
  ylim = c(0, max(means + sds) * 1.15),
  ylab = "Mean pregnancies",
  main = "Pregnancies by diabetes status",
  border = "black"
)

arrows(bp, means - sds, bp, means + sds,
       angle = 90, code = 3, length = 0.08, lwd = 2)



tmp <- diab_long[diab_long$variable == "mass", ]
tmp$diabetes <- factor(tmp$diabetes, levels = c("neg", "pos"))
tmp <- tmp[order(tmp$diabetes), ]

means <- tmp$Mean
sds   <- tmp$SD

bp <- barplot(
  means,
  names.arg = c("No diabetes", "Diabetes"),
  col = c("grey70", "grey40"),
  ylim = c(0, max(means + sds) * 1.15),
  ylab = "Mean BMI",
  main = "BMI by diabetes status",
  border = "black"
)

arrows(bp, means - sds, bp, means + sds,
       angle = 90, code = 3, length = 0.08, lwd = 2)


tmp <- diab_long[diab_long$variable == "pressure", ]
tmp$diabetes <- factor(tmp$diabetes, levels = c("neg", "pos"))
tmp <- tmp[order(tmp$diabetes), ]

means <- tmp$Mean
sds   <- tmp$SD

bp <- barplot(
  means,
  names.arg = c("No diabetes", "Diabetes"),
  col = c("grey70", "grey40"),
  ylim = c(0, max(means + sds) * 1.15),
  ylab = "Mean blood pressure",
  main = "Blood pressure by diabetes status",
  border = "black"
)

arrows(bp, means - sds, bp, means + sds,
       angle = 90, code = 3, length = 0.08, lwd = 2)


tmp <- diab_long[diab_long$variable == "age", ]
tmp$diabetes <- factor(tmp$diabetes, levels = c("neg", "pos"))
tmp <- tmp[order(tmp$diabetes), ]

means <- tmp$Mean
sds   <- tmp$SD

bp <- barplot(
  means,
  names.arg = c("No diabetes", "Diabetes"),
  col = c("grey70", "grey40"),
  ylim = c(0, max(means + sds) * 1.15),
  ylab = "Mean age",
  main = "Age by diabetes status",
  border = "black"
)

arrows(
  bp, means - sds,
  bp, means + sds,
  angle = 90,
  code = 3,
  length = 0.08,
  lwd = 2
)


####Task 2### model making

diab$diabetes <- as.character(diab$diabetes)

#convert pos and neg to binary
diab$diabetes[diab$diabetes == "pos"] <- "1"
diab$diabetes[diab$diabetes == "neg"] <- "0"
diab$diabetes <- as.numeric(diab$diabetes)
table(diab$diabetes)
str(diab$diabetes)


 
# full logistic model with all predictors included
model_1 <- glm(diabetes ~ pregnant + glucose + pressure + triceps + insulin + mass + pedigree + age,
           data = diab, family = binomial)
summary(model_1)

#reduced model with non-significant predictors* removed

model_2 <-glm(diabetes~ + glucose + mass +pedigree +age, data =diab, family = binomial)
summary(model_2)

#get info for table
coef_table <- summary(model_2)$coefficients
coef_table

OR <- exp(coef(model_2))
CI <- exp(confint(model_2))

results_table <- data.frame(
  Variable = names(OR),
  Coefficient = coef(model_2),
  Odds_Ratio = OR,
  CI_lower = CI[,1],
  CI_upper = CI[,2]
)

results_table

results_table[, -1] <- round(results_table[, -1], 3)
results_table


###Task 3###
diab_predicting <- predict(model_2, type ="response")
head(round(diab_predicting,5))
diab_class <- ifelse(diab_predicting >= 0.5, 1, 0)
head(diab_class)


##making confusion matrix
conf_mat <- table(
  Predicted = diab_class,
  Observed  = model.frame(model_2)$diabetes
)

conf_mat



##sens, spes ect##
TN <- conf_mat["0","0"]
FN <- conf_mat["0","1"]
FP <- conf_mat["1","0"]
TP <- conf_mat["1","1"]


sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)
ppv         <- TP / (TP + FP)
npv         <- TN / (TN + FN)
accuracy    <- (TP + TN) / sum(conf_mat)


round(c(
  Sensitivity = sensitivity,
  Specificity = specificity,
  PPV = ppv,
  NPV = npv,
  Accuracy = accuracy
), 3)

##onto AUC and ROC
library(pROC)
library(pROC)

roc_curve <- roc(
  response  = model.frame(model_2)$diabetes,
  predictor = diab_predicting
)

auc(roc_curve)
ci.auc(roc_curve)

plot(
  roc_curve,
  main = "ROC Curve for Final Diabetes Prediction Model",
  col = "blue",
  lwd = 2
)

abline(a = 0, b = 1, lty = 2, col = "grey")


###youdens

youden <- coords(
  roc_curve,
  x = "best",
  best.method = "youden",
  ret = c("threshold", "sensitivity", "specificity")
)

youden


###plotting model 1 (full) vs model2 (reduced with onnly signif var)

roc_full <- roc(
  response  = model.frame(model_1)$diabetes,
  predictor = predict(model_1, type = "response")
)

# Reduced model
roc_reduced <- roc(
  response  = model.frame(model_2)$diabetes,
  predictor = predict(model_2, type = "response")
)


plot(
  roc_full,
  col = "blue",
  lwd = 2,
  main = "ROC Curves for Full and Reduced Models"
)

plot(
  roc_reduced,
  col = "red",
  lwd = 2,
  add = TRUE
)

legend(
  "bottomright",
  legend = c("Full model", "Reduced model"),
  col = c("blue", "red"),
  lwd = 2
)

abline(a = 0, b = 1, lty = 2, col = "grey")

auc(roc_full)
ci.auc(roc_full)

auc(roc_reduced)
ci.auc(roc_reduced)

anova(model_1,model_2)
