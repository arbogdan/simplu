#################################################################################################################
### Program: Simplu, a comprehensive dataset summarization protocol
### Author: Alexander R. Bogdan
### Date Created: April 10th, 2017
### Date Last Modified: April 11th, 2017
### To-Do Notes: Fix 'test.decision' for categorical varibles to account for both variables in statistical test
###              Create conditions for bivariate analyses of continuous vs. categorical variables
###              Create graphics for continuous vs. categorical variables
###              Clean up histogram titles, axes, general themes
###              Create dependent variable functionality: Specify DV and program will suggest tests & features
###              Create output mechanism (.txt or .pdf)
#################################################################################################################


# function(data, dependent.var)


# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(PerformanceAnalytics)


# Create matrix to store variable names and types
data.matrix = as.data.frame(matrix(NA, nrow = ncol(mtcars), ncol = 2))
colnames(data.matrix) = c("var.name", "var.type")
for (i in 1:ncol(mtcars)){
  var.name = colnames(mtcars)[i]
  var.type = typeof(mtcars[, i])
  if (var.type == "numeric" | var.type == "double"){
    int.test = sum(ifelse(mtcars[, i] %% 1, 1, 0))
    int.levels = n_distinct(mtcars[, i])
    var.type = ifelse(int.test >= 1 | int.levels >= 14, var.type, "integer")  # If a categorical variable as more than 14 levels, someone messed up 
  }
  data.matrix[i, 1] = var.name
  data.matrix[i, 2] = var.type
}
data.matrix


# Create sublist of all variables of type "double" & "numeric"
dbl.list = data.matrix %>%
  filter(var.type == "double" | var.type == "numeric")
numeric.list = input.data %>%
  select(one_of(unlist(dbl.list[1])))
numeric.list = data.frame(numeric.list)


# Create table for all continuous variables
continuous.table = matrix(NA, nrow = ncol(numeric.list), ncol = 11)
continuous.table[1:ncol(numeric.list), 2] = nrow(numeric.list)
for (i in 1:ncol(numeric.list)){
  continuous.table[i, 1] = colnames(as.data.frame(numeric.list))[i]
  continuous.table[i, 3] = sum(is.na(numeric.list[, i]))
  continuous.table[i, 4] = ifelse(continuous.table[i, 3] == 0, 0, continuous.table[i, 3] / continuous.table[i, 2])
  continuous.table[i, 5] = round(min(numeric.list[, i], na.rm = TRUE), 2)
  continuous.table[i, 6] = round(quantile(numeric.list[, i], c(0.25), na.rm = TRUE), 2)
  continuous.table[i, 7] = round(median(numeric.list[, i], na.rm = TRUE), 2)
  continuous.table[i, 8] = round(mean(numeric.list[, i], na.rm = TRUE), 2)
  continuous.table[i, 9] = round(quantile(numeric.list[, i], c(0.75), na.rm = TRUE), 2)
  continuous.table[i, 10] = round(max(numeric.list[, i], na.rm = TRUE), 2)
  norm.test = round(shapiro.test(numeric.list[, i])$p.value, 2)
  is.normal = ifelse(norm.test <= 0.05, "No", "Yes")
  continuous.table[i, 11] = paste(is.normal, ", p=", norm.test, sep="")
}
colnames(continuous.table) = c("var.name", "n", "n.miss", "pct.miss", "min", "P25", "median", "mean", "P75", "max", "normal")
data.frame(continuous.table)


# Create histograms for each continuous variable
for (i in 1:ncol(numeric.list)){
  my.plot = ggplot(aes(x = numeric.list[, i]), data = numeric.list) + geom_histogram(aes(y = ..density..), color = "red") + geom_density(alpha = 0.2) +
    ggtitle(paste("Distribution of ", colnames(numeric.list)[i], sep = "")) + theme(plot.title = element_text(hjust = 0.5))
  print(my.plot)
}


# Use PerformanceAnalytics package to create chart.Correlation graphic
library(PerformanceAnalytics)
chart.Correlation(numeric.list)


# Create tile-correlation graphic
## Need to include logical criterion for differentiating between Pearson & Spearman
#cor.values = round(cor(numeric.list[, sort(1:length(numeric.list))], method = "pearson", use = "pairwise.complete.obs"), 2)
#cor.values[lower.tri(cor.values)] = NA
#cor.values
#cor.values = melt(cor.values)
#cor.values$Var1 = as.character(cor.values$Var1)
#cor.values$Var2 = as.character(cor.values$Var2)
#cor.values = na.omit(cor.values)
#cor.plot = ggplot(aes(x = Var2, y = Var1), data = cor.values) +
#  geom_tile(aes(fill = value), data = cor.values, color = "white") +
#  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limit = c(-1, 1), name = "Correlation (Pearson)") +
#  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 11)) +
#  coord_equal()
#cor.plot



# Discrete variable table
## Need to include column for results of Binomial test & Chi-square goodness of fit test (tests if all probabilities are equal)
discrete.vars = data.matrix %>%
  filter(var.type == "integer" | var.type == "character")
discrete.list = mtcars %>%
  select(one_of(unlist(discrete.vars[1])))
discrete.table = matrix(NA, nrow = ncol(discrete.list), ncol = 7)
discrete.table[1:ncol(discrete.list), 2] = nrow(discrete.list) # Specifies N for 2nd column
categorical.pvalues = matrix(NA, nrow = length(discrete.list), ncol = length(discrete.list))
for (i in 1:ncol(discrete.list)){
  discrete.table[i, 1] = colnames(as.data.frame(discrete.list))[i]
  discrete.table[i, 3] = sum(is.na(discrete.list[, i]))
  discrete.table[i, 4] = ifelse(discrete.table[i, 3] == 0, 0, discrete.table[i, 3] / discrete.table[i, 2])
  discrete.table[i, 5] = length(levels(factor(discrete.list[, i])))
  discrete.table[i, 6] = paste(levels(factor(discrete.list[, i])), collapse = ", ")
  discrete.table[i, 7] = round(ifelse(discrete.table[i, 5] > 2, chisq.test(table(discrete.list[, i]))$p.value, prop.test(sum(discrete.list[, i]), length(discrete.list[, i]), p = 0.5)$p.value), 2)
  # Create bivariate output
  test.decision = ifelse(table(discrete.list[i]) > 5, 1, 0)
  which.categorical.test = ifelse(sum(test.decision) / length(test.decision) == 1, 1, 0)
  # test.decision statement only accounts for ith variable, not jth variable; must account for both
  if (which.categorical.test == 1){
    for (j in i:length(discrete.list)){
      chi.pval = round(chisq.test(discrete.list[, i], discrete.list[, j], simulate.p.value = TRUE, B = 25000)$p.value, 2)
      categorical.pvalues[i, j] = paste("Chi: ", chi.pval, sep = "")
    }
  } else{
    for (j in i:length(discrete.list)){
      fisher.pval = round(fisher.test(discrete.list[, i], discrete.list[, j], simulate.p.value = TRUE, B = 25000)$p.value, 2)
      categorical.pvalues[i, j] = paste("Fisher: ", fisher.pval, sep = "")
    }
  }
}
colnames(discrete.table) = c("var.name", "n", "n.miss", "pct.miss", "levels", "values", "p-val")
colnames(categorical.pvalues) = colnames(discrete.list)
rownames(categorical.pvalues) = colnames(discrete.list)
data.frame(discrete.table)
data.frame(categorical.pvalues)




