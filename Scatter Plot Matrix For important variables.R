install.packages("GGally")
library(GGally)


data <- read.csv("G:/R Programming/ML/Predicting Concrete Strength/Raw Data Concrete_Data_Yeh.csv")
data
head(data)
str(data)


library(GGally)
library(ggplot2)

ggpairs(
  important_vars,
  lower = list(continuous = wrap("points",
                                 alpha = 0.6,
                                 color = "blue")),
  
  diag = list(continuous = wrap("densityDiag",
                                fill = "skyblue")),
  
  upper = list(continuous = wrap("cor",
                                 size = 5))
) +
  theme_bw() +
  ggtitle("Scatter Plot Matrix of Important Concrete Variables")

#Caption
#Scatter plot matrix illustrating relationships among important concrete mixture variables and compressive strength. Upper panels represent correlation coefficients, diagonal panels show variable distributions, and lower panels present pairwise scatter plots.

