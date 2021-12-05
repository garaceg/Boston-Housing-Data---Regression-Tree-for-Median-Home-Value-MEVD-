#The Boston Housing Dataset

# The Boston Housing Dataset is a derived from information collected by 
# the U.S. Census Service concerning housing in the area of Boston MA. 
# The following describes the dataset columns:
  
# CRIM - per capita crime rate by town
# ZN - proportion of residential land zoned for lots over 25,000 sq.ft.
# INDUS - proportion of non-retail business acres per town.
# CHAS - Charles River dummy variable (1 if tract bounds river; 0 otherwise)
# NOX - nitric oxides concentration (parts per 10 million)
# RM - average number of rooms per dwelling
# AGE - proportion of owner-occupied units built prior to 1940
# DIS - weighted distances to five Boston employment centres
# RAD - index of accessibility to radial highways
# TAX - full-value property-tax rate per $10,000
# PTRATIO - pupil-teacher ratio by town
# B - 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
# LSTAT - % lower status of the population
# MEDV - Median value of owner-occupied homes in $1000's

# LOAD REQUIRED LIBRARIES
library(caret)
library(rpart)
library(rpart.plot)

# LOAD DATA
boston_df <- read.csv(file = 'Data.csv')
str(boston_df) # DISPLAY DATA STRUCTURE
head(boston_df) # DISPLAY TOP 5 ROWS

# REMOVE ALL NA (NULL) VALUES
na_Values <- length(is.na(boston_df) == T)
if (na_Values > 0) 
{
  boston_df <- boston_df[complete.cases(boston_df),]
}

# KEEP ONLY THE COLUMNS THAT ARE NEEDED (CRIM, CHAS, RM, DIS, TAX, PTRATIO AND MEDV)
drop_Columns <- c('ZN', 'INDUS', 'NOX', 'AGE', 'RAD','B','LSTAT')

boston_df <- boston_df[, !colnames(boston_df) %in% drop_Columns]
#boston_df <- boston_df[,c(1,4,6,8,10,11,14)]

# DISPLAY STRUCTURE TO VERIFY WE HAVE THE CORRECT COLUMNS
str(boston_df)

# BUILDS A REGRESSION TREE TO PREDICT MEDV USING THE REST OF THE VARIABLES (COLUMNS)
m1rtree <- rpart(MEDV~., data=boston_df, method="anova")

# DISPLAY REGRESSION TREE AS TEXT
m1rtree

# PLOTS REGRESSION TREE
rpart.plot(m1rtree, type=3, digits=5, fallen.leaves = TRUE)

# INTERPRETATION
# RM (Average number of rooms per dwelling) IS THE MAIN FACTOR TO DETERMINE THE MEDIAN VALUE OF THE HOME
# FOR CASES WHERE UNDER RM IS UNDER 6.9715, CRIM (crime rate) IS A CRUCIAL FACTOR
# PTRATIO (pupil teacher ratio) IS AN IMPORTANT FACTOR FOR A COUPLE OF FINAL NODES

  