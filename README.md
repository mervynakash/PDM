# Introduction
The package name PDM (Project Dennis Martin) is inspired by the most intriguing missing child's case of Dennis Martin on June 14, 1969 in the Great Smoky Mountains National Park in Tennessee at the age of six. 
More details can be found here - https://en.wikipedia.org/wiki/Disappearance_of_Dennis_Martin.
The package is in honour for all the missing people who have left us. 

# What does it do?
The PDM package is used to find the missing values associated in the dataset - both categorical and numerical. 

# Why use this package?
This package is pretty straight forward - you give the data frame and threshold value. Sometimes there will be too many missing values in a particular feature and it is not a good practice to vaguely impute values of our choice. Threshold takes a value as input from the user and the package will compare the percentage of missing values in the columns less than the given threshold and remove the other columns.
It then imputes (both categorical and continuous) the missing values of other column disregarding the column(s) it has axed.
Categorical imputation is done by decision tree and continuous imputation is done through mean/median depending upon the normality.

# Installation
To get the current development version from github:
```R
install.packages("devtools")
devtools::install_github("mervynakash/PDM")
```

# Functions
```pdm_impute(<df>,<threshold>)```: The method takes the data set and threshold value (default = 20) and returns cleaned data set ie after imputing the missing values.
Let's take an example:
```R
air <- airquality

# To get the number of missing values in each column
print(colSums(is.na(air)))

# Imputing missing values and keeping threshold = 10
df <- pdm_impute(air,20)

print(colSums(is.na(df)))
```
In the above code you'll see that the feature "Ozone" is missing. This is because the missing value percentage in Ozone column is 24% which is more than the given threshold value = 10.
Change the value of threshold to 30 and see the difference.

# Conclusion
There are many other packages which deliver missing value imputation through RandomForest, kNN etc. But the idea behind continuous value imputation is a bit different from what I've generally seen in the imputations.
The package is under development and any bugs or errors are highly appreciated. 
Please do give feedbacks on how to improve the package. Criticism is also given high priority. 
Mail me at mervyn.akash10@gmail.com

Cheers! Happy Coding!!
