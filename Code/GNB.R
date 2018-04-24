###################################################
#
# Gaussian Naive Bayes Engine
#
# Sam Showalter
# Kris Nguyen
###################################################

#Import test data
library(datasets)
data("iris")


#MUST PROVIDE OWN KEY
key = c("virginica", "versicolor", "setosa")

#Split the data into train and test samples
train_test_split = function(data, test_ratio)
{
  sample_nums = 1:nrow(data)
  
  #Pick random number of indices for the test rows
  #print(rownames(iris))
  test_row_nums = sample(sample_nums, test_ratio*nrow(data), replace = FALSE, prob = NULL)
  
  #Get testing and training rows
  test_rows = data[test_row_nums,]
  train_rows = data[-test_row_nums,]
  
  # print(nrow(test_rows))
  # print(" ")
  # print(nrow(train_rows))
  return(list("train" = train_rows, "test" = test_rows))
}


#Make a label for each class in a master data object
#Prediction label must be the last column
master_preprocessing = function(data, key, test_ratio)
{
  #Get label text of last column
  label_text = data[,length(data)]
  
  #Delete the last colum and rename it tempclass
  data = data[,-length(data)]
  data$tempclass = label_text
  
  #Create final class data
  for (key_item in 1:length(key))
  {
    data$class[data$tempclass == key[key_item]] = key_item
  }
  
  #drop tempclass data after use
  data = subset(data, select = -c(tempclass))
  
  #Split the data to train and test
  train_test = train_test_split(data, test_ratio)
  
  #Get train and test subsets
  train = train_test$train
  test = train_test$test
  
  #Return a list of all the data information
  return(list("train" = train, "test" = test, "key" = key))
}

gaussProb = function(mean,stddev,x)
{
  #Get numerator and denominator for pdf of normal dist
  #Can also be done with punif
  num = exp(-((x - mean)**2) / (2*(stddev**2)))
  den = stddev*sqrt(2*pi)
  
  #Return the predictions
  if (den == 0) {return(0)}
  return(num / den)
}


#Separates records in the training dataset by class (label).
#This takes an input of a training dataset and outputs a dictionary of records by class.
class_sep = function(train, key) {
  class_dict = vector(mode="list", length=length(key))
  names(class_dict) = seq(1, length(key))
  
  for (i in 1 : length(key)) {class_dict[[i]] = train[train[, length(train)] == i, - length(train)]}
  
  return (class_dict)
}

#Creates a dictionary of the mean and standard deviation of every feature for each class.
#This takes a dictionary of separated training data, and outputs a dictionary of summary statistics
#(mean and stdev). For a dataset with three  classes and four features, there would be a total of
#3x4 = 12 means and stdevs, separated by class.
class_stats = function(train_class_dict) {
  class_stats_dict = vector(mode="list", length=length(train_class_dict))
  names(class_stats_dict) = seq(1, length(train_class_dict))
  
  for (i in 1 : length(train_class_dict)) {
    #train_class_dict["mean"] 
    for (name in names(train_class_dict[[i]])) {
      # add two additional rows to each sub-list
      train_class_dict[[i]]["mean", name] = mean(train_class_dict[[i]][, name], na.rm=T) # returns NA if not specify na.rm=T
      train_class_dict[[i]]["sd", name] = sd(train_class_dict[[i]][, name], na.rm=T)
    }
    # assign the two additional rows to appropriate sub-list in `class_stats_dict`
    class_stats_dict[[i]] = train_class_dict[[i]][c("mean", "sd"), ]
  }
  
  return (class_stats_dict)
}


data = master_preprocessing(iris,key, 0.4)
data

sep_by_class = class_sep(data$train, data$key)
sep_by_class

class_stats_dict = class_stats(sep_by_class)
class_stats_dict