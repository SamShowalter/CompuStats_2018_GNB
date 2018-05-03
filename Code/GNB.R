###################################################
#
# Gaussian Naive Bayes Engine
#
# Sam Showalter
# Kris Nguyen
###################################################

#clear initial directory
rm(list = ls())

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

predict = function(data, class_stats)
{
  #Remove result from test
  test = data$test[-c(length(data$test))]

  #Results list and probabilities
  probList = c()
  predictions = c()
  
  for (testrec in 1:nrow(test))
  {
    result_row = c()
    for (class in names(class_stats))
    {
      #print(class)
      class_info = class_stats[class]
      class_prob = 1
      
      #Get probability from each column
      for (name in names(class_info[[class]])){class_prob = class_prob * gaussProb(class_info[[class]]["mean",name],class_info[[class]]["sd",name], test[testrec, name])}
      
      result_row = c(result_row,class_prob)
    }
    #print(result_row)
    probList = c(probList, result_row)
    prediction = which.max(result_row)
    
    predictions = c(predictions, prediction)
  }
  
  probList = matrix(probList,                     # the data elements 
                    nrow=nrow(test),              # number of rows 
                    ncol=length(class_stats),     # number of columns 
                    byrow = TRUE)                 # fill matrix by rows 
  
  #Give dataframe probabilities and predictions
  data$probList = probList
  data$preds = predictions
  
  #Return data
  return(data)
}

getAccuracy = function(data)
{
  correct = 0
  for (i in 1:length(data$preds)){if (data$preds[i] == data$test$class[i]) {correct = correct + 1}}
  
  return(correct/length(data$preds))
}

get.individual.accuracy = function(data, labels, visualize=F) {
  accuracies = c(rep(0, length(labels) + 1))
  nlabels = c(rep(0, length(labels)))
  
  for (i in 1 : length(data$preds)) {
    nlabels[data$test$class[i]] = nlabels[data$test$class[i]] + 1
    
    if (data$preds[i] == data$test$class[i]) {
      accuracies[data$preds[i]] = accuracies[data$preds[i]] + 1
      accuracies[length(labels) + 1] = accuracies[length(labels) + 1] + 1
    }
  }
  
  nlabels = c(nlabels, length(data$preds))
  #print(accuracies)
  #print(nlabels)
  for (i in 1 : (length(labels) + 1)) {accuracies[i] = accuracies[i] / nlabels[i] * 100}
  #print(accuracies)
  
  if (visualize) {
    xx = barplot(
      accuracies,
      main = "Model Accuracy Across Classes",
      names.arg = c(labels, "Overall"),
      col = "blue",
      yaxis = "i", xaxis = "i",
      axes = F
    )
    axis(2, at=seq(0, 100, 20), las=1)
    segments(0, 0, 5, 0, xpd=T)
    text(
      x = xx,
      y = accuracies,
      label = paste(round(accuracies, 2), "%", sep=""),
      pos = 3,
      col = "red",
      xpd = T
    )
    text(
      x = 0, y = 110,
      expression("Percent"),
      xpd = T
    )
  }
  
  return (accuracies)
}

MonteCarloSim = function(orig_data, key, test_ratio, iter = 100, viz = F)
{
  final_results = c()
  
  for (iteration in 1:iter)
  {
    if(iteration %% (iter/10) == 0)
    {
      print(paste(iteration, "/",iter))
    }
    data = master_preprocessing(orig_data,key, test_ratio)
    sep_by_class = class_sep(data$train, data$key)
    class_stats_dict = class_stats(sep_by_class)
    data = predict(data, class_stats_dict)
    accuracies = get.individual.accuracy(data, key, visualize=viz)
    final_results = c(final_results, accuracies)
  }
  
  #print(final_results)
  accuracy_matrix = matrix(final_results,                # the data elements 
                           nrow=iter,                    # number of rows 
                           ncol=length(key) +1,     # number of columns plus one for overall
                           byrow = TRUE)                 # fill matrix by rows 
  
  return(colMeans(accuracy_matrix, na.rm = FALSE, dims = 1))
}


GNBtest = function(orig_dataset, key, test_ratio)
{
  data = master_preprocessing(orig_dataset,key, 0.4)
  sep_by_class = class_sep(data$train, data$key)
  class_stats_info = class_stats(sep_by_class)
  data = predict(data, class_stats_info)
  accuracies = get.individual.accuracy(data, key, visualize=T)
  names(accuracies) = acc_names
  data$accuracies = accuracies
  
  return(data)
}

########################################################################
#
# Testing GNB with iris flower data
#
########################################################################
#Import test data
library(datasets)
data("iris")

#MUST PROVIDE OWN KEY
key = c("virginica", "versicolor", "setosa")

#Run single test for iris data
GNBtest(iris,key,test_ratio = 0.4)

########################################################################
#
# Testing GNB with breast cancer data
#
########################################################################

#Create a key
key = c("Benign","Malignant")

#Undergo preprocessing for data
cancer = read.csv("/Users/Sam/Documents/Depauw/04_Senior_Year/Semester_2/CompuStats/CompuStats_2018_GNB/breast_cancer.txt",
                  stringsAsFactors = T)
cancer = data.frame(cancer, stringsAsFactors = F)
cancer$bare_nuclei = as.character(cancer$bare_nuclei)
cancer$bare_nuclei[cancer$bare_nuclei == "?"] = "0"
cancer$bare_nuclei = as.numeric(cancer$bare_nuclei)
cancer = subset(cancer, select = -c(patient_id))
cancer$class[cancer$class == 2] = "Benign"
cancer$class[cancer$class == 4] = "Malignant"

# View data if you want
#cancer

#Run single GNB test
data = GNBtest(cancer, key,0.4)

#Run monte carlo simulation and view the results
monte_carlo_perf = MonteCarloSim(cancer,key,test_ratio = 0.4, iter = 50, viz = T)
monte_carlo_perf



