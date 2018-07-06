# imported libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(rmarkdown)
library(ggplot2)
library(ROCR)
library(scales)

# auxilliary functions
pf <- function() scales::percent_format()
rnd <- function(x) round(x, 3)

# Confusion Matrix
confMatrix <- function(crit = .5, tab){
  
  tab %>%
    mutate(pred = as.numeric(pred > crit)) %>%
    table()
}

# Evaluation Metrics
evalMetrics <- function(df, crit = .5){
  
  #confusion matrix is used to compute the stats
  cf <- confMatrix(tab = df, crit = crit)
  
  ##############
  ####### REAL #
  ####### T  F #
  #Pred ########
  #  T  # A  B #
  #  F  # C  D #
  ##############
  
  # correct predictions
  tp <- cf[1,1] # true positives
  tn <- cf[2,2] # true negatives
  
  # evaluation metrics
  sensitivity = rnd(tp/colSums(cf)[1]) # Sensitivity = A/(A+C)
  specificity = rnd(tn/colSums(cf)[2]) # Specificity = D/(B+D)
  precision   = rnd(tp/rowSums(cf)[1]) # Precision = A/(A+B)
  recall      = rnd(tp/colSums(cf)[1]) # Recall = A/(A+C)
  
  # Balanced Accuracy = (sensitivity+specificity)/2
  balanced_accuracy  = (sensitivity + specificity)/2
  
  # return
  tribble(
    ~Metric, ~Score,
    "sensitivity", pf()(sensitivity),
    "specificity", pf()(specificity),
    "precision",   pf()(precision),
    "recall",      pf()(recall),
    "balanced_accuracy", pf()(balanced_accuracy)
  )
}


# ROC curve plotting function
plotROC <- function(tab) {
  ##################################################
  # input: the real ad_id outcome column,          #
  # the predictions column as proportion in [0,1], #
  # the app name (defaults to empty string)        #
  # and a caption (defaults to empty string)       #
  #                                                #
  # output: an AUC curve with comparison to random #
  # model and a display of the absolute AUC value  #
  ##################################################
  
  # calculating the values for ROC curve
  pred <- ROCR::prediction(select(tab, pred), select(tab, real))
  perf <- ROCR::performance(pred,"tpr","fpr")
  
  # calculating AUC
  auc <- ROCR::performance(pred,"auc")
  
  # now converting S4 class to vector
  auc <- unlist(slot(auc, "y.values"))
  auc <- paste0(c("AUC score = "),prettyNum(auc, digits = 3))
  
  # store the x and y values in a tibble
  dat <- tibble(x_vals = as.numeric(unlist(perf@x.values)),
                y_vals = as.numeric(unlist(perf@y.values)))
  
  # plot ROC using ggplot
  ggplot(dat, aes(x_vals, y_vals, color = x_vals < y_vals)) +
    geom_line(linetype = 2, size = .8) +
    labs(title = "AUC curve",
         x = "False Positive rate",
         y = "True Positive rate") +
    annotate(geom = "text", x = .7, y = .2, label = auc, size = 5, color = "blue") +
    scale_x_continuous(breaks = seq(0,1,0.1)) + scale_y_continuous(breaks = seq(0,1,0.1)) +
    scale_color_discrete(name = 'AUC score',
                         labels = c('Random','Current Model')) +
    theme_bw()
}
