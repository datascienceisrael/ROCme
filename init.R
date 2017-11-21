# imported libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(rmarkdown)
library(ggplot2)
library(ROCR)
library(scales)

# (!) magic numbers
N_CRIT = 50

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
  f1score     = rnd(2*tp/(2*tp + cf[1,2] + cf[2,1])) # F1score = 2xA/(2xA + B + C)
  accuracy    = rnd((tp + tn)/sum(cf)) # Accuracy = (A+D)/(A+B+C+D)
  
  # Balanced Accuracy = (sensitivity+specificity)/2
  balanced_accuracy  = (sensitivity + specificity)/2
  informedness = sensitivity + specificity - 1
  
  # return
  tibble(
    "sensitivity" = pf()(sensitivity),
    "specificity" = pf()(specificity),
    "precision"   = pf()(precision),
    "recall"      = pf()(recall),
    "F1score"     = pf()(f1score), 
    "accuracy"    = pf()(accuracy),
    "balanced_accuracy" = pf()(balanced_accuracy),
    "informedness"      = pf()(informedness)
  )
}


critReco <- function(tab = NULL, w_fp = .5) {

  # incorrect predictions
  crit_grid = seq(0,1, length.out = N_CRIT)
  
  # weights for false positives and false negatives
  w_fn = 1 - w_fp
  
  # init empty data.frame to hold False Positives/Negatives count
  false_count = data.frame(FP = rep(NA, N_CRIT),
                           FN = rep(NA, N_CRIT))
  
  # compute fp and fn for each decision criterion
  i = 1;
  for (crit in crit_grid){
    
    # false predictions table
    falses <- 
      tab %>%
      mutate(pred = as.numeric(pred > crit)) %>% 
      filter(pred != real)  
    
    # fill in the FP/NP count according to each criterion in the grid
    false_count$FP[i] <- nrow(filter(falses, pred == 1)) 
    false_count$FN[i] <- nrow(filter(falses, pred == 0)) 
    
    # increment index
    i = i + 1
  }
  
  # add missclassifications cost and the different decision criterion
  false_count <- 
    false_count %>%
    mutate(crit = crit_grid) %>% 
    mutate(cost = w_fp * FP + w_fn *FN) 
  
  # best criterion
  idx = which.min(false_count$cost)
  min_cost = false_count$cost[idx]
  bst_crit = false_count$crit[idx]
  txt = paste("Best criterion:", round(bst_crit, digits = 3), "Cost:", min_cost)
  
  # plot and return
  false_count %>% 
    ggplot(aes(x = crit, y = cost)) +
    geom_point(size = 3.5, shape = 1) + 
    geom_smooth() +
    labs(title = "Criterion Recommendation",x = "Decision Criterion", y = "Weighted Error Cost") +
    annotate(geom = "text", label = txt, x = bst_crit, y = (min_cost - 1), size = 4.5, color = "blue") +
    theme_bw()
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
