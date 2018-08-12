# imported libraries
library(dashboardthemes)
library(dplyr)
library(ggplot2)
require(gridExtra)
library(rmarkdown)
library(ROCR)
library(PRROC)
library(scales)
library(shiny)
library(shinydashboard)

# auxilliary functions
pf <- function() scales::percent_format()
rnd <- function(x) round(x, 3)

# Constants
N = 10L
N2 = 5L

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
  
  if (length(which(is.na(tab$real))) > 0) {
    warning("Missing Values in Label column")
  }
  
  # calculating the values for ROC curve
  pred <- ROCR::prediction(select(tab, pred), select(tab, real))
  perf <- ROCR::performance(pred, "tpr", "fpr")
  
  # calculating AUC
  auc <- ROCR::performance(pred,"auc")
  
  # now converting S4 class to vector
  auc <- unlist(slot(auc, "y.values"))
  auc <- paste0(c("AUC score = "), prettyNum(auc, digits = 3))
  
  # store the x and y values in a tibble
  dat <- tibble(x_vals = as.numeric(unlist(perf@x.values)),
                y_vals = as.numeric(unlist(perf@y.values)))
  
  # plot ROC using ggplot
  ggplot(dat, aes(x_vals, y_vals)) +
    geom_line(linetype = 1, size = .8, color = "tomato") +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +
    labs(title = "Area Under the ROC curve",
         x = "False Positive rate",
         y = "True Positive rate") +
    annotate(geom = "text", x = .7, y = .2, label = auc, size = 5, color = "blue") +
    scale_x_continuous(breaks = seq(0,1,0.1)) + scale_y_continuous(breaks = seq(0,1,0.1)) +
    scale_color_discrete(name = 'AUC score') +
    theme_bw()
}



# Binary classifier visualization attempt 1 ------------
mplot_density <- function(tab, model_name = NA) {
  ############################################
  # input: labels & predictions data.frame   #
  # in the epxected format, with columns:    #
  # 'pred' and 'real'                        #
  # Optional argument is model name (tbd)    # 
  #                                          #
  # output: area under the precision-recall  #
  # curve plot                               #
  ############################################
  
  # tag = real values, score = predictions
  tag = tab$real
  score = tab$pred
  
  if (length(tag) != length(score)) {
    message("The label and prediction probability vectors should be of the same length.")
    stop(message(paste("Currently, label has",length(tag),"rows and prediction score has",length(score))))
  }
  
  if (length(unique(tag)) != 2) {
    stop("This function is for binary models. You should only have 2 unique values for the tag value!")
  }
  
  out <- data.frame(label = as.character(tag),
                    score = as.numeric(score))
  
  p1 <- ggplot(out) +
    geom_density(aes(x = 100 * score, group = label, fill = as.character(label)), 
                 alpha = 0.6, adjust = 0.25) + 
    guides(fill = guide_legend(title="Label")) +
    labs(y = "Density by Label", x = "Score") + 
    scale_x_continuous(breaks = seq(0,100,10)) + 
    theme_minimal()
  
  p1
}



# Area under the Precision Recall Curve -----------
plot_aucpr <- function(tab = NULL) {
  ############################################
  # input: labels & predictions data.frame   #
  # in the epxected format, with columns:    #
  # 'pred' and 'real'                        #
  #                                          #
  # output: area under the precision-recall  #
  # curve plot                               #
  ############################################
  
  pr <- pr.curve(scores.class0=tab[tab$real=="1",]$pred,
                 scores.class1=tab[tab$real=="0",]$pred,
                 curve=T)
  
  y <- as.data.frame(pr$curve)
  aupr = paste("AUC-PR score:", prettyNum(pr$auc.integral, digits = 3))
  
  y %>% 
  ggplot(aes(V1, V2)) + 
    geom_path(col = 'navy', linetype = 1) +
    geom_hline(yintercept = 0.5, col = "black", linetype = 2) +
    labs(title = "Area Under the Precision-Recall Curve",
         y = "Precision", x = "Recall") +
    annotate(geom = "text", x = .8, y = .1, label = aupr, size = 5, color = "blue") +
    scale_x_continuous(breaks = seq(0,1,0.1)) + scale_y_continuous(breaks = seq(0,1,0.1)) +
    theme_bw()  
}



# Prediction probabilities scores -------
mplot_bins <- function(tab, splits = N, model_name = NA) {
  ############################################
  # input: labels & predictions data.frame   #
  # in the epxected format, with columns:    #
  # 'pred' and 'real'                        #
  # Optional arguments are no. splits (N)    #
  # and model_name (tbd)                     #
  #                                          #
  # output: predictions scores cumulative    #
  # probability plot                         #
  ############################################
  
  if (splits > 25) {
    stop("You should try with less splits!")
  }
  
  score = tab$pred
  deciles <- quantile(score, 
                      probs = seq((1/splits), 1, length = splits), 
                      names = TRUE)
  deciles <- data.frame(cbind(Deciles = row.names(as.data.frame(deciles)),
                              Threshold = as.data.frame(deciles)))
  
  p <- ggplot(deciles, 
              aes(x = reorder(Deciles, deciles), y = deciles, 
                  label = round(deciles, 2))) + 
    geom_col(fill="deepskyblue") + 
    geom_text(aes(fontface = "bold"), vjust = 1.5, size = 4, inherit.aes = TRUE, 
              colour = "white", check_overlap = TRUE) +
    labs(title = "Prediction Scores - Cumulative Probability Mass Function", 
         subtitle = paste("Using", splits, "equally-sized bins"),
         x = NULL, y = "Score") +
    theme_minimal()
  
  if(!is.na(model_name)) {
    p <- p + labs(caption = model_name)
  }

  p
}


# Tag according to ntile -----------------
mplot_splits <- function(tab, splits = N2, model_name = NA, facet = NA) {
  ############################################
  # input: labels & predictions data.frame   #
  # in the epxected format, with columns:    #
  # 'pred' and 'real'                        #
  # Optional arguments are model_name (tbd)  #
  # and no. splits (splits)                  #
  #                                          #
  # output: bins of prediction scores ranges #
  # with the corresponding label proportion  #
  # in each bin                              #
  ############################################
  
  score = tab$pred
  tag = tab$real
  
  if (length(tag) != length(score)) {
    message("The tag and score vectors should be the same length.")
    stop(message(paste("Currently, tag has",length(tag),"rows and score has",length(score))))
  }
  
  if (splits > 10) {
    stop("You should try with less splits!")
  }
  
  df <- data.frame(tag, score, facet)
  npersplit <- round(nrow(df) / splits)
  names <- df %>% 
    mutate(quantile = ntile(score, splits)) %>% group_by(quantile) %>%
    summarise(n = n(), 
              max_score = round(max(score), 2), 
              min_score = round(min(score), 2)) %>%
    mutate(quantile_tag = paste0(quantile," (", min_score, "-", max_score, ")"))
  
  p <- df %>% 
    mutate(quantile = ntile(score, splits)) %>% 
    group_by(quantile, facet, tag) %>% tally() %>%
    ungroup() %>% group_by(facet, tag) %>% 
    arrange(desc(quantile)) %>% 
    mutate(p = round(n / sum(n), 3),
           cum = cumsum(n / sum(n))) %>%
    left_join(names, by = c("quantile")) %>%
    ggplot(aes(x = as.character(tag), y = 100 * p, label = as.character(p),
               fill = as.character(quantile_tag))) +
    geom_col(position = "stack") +
    geom_text(aes(label = paste(100 * p, "%"), fontface = "bold"), 
              size = 4, 
              position = position_stack(vjust = 0.5), 
              check_overlap = TRUE) +
    guides(fill = guide_legend(title = "Scores Range in Bin")) +
    labs(title = "Prediction Scores Broken to Bins",
         x = "Label",
         y = NULL) +
    scale_fill_brewer(palette = "Spectral") +
    scale_y_continuous(labels = NULL) +
    theme_minimal()
  
  if(!is.na(model_name)) {
    p <- p + labs(caption = model_name)
  }
  
  p
}
