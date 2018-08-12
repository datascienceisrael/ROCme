---
title: "ROCme"
author: "Data Science Group"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

<br>

## Analyze Binary Classifiers

<br>

### Version 1.0.0

<br>

+ **Global Metrics** - Diagnose the ROC Curves, Predictions Distribution, and Prediction Buckets

+ **Local Metrics** Interactively Modify the Decision Threshold and Analyse Precision, Recall, Accuracy etc.


<br><br>

#### Current performance metrics included:

  * AUC Plots & Scores  
  * Predictions Distributions  
  * Sensitivity
  * Specificity
  * Precision
  * Recall
  * Balanced_accuracy

<br>


## Functionalities


### Info Tab:

Basic Information about the Application:

<br>
![](assets/rocme_info.png)
<br><br>


### Upload Tab:

Upload a .csv file in the expected format, as detailed in the `info` tab:

<br>
![](assets/rocme_upload.png)
<br><br>


### Global Metrics:

Metrics to evaluate the overall performance of the Model:

<br>
![](assets/rocme_global_roc.png)
<br>

<br>
![](assets/rocme_global_dist.png)
<br>

<br>
![](assets/rocme_global_bins.png)
<br><br>

### Local Metrics:

Interact with the Decision Threshold to inspect Local Performance Metrics:

<br>
![](assets/rocme_local.png)
<br><br>

