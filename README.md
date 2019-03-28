---
title: "Tree structured ParamSets"
vignette: >
  %\VignetteIndexEntry{Tree structured ParamSets}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



# ParamSetTree 

## Setup
Install the Tree branch


```r
devtools::install_github("smilesun/parabox", ref = "tree")
```

This tutorial will guide you how to set up conditional hyperparameters for SVM and Random Forest, setting up Machine Learning Pipeline and Deep Learning Architecture 

## Support Vector Machine/Random Forest Model Multiplexer Hyper-parameter Specification
Some hyper-parameters could depend on other hyper-parameters. For example, if we have a model multiplexer of Support Vector Machine(SVM) and Random Forest(RF). The hyper-parameter "kernel" only makes sense if the chosen model is "SVM". Moreover, the hyper-parameter of "gamma" only makes sense if the chosen kernel for "SVM" is "rbf". This kind of relationship resembles a tree or directed graph structure where a node is only activated when its corresponding parent node is activated. 

To tackle the above mentioned problem, we have the following code. Each separate hyper-parameter has a unique identifier called "id" which is defined inside the ParamSimple(ParamCategorical, ParamReal, ..etc). These "id"s are extracted from ParamSimple and used as the unique identifier as the node in the tree as well.


```r
library(phng)
pst = ParamSetTree$new("test",
  ParamCategorical$new(id = "model", values = c("SVM", "RF")),
  addDep(
    ParamReal$new(id = "C", lower = 0, upper = 100), 
    did = "model", expr = quote(model == "SVM")), # did here means dependant id
  addDep(
    ParamCategorical$new(id = "kernel", values = c("rbf", "poly")), 
    did = "model", expr = quote(model == "SVM")),
  addDep(
    ParamReal$new(id = "gamma", lower = 0, upper = 101), 
    did = "kernel", expr = quote(kernel == "rbf")),
  addDep(
    ParamInt$new(id = "n", lower = 1L, upper = 10L), 
    did = "kernel", expr = quote(kernel == "poly")),
  addDep(
    ParamInt$new(id = "n_tree", lower = 1L, upper = 10L), 
    did = "model", expr = quote(model == "RF"))
)
pst$sample(1L)
##    model        C n_tree kernel  n    gamma
## 1:   SVM 78.83051     NA    rbf NA 89.18476
pst$sample(10L)
##     model        C n_tree kernel  n    gamma
##  1:    RF       NA      1   <NA> NA       NA
##  2:    RF       NA      9   <NA> NA       NA
##  3:    RF       NA      5   <NA> NA       NA
##  4:    RF       NA      5   <NA> NA       NA
##  5:    RF       NA      6   <NA> NA       NA
##  6:   SVM 89.98250     NA    rbf NA 4.248013
##  7:   SVM 95.45036     NA   poly  7       NA
##  8:    RF       NA     10   <NA> NA       NA
##  9:    RF       NA      8   <NA> NA       NA
## 10:    RF       NA      6   <NA> NA       NA
```


## Machine learning pipeline specification 

```r
pst = ParamSetTree$new("pre",     
  ParamCategorical$new(id = "preprocessing", values = c("PCA", "FeatureFiltering")),
  addDep(
    ParamInt$new(id = "pca.k", lower = 1, upper = 5), 
    did = "preprocessing", expr = quote(preprocessing == "PCA")),
 addDep(
   ParamInt$new(id = "filter.n", lower = 1, upper = 10), 
   did = "preprocessing", expr = quote(preprocessing == "FeatureFiltering"))
)

pst1 = ParamSetTree$new("ml",
  ParamCategorical$new(id = "model", values = c("SVM", "RF")),
  addDep(
    ParamReal$new(id = "C", lower = 0, upper = 100), 
    did = "model", expr = quote(model == "SVM")), # did here means dependant id
  addDep(
    ParamCategorical$new(id = "kernel", values = c("rbf", "poly")), 
    did = "model", expr = quote(model == "SVM")),
  addDep(
    ParamReal$new(id = "gamma", lower = 0, upper = 101), 
    did = "kernel", expr = quote(kernel == "rbf")),
  addDep(
    ParamInt$new(id = "poly.n", lower = 1L, upper = 10L), 
    did = "kernel", expr = quote(kernel == "poly")),
  addDep(
    ParamInt$new(id = "n_tree", lower = 1L, upper = 10L), 
    did = "model", expr = quote(model == "RF"))
)
pst$setChild(pst1)
pst$sample(4L)
##       preprocessing pca.k filter.n model        C n_tree kernel poly.n
## 1:              PCA     1       NA    RF       NA     10   <NA>     NA
## 2: FeatureFiltering    NA        8   SVM 47.77960     NA   poly      3
## 3:              PCA     2       NA   SVM 41.45463     NA    rbf     NA
## 4:              PCA     1       NA   SVM 46.59625     NA    rbf     NA
##       gamma
## 1:       NA
## 2:       NA
## 3: 37.25339
## 4: 86.64060
```

## Neural network architecture specification
Multiple Layers


```r
ps = ParamSetTreeRe$new("nn", nr = 2,
  ParamInt$new(id = "layer_dense.units", lower = 2L, upper = 1000L),
  ParamReal$new(id = "kernel_regularizer", lower = 0, upper = 3.0), 
  ParamReal$new(id = "bias_regularizer", lower = 0, upper = 3.0), 
  ParamCategorical$new(id = "reg_type", values = c("regularizer_l1", "regularizer_l2")),
  ParamCategorical$new(id = "activation_fun", values = c("sigmoid", "tanh", "linear")))
ps$sample(3L)
##    L0.bias_regularizer    L0.reg_type L0.layer_dense.units
## 1:           0.1374935 regularizer_l1                  800
## 2:           1.3455490 regularizer_l2                  813
## 3:           0.7308584 regularizer_l2                  419
##    L0.kernel_regularizer L0.activation_fun L1.bias_regularizer
## 1:             0.3656978              tanh           0.6195942
## 2:             2.3830270              tanh           2.2634255
## 3:             2.3645875           sigmoid           1.3046782
##       L1.reg_type L1.layer_dense.units L1.kernel_regularizer
## 1: regularizer_l1                  754            2.68513608
## 2: regularizer_l2                  711            0.00187432
## 3: regularizer_l2                  894            2.65940718
##    L1.activation_fun L2.bias_regularizer    L2.reg_type
## 1:              tanh           1.9953456 regularizer_l1
## 2:              tanh           0.6603567 regularizer_l1
## 3:           sigmoid           0.3920871 regularizer_l2
##    L2.layer_dense.units L2.kernel_regularizer L2.activation_fun
## 1:                  385             0.8231509            linear
## 2:                  614             1.0553937           sigmoid
## 3:                  345             1.9702744           sigmoid
```


```r
library(keras)
ps$sample()
list.par.val = ps$sampleList(flat = FALSE)
tex = keras_helper(input.shape = 256, output.shape = 10L, 
  output.act = "softmax", loss = "mse", 
  lr = 0.0025, list.par.val = list.par.val)
eval(parse(text = tex))
```
