# kgroups
R package to manage kmeans classifier on a matrix of data



## code to install kgroups package
```r
library(devtools)
install_github("cdesterke/kgroups")
```

## Over representation analysis

```r
library(kgroups)
data(mat)
res <- train_kmeans_qc(mat)
res$optimal_k
res$clusters
res$plots
```

![res](https://github.com/cdesterke/kgroups/blob/main/01_train_kmeans_qc.png)
