# kgroups
R package to manage kmeans classifier on a matrix of data



## code to install kgroups package
```r
library(devtools)
install_github("cdesterke/kgroups")
```

## Optimal k for number of clusters

```r
library(kgroups)
data(mat)
res <- train_kmeans_qc(mat)
res$optimal_k
res$clusters
res$plots
```

![res](https://github.com/cdesterke/kgroups/blob/main/01_train_kmeans_qc.png)


## bootstrap at optimal k number

```r
mat_scaled <- scale(t(mat))
res <- bootstrap_stability_kmeans(mat_scaled, k = 3)
res$plot
res
```

![res](https://github.com/cdesterke/kgroups/blob/main/02_bootstrap_stability_kmeans.png)


## stability of nstart parameter of kmeans fonction
```r
qc_nstart(mat_scaled, k = 3)
```

![res](https://github.com/cdesterke/kgroups/blob/main/03_qc_nstart.png)
