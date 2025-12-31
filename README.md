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


## kmeans optimized
```r
res <- run_kmeans_with_pca(mat_scaled, k = 3, palette = "Set1", base_size = 16)
res$pca_plot
res$clusters
```

![res](https://github.com/cdesterke/kgroups/blob/main/04_run_kmeans_with_pca.png)

## heatmap with unsupervised clustering
```r

heatmap_kmeans(mat_scaled, res$clusters)
```

![res](https://github.com/cdesterke/kgroups/blob/main/05_heatmap_kmeans.png)


## QC silhouette for optimal k
```r
km <- kmeans(mat_scaled, centers = 3, nstart = 50)
silhouette_kmeans(mat_scaled, km$cluster, palette = "Set1", base_size = 16)
```

![res](https://github.com/cdesterke/kgroups/blob/main/06_silhouette_kmeans.png)


## QC geometric separation
```r
qc <- qc_geometric_separation(mat_scaled, k_range = 2:10, optimal_k = 3)
qc$plot
```

![res](https://github.com/cdesterke/kgroups/blob/main/07_qc_geometric_separation.png)
