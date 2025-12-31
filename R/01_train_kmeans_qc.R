#' Train a k-means model with multi-criteria QC (Elbow, Silhouette, Gap)
#'
#' This function performs a complete k-means clustering workflow including:
#' data preprocessing, three independent quality-control criteria (Elbow, 
#' Silhouette, Gap Statistic), automatic extraction of the optimal number 
#' of clusters, majority voting across QC metrics, and final k-means training.
#'
#' The function returns both the QC metrics and the final clustering model,
#' along with the diagnostic plots used to assess cluster stability.
#'
#' @param mat A numeric matrix or data frame with features in rows and samples in columns.
#' @param k.max Maximum number of clusters to evaluate (default: 10).
#' @param nstart Number of random initializations for k-means (default: 25).
#' @param B Number of bootstrap samples for the Gap Statistic (default: 100).
#' @param seed Random seed for reproducibility (default: 123).
#'
#' @details
#' The function applies the following steps:
#' \itemize{
#'   \item Transposition of the matrix so that samples are in rows.
#'   \item Standardization using \code{scale()}.
#'   \item Elbow method using total within-cluster sum of squares.
#'   \item Silhouette method using average silhouette width.
#'   \item Gap Statistic using bootstrap resampling.
#'   \item Automatic extraction of the best \code{k} for each QC method.
#'   \item Majority vote to determine the final optimal number of clusters.
#'   \item Final k-means training with the selected \code{k}.
#' }
#'
#' @return A list containing:
#' \describe{
#'   \item{k_elbow}{Optimal k from the Elbow method.}
#'   \item{k_silhouette}{Optimal k from the Silhouette method.}
#'   \item{k_gap}{Optimal k from the Gap Statistic.}
#'   \item{optimal_k}{Final optimal k (majority vote).}
#'   \item{plots}{A combined patchwork object with the three QC plots.}
#'   \item{kmeans_model}{The final k-means model.}
#'   \item{clusters}{Cluster assignments for each sample.}
#' }
#'
#' @importFrom stats kmeans
#' @importFrom cluster clusGap
#' @importFrom factoextra fviz_nbclust fviz_gap_stat
#' @importFrom dplyr filter pull
#' @importFrom patchwork wrap_plots
#'
#' @examples
#' \dontrun{
#'   set.seed(123)
#'   mat <- matrix(rnorm(1000), nrow = 100, ncol = 10)
#'   res <- train_kmeans_qc(mat)
#'   res$optimal_k
#'   res$clusters
#'   res$plots
#' }
#'
#' @export


train_kmeans_qc <- function(mat, k.max = 10, nstart = 25, B = 100, seed = 123) {
  require(factoextra)
  require(cluster)
  require(dplyr)
  require(patchwork)
  
  # ------------------------------------------------------------
  # Préparation des données
  # ------------------------------------------------------------
  mat_t <- t(mat)
  mat_scaled <- scale(mat_t)
  
  # ------------------------------------------------------------
  # QC 1 : Elbow (WSS)
  # ------------------------------------------------------------
  p_elbow <- fviz_nbclust(mat_scaled, kmeans, method = "wss", k.max = k.max) +
    ggtitle("Elbow")
  
  # Extraction automatique du elbow
  wss <- vapply(1:k.max, function(k) {
    kmeans(mat_scaled, centers = k, nstart = nstart)$tot.withinss
  }, numeric(1))
  
  d2 <- diff(wss, differences = 2)
  k_elbow <- which.max(d2) + 1
  
  # ------------------------------------------------------------
  # QC 2 : Silhouette
  # ------------------------------------------------------------
  sil <- fviz_nbclust(mat_scaled, kmeans, method = "silhouette", k.max = k.max)
  p_sil <- sil + ggtitle("Silhouette")
  
  k_sil <- sil$data %>%
    filter(y == max(y)) %>%
    pull(clusters)
  
  # ------------------------------------------------------------
  # QC 3 : Gap Statistic
  # ------------------------------------------------------------
  set.seed(seed)
  gap_stat <- clusGap(mat_scaled, FUN = kmeans, nstart = nstart,
                      K.max = k.max, B = B)
  
  p_gap <- fviz_gap_stat(gap_stat) + ggtitle("Gap Statistic")
  
  k_gap <- maxSE(gap_stat$Tab[,"gap"],
                 gap_stat$Tab[,"SE.sim"],
                 method = "firstSEmax")
  
  # ------------------------------------------------------------
  # Vote majoritaire
  # ------------------------------------------------------------
  k_values <- c(k_elbow, k_sil, k_gap)
  optimal_k <- as.numeric(names(sort(table(k_values), decreasing = TRUE))[1])
  
  # ------------------------------------------------------------
  # Entrainement final du kmeans
  # ------------------------------------------------------------
  set.seed(seed)
  km_final <- kmeans(mat_scaled, centers = optimal_k, nstart = nstart)
  
  # ------------------------------------------------------------
  # Retour
  # ------------------------------------------------------------
  list(
    k_elbow = k_elbow,
    k_silhouette = k_sil,
    k_gap = k_gap,
    optimal_k = optimal_k,
    plots = p_elbow + p_sil + p_gap,
    kmeans_model = km_final,
    clusters = km_final$cluster
  )
}


