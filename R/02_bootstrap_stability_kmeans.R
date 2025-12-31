#' Bootstrap stability of k-means clusters (Jaccard index)
#'
#' This function computes the bootstrap stability of k-means clusters using
#' the Jaccard index from the \code{clusterboot()} function (package fpc).
#' It returns both the stability values and a ggplot2 barplot.
#'
#' @param mat A numeric matrix or data frame (samples in rows).
#' @param k Number of clusters (same as used in k-means).
#' @param B Number of bootstrap resamples (default: 100).
#' @param palette A color for the bars (default: "darkorange").
#' @param base_size Base font size for the plot (default: 14).
#' @param main Title of the plot.
#'
#' @return A list containing:
#' \describe{
#'   \item{jaccard}{Vector of Jaccard stability indices.}
#'   \item{plot}{A ggplot2 barplot of cluster stability.}
#'   \item{clusterboot_object}{The full clusterboot() output.}
#' }
#'
#' @importFrom fpc clusterboot kmeansCBI
#' @importFrom ggplot2 ggplot aes geom_bar geom_hline ylim theme_minimal ggtitle
#'
#' @examples
#' \dontrun{
#'   mat_scaled <- scale(t(mat))
#'   res <- bootstrap_stability_kmeans(mat_scaled, k = 3)
#'   res$plot
#' }
#'
#' @export
bootstrap_stability_kmeans <- function(mat,
                                       k,
                                       B = 100,
                                       palette = "darkorange",
                                       base_size = 14,
                                       main = NULL) {
  
  stopifnot(is.matrix(mat) || is.data.frame(mat))
  stopifnot(k >= 2)
  
  if (is.null(main)) {
    main <- paste("Bootstrap stability of clusters (k =", k, ")")
  }
  
  # ------------------------------------------------------------
  # Bootstrap stability via clusterboot()
  # ------------------------------------------------------------
  set.seed(123)
  cb <- fpc::clusterboot(
    mat,
    B = B,
    bootmethod = "boot",
    clustermethod = fpc::kmeansCBI,
    krange = k,
    seed = 123,
    count = FALSE
  )
  
  jaccard <- cb$bootmean
  
  # ------------------------------------------------------------
  # Data frame for plotting
  # ------------------------------------------------------------
  stab_df <- data.frame(
    Cluster = factor(seq_along(jaccard)),
    Jaccard = jaccard
  )
  
  # ------------------------------------------------------------
  # Plot
  # ------------------------------------------------------------
  p <- ggplot2::ggplot(stab_df, ggplot2::aes(Cluster, Jaccard)) +
    ggplot2::geom_bar(stat = "identity", fill = palette) +
    ggplot2::geom_hline(
      yintercept = c(0.6, 0.75, 0.85),
      linetype = "dashed",
      color = "grey50"
    ) +
    ggplot2::ylim(0, 1) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::ggtitle(main)
  
  # ------------------------------------------------------------
  # Return
  # ------------------------------------------------------------
  list(
    jaccard = jaccard,
    plot = p,
    clusterboot_object = cb
  )
}
