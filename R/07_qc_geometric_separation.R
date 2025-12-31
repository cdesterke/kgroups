#' Geometric QC: separation vs compactness across k
#'
#' This function computes the geometric quality-control metric
#' BetweenSS / TotalSS for a range of k values in k-means clustering.
#' It returns both the data frame and a ggplot2 visualization.
#'
#' @param mat A numeric matrix or data frame (samples in rows).
#' @param k_range A vector of k values to evaluate (default: 2:10).
#' @param optimal_k Optional value of k to highlight with a vertical line.
#' @param nstart Number of random initializations for k-means (default: 20).
#' @param palette Color for the line and points (default: "steelblue").
#' @param base_size Base font size for the plot (default: 14).
#' @param main Title of the plot.
#'
#' @return A list containing:
#' \describe{
#'   \item{df}{Data frame with k, betweenSS, totalSS, ratio.}
#'   \item{plot}{A ggplot2 object.}
#' }
#'
#' @importFrom stats kmeans
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_vline labs theme_minimal
#'
#' @examples
#' \dontrun{
#'   mat_scaled <- scale(t(mat))
#'   qc <- qc_geometric_separation(mat_scaled, k_range = 2:10, optimal_k = 3)
#'   qc$plot
#' }
#'
#' @export
qc_geometric_separation <- function(mat,
                                    k_range = 2:10,
                                    optimal_k = NULL,
                                    nstart = 20,
                                    palette = "steelblue",
                                    base_size = 14,
                                    main = "Proportion of explained variance by cluster") {
  
  stopifnot(is.matrix(mat) || is.data.frame(mat))
  stopifnot(all(k_range >= 2))
  
  # ------------------------------------------------------------
  # Compute BetweenSS / TotalSS for each k
  # ------------------------------------------------------------
  bw_df <- do.call(rbind, lapply(k_range, function(k) {
    tmp <- stats::kmeans(mat, centers = k, nstart = nstart)
    data.frame(
      k = k,
      between = tmp$betweenss,
      total = tmp$totss,
      ratio = tmp$betweenss / tmp$totss
    )
  }))
  
  # ------------------------------------------------------------
  # Plot
  # ------------------------------------------------------------
  p <- ggplot2::ggplot(bw_df, ggplot2::aes(k, ratio)) +
    ggplot2::geom_line(color = palette, linewidth = 1) +
    ggplot2::geom_point(color = palette, size = 2) +
    ggplot2::labs(
      x = "k",
      y = "BetweenSS / TotalSS",
      title = main
    ) +
    ggplot2::theme_minimal(base_size = base_size)
  
  if (!is.null(optimal_k)) {
    p <- p + ggplot2::geom_vline(
      xintercept = optimal_k,
      linetype = "dashed",
      color = "red"
    )
  }
  
  # ------------------------------------------------------------
  # Return
  # ------------------------------------------------------------
  list(
    df = bw_df,
    plot = p
  )
}
