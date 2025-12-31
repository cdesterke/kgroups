#' QC: Mean sample profiles per k-means cluster (with shortened feature names)
#'
#' This function computes the mean profile of each cluster across all features
#' and visualizes them as line plots. Feature names can be automatically
#' shortened for readability on the x-axis.
#'
#' @param mat A numeric matrix or data frame (samples in rows).
#' @param clusters A vector of cluster assignments (same order as rownames(mat)).
#' @param palette A color palette name or vector of colors (default: "Dark2").
#' @param base_size Base font size for the plot (default: 14).
#' @param feature_abbrev Integer: number of characters to keep when shortening
#'   feature names (default: 10). Longer names are truncated and suffixed with "…".
#' @param main Title of the plot.
#'
#' @return A ggplot2 object showing mean profiles per cluster.
#'
#' @importFrom dplyr group_by summarise across
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_line theme_minimal theme element_text ggtitle
#' @importFrom ggplot2 scale_color_brewer
#'
#' @examples
#' \dontrun{
#'   mat_scaled <- scale(t(mat))
#'   km <- kmeans(mat_scaled, centers = 3)
#'   qc_sample_profiles(mat_scaled, km$cluster, feature_abbrev = 16)
#' }
#'
#' @export
qc_sample_profiles <- function(mat,
                               clusters,
                               palette = "Dark2",
                               base_size = 14,
                               feature_abbrev = 10,
                               main = "Sample profiles by cluster (k-means)") {
  
  stopifnot(is.matrix(mat) || is.data.frame(mat))
  stopifnot(length(clusters) == nrow(mat))
  stopifnot(feature_abbrev >= 1)
  
  # ------------------------------------------------------------
  # Build dataframe with cluster annotation
  # ------------------------------------------------------------
  df <- as.data.frame(mat)
  df$Cluster <- factor(clusters)
  
  # ------------------------------------------------------------
  # Compute mean profiles per cluster
  # ------------------------------------------------------------
  mean_profiles <- df %>%
    dplyr::group_by(Cluster) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), mean))
  
  # ------------------------------------------------------------
  # Long format
  # ------------------------------------------------------------
  mean_long <- mean_profiles %>%
    tidyr::pivot_longer(
      cols = -Cluster,
      names_to = "Feature",
      values_to = "Score"
    )
  
  # ------------------------------------------------------------
  # Shorten feature names
  # ------------------------------------------------------------
  mean_long$Feature_short <- substr(mean_long$Feature, 1, feature_abbrev)
  mean_long$Feature_short <- ifelse(
    nchar(mean_long$Feature) > feature_abbrev,
    paste0(mean_long$Feature_short, "…"),
    mean_long$Feature_short
  )
  
  # ------------------------------------------------------------
  # Plot
  # ------------------------------------------------------------
  p <- ggplot2::ggplot(
    mean_long,
    ggplot2::aes(Feature_short, Score, color = Cluster, group = Cluster)
  ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)
    ) +
    ggplot2::ggtitle(main) +
    ggplot2::scale_color_brewer(palette = palette)
  
  return(p)
}

