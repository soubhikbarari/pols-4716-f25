library(tidyverse)

# This helper function runs PCA on selected variables and returns the original
# dataset with new principal component scores (PC1, PC2, ...) added.
#
# Arguments:
# - data: A data frame
# - vars: Character vector of variable names to include in PCA
# - num_comps: Number of components to keep (default = 3)
# - scale_vars: Whether to standardize variables before PCA (default = TRUE)
#
# Output:
# - Returns original data + PC columns
# - Also attaches:
#   - attr(out, "pca_var_expl") gives variance explained of each of the desired components
#   - attr(out, "pca_model") gives the prcomp object
discover_principal_components <- function(data, 
                                          vars, 
                                          num_comps = 3, 
                                          scale_vars = TRUE) {
  # Add ID to join back later
  data <- data %>% mutate(.row_id = row_number())
  
  # Select and clean variables for PCA
  X <- data %>%
    select(.row_id, all_of(vars)) %>%
    drop_na() %>%
    select(where(~ n_distinct(.) > 1))
  
  # Run PCA
  pca <- prcomp(select(X, - .row_id), scale. = scale_vars)
  
  # Get scores for top components
  pc_scores <- as.data.frame(pca$x[, 1:num_comps])
  colnames(pc_scores) <- paste0("PC", 1:num_comps)
  X <- bind_cols(.row_id = X$.row_id, pc_scores)
  
  # Attach variance explained as attribute
  attr(X, "pca_var_expl") <- round(100 * summary(pca)$importance[2, 1:num_comps], 1)
  
  # Merge scores back to original data
  out <- data %>% 
    left_join(X, by = ".row_id") %>%
    select(-.row_id)
  
  # Also attach PCA model/inputs as attribute
  attr(out, "pca_model") <- pca
  attr(out, "pca_vars") <- vars
  
  return(out)
}


# This helper function runs **k-means clustering** on selected variables and
# returns the original dataset with a new column, `cluster`, indicating each
# observation's assigned cluster.
#
# Arguments:
# - data: A data frame.
# - vars: Character vector of variable names to use for clustering.
# - k: Number of clusters to form (default = 3).
# - seed: Random seed for reproducibility (default = 123).
#
# Output:
# - Returns the original dataset with an additional column:
#   - `cluster`: factor variable indicating cluster assignment.
# - Also attaches:
#   - `attr(out, "kmeans_model")`: the fitted k-means object.
#   - `attr(out, "kmeans_vars")`: variables used in clustering.
#   - `attr(out, "kmeans_tot_withinss")`: total within-cluster sum of squares.
#   - `attr(out, "kmeans_betweenss")`: between-cluster sum of squares.
#   - `attr(out, "kmeans_totss")`: total sum of squares.
#
# Notes:
# - K-means is sensitive to scaling — this function standardizes variables automatically.
# - Choosing `k` is part of the analytical judgment. Use tools like an elbow plot to guide your decision.
# - Cluster labels are arbitrary (1, 2, 3, ...); interpret them based on patterns in the data.
discover_kmeans_clusters <- function(data, vars, k = 3, seed = 123) {
  set.seed(seed)
  
  # Add ID to join back later
  data <- data %>% mutate(.row_id = row_number())
  
  # Select and clean variables
  X <- data %>%
    select(.row_id, all_of(vars)) %>%
    drop_na() %>%
    select(where(~ n_distinct(.) > 1))
  
  # Standardize variables
  X_std <- scale(select(X, - .row_id))
  
  # Run K-means clustering
  km <- kmeans(X_std, centers = k, nstart = 25)
  
  # Prepare cluster assignments
  clusters <- tibble(
    .row_id = X$.row_id,
    cluster = factor(km$cluster)
  )
  
  # Merge clusters back into original data
  out <- data %>%
    left_join(clusters, by = ".row_id") %>%
    select(-.row_id)
  
  # Attach some useful info to the output
  attr(out, "kmeans_model") <- km
  attr(out, "kmeans_vars") <- vars
  attr(out, "kmeans_tot_withinss") <- km$tot.withinss
  attr(out, "kmeans_betweenss") <- km$betweenss
  attr(out, "kmeans_totss") <- km$totss
  
  return(out)
}


# This helper function **visualizes the results of a PCA** in a 2D scatterplot
# using the first two principal components (PC1 and PC2). It can also optionally
# overlay cluster assignments and loading vectors for interpretation.
#
# Arguments:
# - data: A data frame returned by `discover_principal_components()`.
# - labels: (optional) Name of a column to use as text labels for points.
# - with_clusters: If TRUE, colors points by cluster (requires a `cluster` column).
# - with_loadings: If TRUE, adds top loading vectors as arrows and labels.
# - top_n_loadings: Number of loading vectors to display (default = 8).
plot_pca_2d <- function(data, 
                        labels = NULL, 
                        with_clusters = FALSE, 
                        with_loadings = FALSE, 
                        top_n_loadings = 8) {
  # --- Extract PCA info ---
  pca <- attr(data, "pca_model")
  vars <- attr(data, "pca_vars")
  var_expl <- round(100 * summary(pca)$importance[2, 1:2], 1)
  
  if (!is.null(labels) && labels %in% colnames(data)) {
    data$lab <- data[[labels]]
  }
  
  # --- Build base layer ---
  p <- ggplot(data, aes(x = PC1, y = PC2)) +
    theme_minimal() +
    theme(legend.position = "top") +
    labs(
      title = "PCA Visualization",
      subtitle = paste0("PC1: ", var_expl[1], "%, PC2: ", var_expl[2], "% variance explained"),
      x = paste0("PC1 (", var_expl[1], "%)"),
      y = paste0("PC2 (", var_expl[2], "%)")
    )
  
  if (!is.null(labels) && labels %in% colnames(data)) {
    p <- p + 
      geom_text(aes(label = lab), size = 3, alpha = 0.8)
  }
  
  # --- Add clusters if requested ---
  if (with_clusters && "cluster" %in% colnames(data)) {
    p <- p + geom_point(aes(color = cluster), alpha = 0.7) +
      scale_color_brewer(palette = "Set2") +
      labs(color = "Cluster")
  } else {
    p <- p + geom_point(color = "grey", shape = 21)
  }
  
  # --- Add loadings if requested ---
  if (with_loadings) {
    loadings <- as.data.frame(pca$rotation[, 1:2]) %>%
      rownames_to_column("Variable") %>%
      mutate(Length = sqrt(PC1^2 + PC2^2)) %>%
      slice_max(order_by = Length, n = top_n_loadings)
    
    # Get labels from attr()
    label_map <- map_chr(loadings$Variable, function(v) {
      lbl <- attr(data[[v]], "label")
      if (is.null(lbl) || lbl == "") v else lbl
    })
    loadings$Label <- label_map
    
    # Scale arrows to match data spread
    max_score <- max(abs(c(data$PC1, data$PC2)), na.rm = TRUE)
    loadings <- loadings %>%
      mutate(
        PC1_scaled = PC1 * max_score,
        PC2_scaled = PC2 * max_score#,
        # angle = atan2(PC2_scaled, PC1_scaled) * 180 / pi,
        # angle = ifelse(angle > 90 | angle < -90, angle + 180, angle)
      )
    
    p <- p +
      geom_segment(
        data = loadings,
        aes(x = 0, y = 0, xend = PC1_scaled, yend = PC2_scaled),
        arrow = arrow(length = unit(0.25, "cm")),
        color = "gray30"
      ) +
      geom_label(
        data = loadings,
        # aes(x = PC1_scaled, y = PC2_scaled, label = Label, angle = angle),
        aes(x = PC1_scaled, y = PC2_scaled, label = Label),
        hjust = 0.5, vjust = -0.5, size = 3
      )
  }
  
  return(p)
}

# This helper function visualizes the **top variable loadings** for each
# principal component, showing which variables contribute most strongly to each
# component and in which direction (positive or negative).
#
# Arguments:
# - data: A data frame returned by `discover_principal_components()`.
# - num_comps: Number of principal components to plot (default = 2).
# - top_num_vars: Number of top variables (by absolute loading) to display per component (default = 10).
# - scale_vars: Whether variables were scaled before PCA (kept for compatibility; not used directly in plotting).
plot_pca_loadings <- function(data, 
                              num_comps = 2, 
                              top_num_vars = 10, 
                              scale_vars = TRUE) {
  # --- Extract PCA info ---
  pca <- attr(data, "pca_model")
  vars <- attr(data, "pca_vars")
  var_expl <- round(100 * summary(pca)$importance[2, 1:2], 1)

  # --- Extract labels from attr() ---
  label_map <- map_chr(vars, function(v) {
    lbl <- paste0("[",v,"] ", attr(data[[v]], "label"))
    if (is.null(lbl) || lbl == "") v else lbl
  }) %>% set_names(vars)
  
  # --- Loadings ---
  loadings <- as.data.frame(pca$rotation) %>%
    rownames_to_column("Variable") %>%
    pivot_longer(
      cols = starts_with("PC"),
      names_to = "PC",
      values_to = "Loading"
    ) %>%
    mutate(
      abs_loading = abs(Loading),
      Label = ifelse(Variable %in% names(label_map),
                     label_map[Variable],
                     Variable)
    ) %>%
    filter(PC %in% paste0("PC", 1:num_comps)) %>%
    group_by(PC) %>%
    slice_max(order_by = abs_loading, n = top_num_vars, with_ties = FALSE) %>%
    ungroup()
  
  # --- Sort labels within each facet ---
  loadings <- loadings %>%
    group_by(PC) %>%
    mutate(Label = fct_reorder(Label, abs_loading, .desc = TRUE)) %>%
    ungroup()
  
  # --- Facet labels with % variance explained ---
  facet_labels <- paste0("PC", 1:num_comps, " (", var_expl, "%)")
  names(facet_labels) <- paste0("PC", 1:num_comps)
  
  # --- Plot ---
  ggplot(loadings, aes(x = Label, y = Loading, fill = Loading > 0)) +
    geom_col() +
    coord_flip() +
    facet_grid(PC ~ ., scales = "free", labeller = labeller(PC = facet_labels)) +
    scale_fill_manual(values = c("TRUE" = "#2b83ba", "FALSE" = "#d7191c"), guide = "none") +
    theme_bw() +
    theme(legend.position = "top") +
    labs(
      title = "Top PCA Loadings by Component",
      x = "Variable",
      y = "Loading"
    )
}

# This helper function creates an **elbow plot** to help decide on the appropriate
# number of clusters (k) when using k-means. It shows how the total within-cluster
# sum of squares (a measure of compactness) decreases as k increases.
#
# Arguments:
# - data: A data frame (often with principal components).
# - vars: Character vector of variable names to use for clustering (e.g., `c("PC1", "PC2")`).
# - k_max: Maximum number of clusters to evaluate (default = 10).
# - scale_vars: Whether to standardize variables before clustering (default = TRUE).
# - seed: Random seed for reproducibility (default = 123).
#
# Output:
# - A `ggplot` line plot with:
#   - x-axis: number of clusters (k),
#   - y-axis: total within-cluster sum of squares.
# - The **“elbow”** is the point where the curve begins to flatten, suggesting a
#   good candidate for k.
#
# Notes:
# - K-means tends to show rapid improvement up to the “right” number of clusters,
#   then flatten out — similar to diminishing returns.
# - The elbow plot is a **heuristic**, not a strict rule — use it as a guide.
plot_kmeans_elbow <- function(data, vars, k_max = 10, scale_vars = TRUE, seed = 123) {
  set.seed(seed)
  
  # Select and clean variables
  X <- data %>%
    select(all_of(vars)) %>%
    drop_na() %>%
    select(where(~ n_distinct(.) > 1))
  
  # Standardize if needed
  if (scale_vars) {
    X <- scale(X)
  }
  
  # Compute total within-cluster sum of squares for each k
  elbow <- tibble(
    k = 1:k_max,
    tot_withinss = map_dbl(1:k_max, ~ kmeans(X, centers = .x, nstart = 25)$tot.withinss)
  )
  
  # Plot elbow
  ggplot(elbow, aes(x = k, y = tot_withinss)) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    labs(
      title = "K-means Elbow Plot",
      x = "Number of clusters (k)",
      y = "Total within-cluster sum of squares"
    )
}