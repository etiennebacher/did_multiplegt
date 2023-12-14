did_multiplegt_preprocess <- function(
    df,
    Y,
    G,
    T,
    D,
    controls,
    placebo,
    dynamic,
    recat_treatment,
    trends_nonparam,
    trends_lin,
    cluster
) {

  cluster_column <- if (!is.null(cluster)) {
    df[[cluster]]
  } else {
    NULL
  }
  trends <- if (!is.null(trends_nonparam)) {
    trends_nonparam
  } else if (!is.null(trends_lin)) {
    trends_lin
  } else {
    NULL
  }

  df <- did_multiplegt_rename_var(df, Y, G, T, D, controls, recat_treatment, trends)
  df <- did_multiplegt_transform(df, controls, !is.null(trends_nonparam))

  if (placebo > 0) {
    for (counter_placebo in 1:placebo) {
      df <- placebo_transform_level(df, counter_placebo, controls)
    }
    df$placebo_cond <- TRUE
  }

  if (dynamic > 0) {
    for (counter_dynamic in 1:dynamic) {
      df <- dynamic_transform_level(df, counter_dynamic, controls)
    }
    df$dynamic_cond <- TRUE
  }

  if (!is.null(cluster)) {
    df$cluster <- cluster_column
  }
  df
}
