plot_base <- function() {

  ggplot() +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "#444444", color = NA),  # Dark panel
      plot.background = element_rect(fill = "#2A2A2A", color = NA),  # Dark background
      panel.grid.major = element_line(color = "#4C4C4C"),  # Darker grid
      panel.grid.minor = element_line(color = "#333333"),
      axis.ticks = element_line(color = "white"),
      axis.text = element_text(color = "white"),
      axis.title = element_text(color = "white"),
      strip.text = element_text(color = "white"),
      plot.title = element_text(color = "white", face = "bold"),
      legend.background = element_rect(fill = "#444444"),
      legend.text = element_text(color = "white"),
      legend.title = element_text(color = "white"))
}

get_LL_plot <- function(df) {

  df |>
    ggplot(aes(x = psi, y = .data[[names(df)[2]]])) +
    geom_point(color = "cyan", size = 3, alpha = 0.7) +
    theme_minimal(base_size = 15) +  # Minimal theme with a larger base font size
    theme(
      plot.background = element_rect(fill = "#2E2E2E", color = NA),  # Dark background for the whole plot
      panel.background = element_rect(fill = "#3A3A3F", color = "#1A1A1A", linewidth = 2),  # Lighter panel with a border
      axis.text = element_text(color = "white"),  # White axis labels
      axis.title = element_text(color = "white"),  # White axis titles
      plot.title = element_text(color = "white", size = 18, face = "bold"),  # White title
      plot.caption = element_text(color = "gray", size = 10),  # Gray caption
      panel.grid = element_line(color = "gray30", linetype = "dashed")  # Subtle grid lines
    ) +
    labs(
      title = paste(names(df)[[2]], "Log-Likelihood"),
      x = "\u03C8",
      y = expression("log L("*psi*")")
    )
}

get_branches_plot <- function(mat) {

  df <- as.data.frame(mat)
  df$CurveID <- paste0("Curve_", seq_len(nrow(df)))

  df_long <- df |>
    pivot_longer(-CurveID, names_to = "X", values_to = "Y") |>
    mutate(X = as.numeric(X))

  plot_base() +
    theme(legend.position = "none") +
    geom_line(data = df_long, aes(x = X, y = Y, group = CurveID, color = CurveID), linewidth = 1) +
    labs(title = "Integrated Log-Likelihood Branches",
         x = "\u03C8",
         y = expression("log L("*psi*")"))
}

get_branches_plot_shifted <- function(mat, crit) {
  # shift each branch so its max is 0
  mat_shifted <- t(apply(mat, 1, function(row) row - max(row, na.rm = TRUE)))

  psi_vals <- as.numeric(colnames(mat_shifted))

  # Make a smaller grid for x-axis labels
  psi_grid <- seq(min(psi_vals, na.rm = TRUE),
                  max(psi_vals, na.rm = TRUE),
                  length.out = 10)

  df <- as.data.frame(mat_shifted)
  df$CurveID <- paste0("Curve_", seq_len(nrow(df)))

  df_long <- df |>
    tidyr::pivot_longer(-CurveID, names_to = "X", values_to = "Y")
  df_long$X <- as.numeric(df_long$X)

  plot_base() +
    theme(legend.position = "none") +
    geom_line(
      data = df_long,
      aes(x = X, y = Y, group = CurveID, color = CurveID),
      linewidth = 1
    ) +
    geom_hline(yintercept = -crit, color = "red") +
    scale_x_continuous(breaks = psi_grid, labels = round(psi_grid, 2)) +
    labs(
      title = "Integrated Log-Likelihood Branches (shifted)",
      x = "\u03C8",
      y = expression("log L("*psi*")")
    )
}

get_LL_comparison_plot <- function(stat_fns,
                                   LL_df_long,
                                   MLE_data,
                                   alpha_levels,
                                   psi_0) {

  c(stat_fn_IL, stat_fn_PL) %<-% stat_fns

  crit_df <- tibble(
    alpha = alpha_levels,
    crit = qchisq(1 - alpha_levels, df = 1) / 2,
    label = paste0(100 * (1 - alpha_levels), "% CI"),
    color = RColorBrewer::brewer.pal(length(alpha_levels), "Dark2")
  )

  palette_colors <- c("Integrated" = "#E41A1C",
                      "Profile" = "#377EB8",
                      "Truth" = "#4DAF4A") |>
    c(get_ci_colors(crit_df))

  psi_endpoints <- LL_df_long |>
    pull(psi) |>
    (\(x) c(head(x, 1), tail(x, 1)))()

  y_min <- -max(crit_df$crit) - 0.5

  label_data <- MLE_data |>
    add_row(Source = "Truth",
            MLE = psi_0,
            MLE_label = "psi[0]")

  plot_base() +
    stat_fn_IL +
    stat_fn_PL +

    # Baseline
    geom_hline(yintercept = 0, linetype = 5) +

    # Confidence interval lines (not mapped to color)
    geom_hline(data = crit_df,
               aes(yintercept = -crit, color = label),
               linewidth = 1.2,
               show.legend = TRUE) +

    # Vertical lines at MLEs and Truth
    geom_vline(
      aes(xintercept = MLE, color = Source),
      data = label_data,
      show.legend = FALSE
    ) +

    # Repelled MLE/truth labels
    geom_label_repel(
      aes(x = MLE, y = y_min / 2, label = MLE_label, color = Source),
      data = label_data,
      direction = "y",
      parse = TRUE,
      show.legend = FALSE,
      seed = 7835
    ) +

    # Log-likelihood points
    geom_point(
      aes(x = psi, y = value - max(value, na.rm = TRUE)),
      data = LL_df_long |> filter(pseudolikelihood == "Integrated"),
      size = 1
    ) +

    geom_point(
      aes(x = psi, y = value - max(value, na.rm = TRUE)),
      data = LL_df_long |> filter(pseudolikelihood == "Profile"),
      size = 1
    ) +

    # Axes, labels
    ylab(expression("log L("*psi*")")) +
    xlab(expression(psi)) +
    ggtitle("Pseudo Log-Likelihood Comparison Plot") +
    scale_x_continuous(expand = c(0, 0), limits = psi_endpoints) +
    scale_y_continuous(expand = c(0, 0), limits = c(y_min, 0.1)) +
    scale_color_manual(name = NULL, values = palette_colors, breaks = crit_df$label) +
    theme(legend.position = "inside",
          legend.position.inside = c(1, 1),
          legend.justification = c(1, 1))
}




get_LL_df_long <- function(LL_df) {

  LL_df |>
    pivot_longer(cols = -psi,
                 names_to = "pseudolikelihood",
                 values_to = "value") |>
    mutate(pseudolikelihood = pseudolikelihood |>
             as_factor())
}

get_spline_models <- function(LL_df_long) {

  LL_df_long |>
    drop_na(value) |>
    group_by(pseudolikelihood) |>
    group_map(~ smooth.spline(.x$psi, .x$value)) |>
    set_names(levels(LL_df_long$pseudolikelihood))
}

get_MLE_data <- function(spline_models, LL_df_long) {

  spline_models |>
    imap(
      \(mod, pseudolikelihood) {
        optimize(\(psi) predict(mod, psi)$y,
                 lower = LL_df_long |>
                   filter(pseudolikelihood == pseudolikelihood) |>
                   select(psi) |>
                   min(),
                 upper = LL_df_long |>
                   filter(pseudolikelihood == pseudolikelihood) |>
                   select(psi) |>
                   max(),
                 maximum = TRUE) |>
          data.frame() |>
          mutate(MLE = as.numeric(maximum),
                 Maximum = as.numeric(objective)) |>
          select(MLE, Maximum)
      }) |>
    do.call(rbind, args = _) |>
    mutate(MLE_label = c("hat(psi)[IL]", "hat(psi)[PL]")) |>
    rownames_to_column("Source")
}

get_pseudolikelihoods <- function(spline_models, MLE_data) {

  spline_models |>
    map2(MLE_data$Maximum,
         \(mod, maximum) \(psi) predict(mod, psi)$y - maximum)
}

get_confidence_intervals <- function(pseudolikelihoods,
                                     LL_df_long,
                                     MLE_data,
                                     alpha_levels) {

  map2_dfr(
    pseudolikelihoods,
    names(pseudolikelihoods),
    \(pseudolikelihood, name) {

      MLE <- MLE_data |>
        filter(Source == name) |>
        pull(MLE)

      psi_endpoints <- LL_df_long |>
        filter(pseudolikelihood == name) |>
        drop_na() |>
        pull(psi) |>
        (\(x) c(head(x, 1), tail(x, 1)))()

      map_dfr(alpha_levels, \(alpha) {
        crit <- qchisq(1 - alpha, df = 1) / 2
        conf_label <- paste0(100 * (1 - alpha), "%")

        lower_bound <- tryCatch(
          uniroot(\(psi) pseudolikelihood(psi) + crit,
                  interval = c(psi_endpoints[1], MLE))$root,
          error = function(e) return(NA_real_)
        )

        upper_bound <- tryCatch(
          uniroot(\(psi) pseudolikelihood(psi) + crit,
                  interval = c(MLE, psi_endpoints[2]))$root,
          error = function(e) return(NA_real_)
        )

        tibble(
          pseudolikelihood = name,
          confidence = conf_label,
          alpha = alpha,
          lower = round(lower_bound, 3),
          upper = round(upper_bound, 3)
        )
      })
    }
  )
}

make_kable_caption <- function(path) {

  path_parts <- strsplit(path, "/")[[1]]

  experiment_version <- path_parts[grep("^exp_v", path_parts)]
  sim_num <- path_parts[grep("^sim_", path_parts)]
  iter_num <- path_parts[grep("^iter_", path_parts)]

  subtitle <- paste("Experiment", experiment_version,
                    "— Simulation", sim_num,
                    "— Iteration", iter_num)
  title = "Pseudolikelihood Estimate Comparison Tables"

  caption = paste0("<center><span style='font-size:100%'>",
                   title,
                   "</span><br><span style='font-size:50%'>",
                   subtitle,
                   "</span></center>")
}

render_LL_comparison_tables <- function(MLE_data, conf_ints, psi_0) {

  # Format MLE table
  mle_table <- MLE_data |>
    select(Source, MLE) |>
    mutate(MLE = sprintf("%.3f", MLE)) |>
    bind_rows(tibble(Source = "Truth", MLE = sprintf("%.3f", psi_0))) |>
    kbl(
      caption = "MLE Table",
      align = "c",
      escape = FALSE
    ) |>
    kable_material_dark(
      lightable_options = c("hover"),
      position = "center",
      font_size = 18
    ) |>
    row_spec(nrow(MLE_data) + 1, color = "green", bold = TRUE) |>
    column_spec(1:2, extra_css = "vertical-align:middle;")

  # Format Confidence Interval table
  ci_table <- conf_ints |>
    mutate(
      contains_truth = !is.na(lower) & !is.na(upper) & (lower <= psi_0 & upper >= psi_0),
      Status = ifelse(contains_truth, "✅", "❌"),
      Interval = sprintf("(%.3f, %.3f)", lower, upper),
      Length = ifelse(
        is.na(lower) | is.na(upper),
        "--",
        sprintf("%.3f", upper - lower)
      ),
      `Confidence Level` = confidence
    ) |>
    rename(Source = pseudolikelihood) |>
    select(`Confidence Level`, Source, Interval, Length, Status)


  # Add "Truth" rows for each confidence level
  confidence_levels <- unique(ci_table$`Confidence Level`)
  truth_rows <- tibble(
    `Confidence Level` = confidence_levels,
    Source = "Truth",
    Interval = sprintf("%.3f", psi_0),
    Length = "",
    Status = ""
  )

  ci_table <- bind_rows(ci_table, truth_rows) |>
    arrange(`Confidence Level`, Source)

  ci_kable <- ci_table |>
    kbl(
      caption = "Confidence Interval Table",
      escape = FALSE,
      align = "c"
    ) |>
    kable_material_dark(
      lightable_options  = c("hover"),
      position = "center",
      font_size = 18,
      html_font = "Arial"
    ) |>
    collapse_rows(columns = 1, valign = "middle") |>
    row_spec(which(ci_table$Source == "Truth"), bold = TRUE, color = "green") |>
    column_spec(1:4, extra_css = "vertical-align:middle;")

  list(MLEs = mle_table, CIs = ci_kable)
}

get_stat_fns <- function(pseudolikelihoods, LL_df) {

  psi_endpoints <- LL_df |>
    pull(psi) |>
    (\(x) c(head(x, 1), tail(x, 1)))()

  pseudolikelihoods |>
    imap(
      \(pseudolikelihood, name) {

        stat_function(fun = pseudolikelihood,
                      geom = "line",
                      aes(color = name),
                      linewidth = 1.5,
                      show.legend = FALSE,
                      xlim = psi_endpoints)
      }
    )
}

get_ci_colors <- function(crit_df) {
  labels <- crit_df$label
  n <- length(labels)

  # Get 'n' colors from the Dark2 palette
  colors <- RColorBrewer::brewer.pal(n, "Dark2")

  # Assign names
  names(colors) <- labels

  return(colors)
}

summarize_confidence_intervals <- function(ci_list, true_value) {

  ci_list |>
    bind_rows(.id = "source_id") |>
    mutate(valid = !is.na(lower) & !is.na(upper),
           covers_true = lower <= true_value & upper >= true_value,
           width = upper - lower) |>
    group_by(pseudolikelihood, confidence) |>
    summarise(
      mean_width = mean(width, na.rm = TRUE),
      median_width = median(width, na.rm = TRUE),
      coverage_rate = mean(covers_true, na.rm = TRUE),
      valid_rate = mean(valid),
      .groups = "drop")
}

summarize_mle_performance <- function(mle_list, true_value) {

  combined <- bind_rows(mle_list, .id = "source_id")

  summary_df <- combined |>
    group_by(Source) |>
    summarise(
      bias = mean(MLE - true_value),
      sd = sqrt(mean((MLE - mean(MLE))^2)),
      rmse = sqrt(mean((MLE - true_value)^2)),
      .groups = "drop"
    )

  return(summary_df)
}



