

## previous function, before you added the title argument
# note: this one is 20 lines LONGER than the one above, so I am keeping it around in case chatgpt eliminated any functionality.

##------------------------------------------
## B) Main function with Approach A (top→bottom + fct_rev)
##------------------------------------------
create_sim_eps_graph <- function(data_graph,
                                 ord_nums_graph,
                                 eps_codes_graph,
                                 variable = c("race", "firstgen"),
                                 title = "") {
  
  # Match user input to "race" or "firstgen"
  variable <- match.arg(variable)
  
  if (variable == "race") {
    table_list <- create_sim_eps_race_table(
      data      = data_graph,
      ord_nums  = ord_nums_graph,
      eps_codes = eps_codes_graph
    )
    
    # -- Row pivot code (unchanged logic) --
    df_r <- table_list[[2]] %>%
      tidyr::pivot_longer(
        cols = c(r_white, r_asian, r_black, r_hispanic, r_multi, r_aian, r_nhpi),
        names_to  = "group",
        values_to = "value"
      ) %>%
      dplyr::mutate(
        group = recode_race(group),
        group = factor(group, levels = race_row_levels)
      )
    
    # Reorder eps_codename so "All" is top
    all_eps <- unique(df_r$eps_codename)
    new_eps <- move_all_to_top_in_row(all_eps)
    df_r <- df_r %>%
      dplyr::mutate(
        eps_codename = factor(eps_codename, levels = new_eps)
      )
    
    # -- Column pivot code (unchanged logic) --
    df_c <- table_list[[3]] %>%
      tidyr::pivot_longer(
        cols = c(c_known, c_white, c_asian, c_black, c_hispanic, c_multi, c_aian, c_nhpi),
        names_to  = "group",
        values_to = "value"
      ) %>%
      dplyr::filter(eps_codename != "All") %>%
      dplyr::mutate(
        group = recode_race(group),
        group = factor(group, levels = race_col_levels),
        eps_codename = factor(eps_codename)
      ) %>%
      dplyr::mutate(
        group       = forcats::fct_rev(group),
        eps_codename = forcats::fct_rev(eps_codename)
      )
    
    # Totals & labeling
    totals <- table_list[[1]] %>%
      dplyr::filter(eps_codename == "All") %>%
      tidyr::pivot_longer(
        cols = c(race_known, dplyr::starts_with("stu_")),
        names_to  = "group",
        values_to = "total_group"
      ) %>%
      dplyr::select(-all, -eps_codename) %>%
      dplyr::mutate(
        group       = recode_race(group),
        total_group = scales::comma(total_group, accuracy = 1)
      )
    
    df_c <- df_c %>%
      dplyr::left_join(totals, by = "group") %>%
      dplyr::mutate(
        group = factor(group, levels = rev(race_col_levels)),
        group_label = factor(
          paste0(group, "\n(N = ", total_group, ")"),
          levels = rev(
            paste0(race_col_levels, "\n(N = ", totals$total_group, ")")
          )
        )
      ) %>%
      dplyr::filter(!group %in% c("AIAN, non-Hispanic", "NHPI, non-Hispanic"))
    
    # Original titles
    row_plot_title <- "Race Distribution Within Each EPS Code (Row %)"
    col_plot_title <- "Distribution of Each Race Across EPS Codes (Column %)"
    fill_legend    <- "Geomarket"
    
  } else {
    # -- FIRST-GEN BRANCH --
    table_list <- create_sim_eps_firstgen_table(
      data = data_graph,
      ord_nums = ord_nums_graph,
      eps_codes = eps_codes_graph
    )
    
    # Row pivot
    df_r <- table_list[[2]] %>%
      tidyr::pivot_longer(
        cols = c(r_no_col, r_some_col, r_not_first),
        names_to  = "group",
        values_to = "value"
      ) %>%
      dplyr::mutate(
        group = recode_firstgen(group),
        group = factor(group, levels = firstgen_row_levels)
      )
    
    all_eps <- unique(df_r$eps_codename)
    new_eps <- move_all_to_top_in_row(all_eps)
    df_r <- df_r %>%
      dplyr::mutate(
        eps_codename = factor(eps_codename, levels = new_eps)
      )
    
    # Column pivot
    df_c <- table_list[[3]] %>%
      tidyr::pivot_longer(
        cols = c(c_known, c_no_col, c_some_col, c_not_first),
        names_to  = "group",
        values_to = "value"
      ) %>%
      dplyr::filter(eps_codename != "All") %>%
      dplyr::mutate(
        group = recode_firstgen(group),
        group = factor(group, levels = firstgen_col_levels),
        eps_codename = factor(eps_codename)
      ) %>%
      dplyr::mutate(
        group       = forcats::fct_rev(group),
        eps_codename = forcats::fct_rev(eps_codename)
      )
    
    # Totals & labeling
    totals <- table_list[[1]] %>%
      dplyr::filter(eps_codename == "All") %>%
      tidyr::pivot_longer(
        cols = c(stu_known, stu_no_col, stu_some_col, stu_not_first),
        names_to  = "group",
        values_to = "total_group"
      ) %>%
      dplyr::select(-eps_codename) %>%
      dplyr::mutate(
        group       = recode_firstgen(group),
        total_group = scales::comma(total_group, accuracy = 1)
      )
    
    df_c <- df_c %>%
      dplyr::left_join(totals, by = "group") %>%
      dplyr::mutate(
        group = factor(group, levels = rev(firstgen_col_levels)),
        label_str = paste0(group, "\n(N = ", total_group, ")")
      )
    
    label_levels <- rev(
      sapply(firstgen_col_levels, function(g) {
        val <- totals %>%
          dplyr::filter(group == g) %>%
          dplyr::pull(total_group)
        paste0(g, "\n(N = ", val, ")")
      })
    )
    
    df_c <- df_c %>%
      dplyr::mutate(
        group_label = factor(label_str, levels = label_levels)
      )
    
    # Original titles
    row_plot_title <- "First-Gen Distribution Within Each EPS Code (Row %)"
    col_plot_title <- "Distribution of First-Gen Groups Across EPS Codes (Column %)"
    fill_legend    <- "Geomarket"
  }
  
  # ----------------------------------------------------------------
  # (A) Append the user-supplied 'title' to each existing plot title
  # ----------------------------------------------------------------
  if (!is.null(title) && nzchar(title)) {
    row_plot_title <- paste0(row_plot_title, " - ", title)
    col_plot_title <- paste0(col_plot_title, " - ", title)
  }
  
  # ----------------------------------------------------------------
  # 3) Build the row-percent Plot
  # ----------------------------------------------------------------
  known_col <- if (variable == "race") "race_known" else "stu_known"
  
  plot_r <- df_r %>%
    ggplot2::ggplot(ggplot2::aes(x = fct_rev(eps_codename), y = value, fill = group)) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_fill(reverse = TRUE)) +
    
    ggplot2::geom_text(
      data = df_r %>% dplyr::distinct(eps_codename, .keep_all = TRUE),
      ggplot2::aes(
        x = eps_codename,
        y = 1,
        label = paste0("N=", formattable::comma(.data[[known_col]], digits = 0))
      ),
      hjust       = -0.1,
      size        = 3,
      inherit.aes = FALSE
    ) +
    
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 100)) +
    ggplot2::labs(
      title = row_plot_title,
      x     = NULL,
      y     = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::coord_flip(clip = "off")
  
  # ----------------------------------------------------------------
  # 4) Build the column-percent Plot
  # ----------------------------------------------------------------
  plot_c <- df_c %>%
    ggplot2::ggplot(ggplot2::aes(x = group_label, y = value, fill = eps_codename)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::labs(
      title = col_plot_title,
      x     = NULL,
      y     = NULL,
      fill  = fill_legend
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(scale = 1),
      expand = ggplot2::expansion(mult = c(0, 0.1))
    ) +
    ggplot2::scale_fill_discrete(guide = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x        = ggplot2::element_text(angle = 0, vjust = 1, hjust = 0.5),
      axis.title.x       = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank()
    ) +
    ggplot2::coord_flip(clip = "off")
  
  # ----------------------------------------------------------------
  # 5) Return both plots
  # ----------------------------------------------------------------
  list(plot_r = plot_r, plot_c = plot_c)
}