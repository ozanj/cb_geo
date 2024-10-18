
format_eps_sim_table_simple <- function(df) {
  library(dplyr)
  library(kableExtra)
  
  # Extract the individual tables
  counts <- df$count_table
  col_pct <- df$col_pct_table
  row_pct <- df$row_pct_table
  
  # Rename columns for consistency
  counts_formatted <- counts %>%
    rename(
      `EPS Codename` = eps_codename,
      `Total Students` = stu_all,
      `Race Known` = stu_race_known,
      White = stu_white,
      Asian = stu_asian,
      Black = stu_black,
      Hispanic = stu_hispanic,
      `Two Races` = stu_tworaces
    ) %>%
    mutate(Section = "1. Counts") %>%
    select(Section, everything())
  
  col_pct_formatted <- col_pct %>%
    rename(
      `EPS Codename` = eps_codename,
      `Total Students` = stu_all,
      `Race Known` = stu_race_known,
      White = col_pct_white,
      Asian = col_pct_asian,
      Black = col_pct_black,
      Hispanic = col_pct_hispanic,
      `Two Races` = col_pct_tworaces
    ) %>%
    mutate(Section = "2. Column Percentages") %>%
    select(Section, everything())
  
  row_pct_formatted <- row_pct %>%
    rename(
      `EPS Codename` = eps_codename,
      `Total Students` = stu_all,
      `Race Known` = stu_race_known,
      White = row_pct_white,
      Asian = row_pct_asian,
      Black = row_pct_black,
      Hispanic = row_pct_hispanic,
      `Two Races` = row_pct_tworaces
    ) %>%
    mutate(Section = "3. Row Percentages") %>%
    select(Section, everything())
  
  # Combine all sections into one table
  full_table <- bind_rows(counts_formatted, col_pct_formatted, row_pct_formatted)
  
  # Create a LaTeX-compatible table using kable
  table_kable <- full_table %>%
    kbl(
      format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      col.names = c("Section", "EPS Codename", "Total Students", "Race Known", 
                    "White", "Asian", "Black", "Hispanic", "Two Races"),
      caption = "Characteristics of Geomarkets around Philadelphia \\label{tab:philly_eps_table_summary}"
    ) %>%
    kable_styling(
      latex_options = c("hold_position", "striped", "scale_down"),
      position = "left",
      font_size = 10
    ) %>%
    # Bold and shade the section headers
    row_spec(
      which(full_table$Section != ""),
      bold = TRUE,
      background = "#D3D3D3"
    )
  
  return(table_kable)
}

# Example Usage:
# Assuming philly_order448440_psat1270_1520 is already loaded and structured
formatted_table <- format_eps_sim_table_simple(df = philly_order448440_psat1270_1520)
cat(formatted_table)