library(gt)
library(dplyr)

# Sample data frame
data <- tibble(
  name = c("Alice", "Bob", "Charlie"),
  value = c(10, 20, 30),
  color = c("#FF9999", "#99CCFF", "#99FF99")  # Column with color values
)

# Create gt table
gt_table <- data %>%
  gt() %>%
  tab_style(
    style = cell_fill(color = data$color[1]),
    locations = cells_body(rows = 1)
  ) %>%
  tab_style(
    style = cell_fill(color = data$color[2]),
    locations = cells_body(rows = 2)
  ) %>%
  tab_style(
    style = cell_fill(color = data$color[3]),
    locations = cells_body(rows = 3)
  )

# Render the table
gt_table

library(gt)
library(dplyr)

# Sample data frame
data <- tibble(
  name = c("Alice", "Bob", "Charlie"),
  value = c(10, 20, 30),
  color = c("#FF9999", "#99CCFF", "#99FF99")  # Column with color values
)

# Create gt table with only name and value columns
gt_table <- data %>%
  select(name, value) %>%  # Select only the columns you want to display
  gt()

# Apply row-specific background colors (still using the full data frame's color column)
for (i in seq_len(nrow(data))) {
  gt_table <- gt_table %>%
    tab_style(
      style = cell_fill(color = data$color[i]),
      locations = cells_body(rows = i)
    )
}

# Render the table
gt_table


library(gt)
library(dplyr)
library(purrr)

# Your pipeline with corrected gt table creation
gt_table <- attr_tabel_individuele_tickers_sel_ff() %>%
  rename(weight_in_RD = start_weight, weight_in_bm = wt_bm_start, OU = OU_start,
         `weight_in_RD ` = end_weight, `weight_in_bm ` = wt_bm_end, `OU ` = OU_end) |> 
  #kleurtjes
  left_join(sectors_colors, by = c("sector" = "ticker")) %>%
  mutate(fill = ifelse(is.na(fill), "#D3D3D3", fill)) %>%
  { data <- .; 
    gt(., id = "one") %>% 
    reduce(.x = seq_len(nrow(data)), 
           .f = function(tbl, i, df = data) {
             tab_style(tbl, 
                       style = cell_fill(color = df$fill[i], alpha = 0.5), 
                       locations = cells_body(columns = 1:3, rows = i))
           }, 
           .init = .)} %>%
  cols_hide(columns = fill) |> 
  #einde kleurtjes
  fmt_percent(columns = c(weight_in_RD, weight_in_bm, `weight_in_RD `, `weight_in_bm `, OU, `OU `, trr, sel_ff), decimals = 2) |>
  #tab_spanner(columns = c(weight_in_RD, weight_in_bm, OU), label = format(start_date, "%d-%b-%y")) |>
  #tab_spanner(columns = c(`weight_in_RD `, `weight_in_bm `, `OU `), label = format(end_date, "%d-%b-%y")) |>
  tab_spanner(columns = c(trr, sel_ff), label = "Performance") |>
  tab_style(locations = cells_body(columns = c(OU, `OU `, sel_ff)),
           style = list(cell_text(weight = "bold"))) |>
  gt_add_dividers_right(columns = c(3, 6, 9)) |>
  gt_data_colors(columns = c("OU", "OU ", "sel_ff")) |>
  gt_delete_first_col_label() |>
  gt_cols_align() |>
  gt_first_col_bold() |>
  sub_zero(zero_text = " ") |>
  opt_row_striping() |>
  gt_font_padding_align() |>
  gt_last_row_bold() |>
  gt_container_scroll() 


# Render the table
gt_table