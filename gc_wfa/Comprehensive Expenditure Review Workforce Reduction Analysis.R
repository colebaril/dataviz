require(pacman)
p_load(tidyverse, janitor, here, readxl, trashpanda, shadowtext, patchwork)

df <- read_excel(here("Data/wfa.xlsx")) |>
  clean_names() |>
  rename(org = core_public_administration_organization,
         total_pop = total_population_as_of_march_2025) |> 
  mutate(across(where(is.character), ~if_else(str_detect(., regex("Table ", ignore_case = TRUE)), NA_character_, .)))

staffing_historical <- read_csv(here("Data/historical_staffing_levels.csv")) |> 
  clean_names() |> 
  mutate(department_or_agency = case_when(department_or_agency == "Treasury Board of Canada Secretariat" ~ 
                                            "Treasury Board Secretariat",
                                          .default = department_or_agency))

staffing_2025 <- staffing_historical |> 
  filter(year == "2025") |> select(-year) |> 
  pivot_wider(names_from = tenure, values_from = number_of_employees) |> clean_names()

df <- df |> 
  left_join(staffing_2025, by = c("org" = "department_or_agency"))

wfa <- df |> 
  select(1:2, contains(c("wfa", "reduced"))) |> 
  mutate(across(contains(c("wfa", "reduced")), ~as.numeric(.)),
         total_wfa = rowSums(across(contains("wfa")), na.rm = TRUE),
         percent_wfa = total_wfa / total_pop,
         total_reduced = rowSums(across(contains("reduced")), na.rm = TRUE),
         percent_reduced = total_reduced / total_pop,
         .by = org) |> 
  mutate(org = fct_reorder(org, total_reduced)) |> 
  select(org, total_wfa:last_col()) |> 
  pivot_longer(!org, names_to = "var", values_to = "value")

wfa_total <- wfa |> 
  filter(var %in% c("total_wfa", "total_reduced"))

plot_total <- wfa_total |> 
  ggplot(aes(x = value, y = org, fill = var)) + 
  geom_col(position = position_dodge(reverse = TRUE)) +
  geom_text(aes(label = add_commas(value)),
            position = position_dodge(width = 0.9, reverse = TRUE),
            # colour = "black",
            colour = ifelse(wfa_total$value == max(wfa_total$value), "white", "black"),
            size = 3,
            fontface = "bold",
            vjust = 0.5, 
            hjust = ifelse(wfa_total$value == max(wfa_total$value), 1.1, 0)
  ) +
  scale_x_continuous(expand = c(0, 0), label = scales::comma) +
  scale_fill_manual(values = c("#A62A1E", "#26374A"), labels = c("Total Reductions", "WFA Reductions")) +
  theme_cole(base_size = 12, remove_grid = TRUE, show_axis_lines = "none") +
  theme(
    text = element_text(colour = "#333333"),
    axis.text.y = element_text(margin = margin(r = 0), vjust = 0.5),
    axis.text = element_text(face = "bold", colour = "#333333"),
    legend.title = element_blank()
  ) +
  labs(
    x = "Number of positions to be eliminated",
    y = "Core public administration organization")


wfa_percent_rank <- df |> 
  select(1:2, contains(c("wfa", "reduced"))) |> 
  mutate(across(contains(c("wfa", "reduced")), ~as.numeric(.)),
         total_wfa = rowSums(across(contains("wfa")), na.rm = TRUE),
         percent_wfa = total_wfa / total_pop,
         total_reduced = rowSums(across(contains("reduced")), na.rm = TRUE),
         percent_reduced = total_reduced / total_pop,
         .by = org) |> 
  mutate(org = fct_reorder(org, percent_reduced)) |> 
  select(org, total_wfa:last_col()) |> 
  pivot_longer(!org, names_to = "var", values_to = "value")



percent_wfa <- wfa_percent_rank |> 
  filter(var %in% c("percent_wfa", "percent_reduced")) 

plot_percent <- percent_wfa |> 
  ggplot(aes(x = value, y = org, fill = var)) + 
  geom_col(position = position_dodge(reverse = TRUE)) +
  geom_text(aes(label = paste0(round((value * 100), digits = 1), "%")),
            position = position_dodge(width = 0.9, reverse = TRUE),
            colour = ifelse(percent_wfa$value == max(percent_wfa$value), "white", "black"),
            size = 3,
            fontface = "bold",
            vjust = 0.5, 
            hjust = ifelse(percent_wfa$value == max(percent_wfa$value), 1.1, -0.1)
  ) +
  scale_x_continuous(expand = c(0, 0), label = scales::percent) +
  scale_fill_manual(values = c("#A62A1E", "#26374A"), labels = c("Total Reductions", "WFA Reductions")) +
  theme_cole(base_size = 12, remove_grid = TRUE, show_axis_lines = "none") +
  theme(
    text = element_text(colour = "#333333"),
    axis.text.y = element_text(margin = margin(r = 0), vjust = 0.5),
    axis.text = element_text(face = "bold", colour = "#333333"),
    legend.title = element_blank()) +
  labs(
    x = "Proportion of total organization population* to be eliminated",
    y = "Core public administration organization")




plot_total / plot_percent +
  plot_layout(axis_titles = "collect_y",
              guides = "collect") +
  plot_annotation(title = "Workforce Reductions Following Comprehensive Expenditure Review",
                  subtitle = "Total positions to be eliminated (top) and the proportion of total organization\npopulation* to be eliminated (bottom)",
                  caption = "Data: Government of Canada: Workforce reductions in the federal public service as of 2026-01-30\n*Total populations listed include employees of all tenure (indeterminate, term, casual, student) and WFA only applies to indeterminate employees") &
  theme(plot.title = element_text(size = 20, face = "bold", colour = "#333333", hjust = 0.5),
        plot.subtitle = element_text(size = 16, colour = "#333333", hjust = 0.5),
        plot.caption = element_text(colour = "#43474E", hjust = 0),
        legend.position = "top")

ggsave(plot = last_plot(), filename = here("Plots/cer_wfa_reductions.png"), dpi = 300, width = 10, height = 17.5)

