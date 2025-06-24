library(tidyverse)
load("../../Kirk Projects/air_bnb/data/output/dpm_quarter_fit.RData")
load("../../Kirk Projects/air_bnb/data/output/dpm_quarter_fit_unstd.RData")

hide_lag = TRUE
con_only = FALSE
scales = "fixed"
x <- dpm_quarter_fit_unstd
dv_levels <- c("Robbery", "Burglary", "Theft", "ASB", "Violence", "Harm")
plot_data <- x %>%
  select(dv, spec, term, estimate, std.error, conf.low, conf.high) %>%
  mutate(term = ifelse(str_detect(term, "dp|dlg") & str_detect(term, "t - 1"), "crime (t - 1)", term),
         across(c(dv, term), ~ str_to_title(str_replace_all(str_remove_all(., "(log_)|(std_)|(dlg_|dp_|abnb_)|(_aw$)?"), "_", " "))),
         spec = fct_rev(factor(str_to_title(spec), levels = c("Con", "Both", "Lag"))),
         dv = fct_relevel(str_replace_all(dv, c("Violence Harm" = "Harm", "Asb" = "ASB")), dv_levels))
if(hide_lag){
  plot_data <- plot_data %>% filter(term != "Crime (T - 1)")
}
plot_data <- plot_data %>% 
  filter(spec == "Both") %>% 
  mutate(dv = fct_rev(dv)) %>%
  mutate(graybar = ifelse(dv %in% c("Burglary", "ASB", "Harm"), "black", "white"))
out_plot <- ggplot(plot_data, aes(x = estimate, y = dv)) + 
  geom_tile(aes(alpha = graybar), 
            width = Inf, height = 1, fill = "black") +
  scale_alpha_manual(values = c("black" = 0.3, "white" = 0)) +
  ylab(NULL) + xlab("Additional quarterly crimes per active rental") +
  geom_point() + 
  coord_cartesian(xlim = c(-0.05, 0.18), expand = TRUE) +
  facet_grid( ~ term, scales = scales) + 
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, size = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  theme_minimal() + 
  theme(text = element_text(family = "serif"),
        panel.grid.major.y  = element_blank(),
        panel.grid.minor.x  = element_blank(),
        panel.spacing.y = unit(0,"lines"),
        panel.spacing.x = unit(1,"lines"),
        strip.placement = "outside",
        strip.text.y = element_text(face = "bold"),
        # axis.text.y = element_text(face = "bold"),
        axis.title.y = ggtext::element_markdown(),
        legend.position = "none")
ggsave(plot = out_plot, filename = "./img/quarter-results-1.png", width = 4.5, height = 4.5)



load("../../Kirk Projects/air_bnb/data/output/dpm_quarter_diffprops_fit_unstd.RData")

plot_data <- dpm_quarter_diffprops_fit_unstd %>%
  select(dv, spec, term, estimate, std.error, conf.low, conf.high) %>%
  mutate(term = ifelse(str_detect(term, "dp|dlg") & str_detect(term, "t - 1"), "crime (t - 1)", term),
         across(c(dv, term), ~ str_to_title(str_replace_all(str_remove_all(., "(log_)|(std_)|(dlg_|dp_|abnb_)|(_aw$)?"), "_", " "))),
         spec = fct_rev(factor(str_to_title(spec), levels = c("Con", "Both", "Lag"))),
         dv = fct_relevel(str_replace_all(dv, c("Violence Harm" = "Harm", "Asb" = "ASB")), dv_levels)) |>
  filter(spec == "Both" & !str_detect(term, "T - 1")) %>% 
  mutate(dv = fct_rev(dv)) %>%
  mutate(graybar = ifelse(dv %in% c("Burglary", "ASB", "Harm"), "black", "white"))
out_plot <- ggplot(plot_data, aes(x = estimate, y = dv)) + 
  geom_tile(aes(alpha = graybar), 
            width = Inf, height = 1, fill = "black") +
  scale_alpha_manual(values = c("black" = 0.3, "white" = 0)) +
  ylab(NULL) + xlab("Additional quarterly crimes per active rental") +
  geom_point() + 
  facet_grid( ~ term, scales = scales) + 
  coord_cartesian(xlim = c(-0.05, 0.18), expand = TRUE) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, size = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  theme_minimal() + 
  theme(text = element_text(family = "serif"),
        panel.grid.major.y  = element_blank(),
        panel.grid.minor.x  = element_blank(),
        panel.spacing.y = unit(0,"lines"),
        panel.spacing.x = unit(1,"lines"),
        strip.placement = "outside",
        strip.text.y = element_text(face = "bold"),
        # axis.text.y = element_text(face = "bold"),
        axis.title.y = ggtext::element_markdown(),
        legend.position = "none")
ggsave(plot = out_plot, filename = "./img/property-results-1.png", width = 5.5, height = 4.5)

# 
# load("../../Kirk Projects/air_bnb/data/output/dpm_year_ce_imputed_2lvl_fit.RData")
# plot_data <- dpm_year_ce_imputed_2lvl_fit |>
#   select(dv, spec, term, estimate, std.error, conf.low, conf.high) %>%
#   mutate(term = ifelse(str_detect(term, "dp|dlg") & str_detect(term, "t - 1"), "crime (t - 1)", term),
#          term = str_replace(term, "std_ce_imputed_2lvl", "Collective\nEfficacy"),
#          across(c(dv, term), ~ str_to_title(str_replace_all(str_remove_all(., "(log_)|(std_)|(dlg_|dp_|abnb_)|(_aw$)?"), "_", " "))),
#          term = str_replace(term, "Active Rentals", "Active\nRentals"),
#          spec = fct_rev(factor(str_to_title(spec), levels = c("Con", "Both", "Lag"))),
#          dv = fct_relevel(str_replace_all(dv, c("Violence Harm" = "Harm", "Asb" = "ASB")), dv_levels)) |>
#   filter(spec == "Both" & !str_detect(term, "Crime") & !str_detect(term, "T - 1")) %>% 
#   mutate(dv = fct_rev(dv)) %>%
#   mutate(graybar = ifelse(dv %in% c("Burglary", "ASB", "Harm"), "black", "white"))
# out_plot <- ggplot(plot_data, aes(x = estimate, y = dv)) + 
#   geom_tile(aes(alpha = graybar), 
#             width = Inf, height = 1, fill = "black") +
#   scale_alpha_manual(values = c("black" = 0.3, "white" = 0)) +
#   ylab(NULL) + xlab("Yearly crimes per active rental (fully standardized)") +
#   geom_point() + 
#   facet_grid( ~ term, scales = scales) + 
#   coord_cartesian(xlim = c(-0.10, 0.10), expand = TRUE) +
#   geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, size = 0.2) +
#   geom_vline(xintercept = 0, linetype = "dashed") + 
#   theme_minimal() + 
#   theme(text = element_text(family = "serif"),
#         panel.grid.major.y  = element_blank(),
#         panel.grid.minor.x  = element_blank(),
#         panel.spacing.y = unit(0,"lines"),
#         panel.spacing.x = unit(1,"lines"),
#         strip.placement = "outside",
#         strip.text.y = element_text(face = "bold"),
#         # axis.text.y = element_text(face = "bold"),
#         axis.title.y = ggtext::element_markdown(),
#         legend.position = "none")
# ggsave(plot = out_plot, filename = "./img/property-results-1.png", width = 4.5, height = 4.5)