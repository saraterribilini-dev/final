#packages:
library(ggplot2)
library(dplyr)
library(cowplot)
library(scales)

# ============================================================
# FINAL MULTI-PANEL FIGURE
# ============================================================

# ------------------------------------------------------------
# Clean panel A: map
# ------------------------------------------------------------

graphA_final <- graphA +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    legend.position = "right",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8))

# ------------------------------------------------------------
# Clean panel B: elevation distribution
# ------------------------------------------------------------

plotB_final <- ggplot(df_points, aes(x = elevation, fill = Climate_Re)) +
  geom_density(
    alpha = 0.6,
    adjust = 3,
    color = "black",
    linewidth = 0.3) +
  coord_cartesian(xlim = c(0, 1000)) +
  scale_fill_manual(values = climate_cols, name = "Climate region") +
  facet_wrap(~ species, scales = "free_y") +
  labs(
    title = "B. Elevation distribution across climate regions",
    x = "Elevation (m)",
    y = "Density") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 12, face = "bold"),
    strip.text = element_text(size = 9, face = "italic"),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    panel.grid.minor = element_blank())

# ------------------------------------------------------------
# Clean panel C: species composition across climate regions
# ------------------------------------------------------------

climate_species <- df_points %>%
  count(Climate_Re, species) %>%
  group_by(Climate_Re) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

plotC_final <- ggplot(
  climate_species,
  aes(
    x = Climate_Re,
    y = species,
    size = prop,
    color = species,
    label = scales::percent(prop, accuracy = 1))) +
  geom_point(alpha = 0.85) +
  geom_text(
    
    color = "#f9e2c2",
    fontface = "bold",
    size = 3) +
  scale_size_area(max_size = 16) +
  scale_color_manual(values = species_cols) +
  labs(title = "C. Species composition across climate regions",
    x = "Climate region",
    y = NULL) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11,face = "bold",hjust = 1),
    axis.title = element_text(size = 9),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(angle = 35, hjust = 1, size = 8),
    panel.grid.minor = element_blank())


#This code it works when I run it manually, but when I do it via luncher with source() it doesn't work, i tried so many different code but every time is the same thing.
#The traceback() confirms that the error occurs when R tries to print the final plot, not when it creates it.

figure_finale <- ggdraw() +
  draw_plot(graphA_final, x = 0.00, y = 0.42, width = 1.00, height = 0.58) +
  draw_plot(plotB_final,  x = 0.00, y = 0.00, width = 0.65, height = 0.42) +
  draw_plot(plotC_final,  x = 0.65, y = 0.00, width = 0.35, height = 0.42)

#quartz()
#print(figure_finale)

#The only solution I found to ensure that the final map can be viewed via luncher is to save it and then reload the saved map
ggplot2::ggsave(
  filename = "final_summary_panel.png",
  plot = figure_finale,
  width = 14,
  height = 10,
  dpi = 300)
system("open final_summary_panel.png")

# All analyses of the individual plots have already been carried out.
# These three plots were combined because they summarise the three main dimensions of the study: geographic distribution (A), environmental
# gradients (B), and climatic composition (C). Together they provide a compact overview of the ecological overlap between the cuckoo
# and its host species.

