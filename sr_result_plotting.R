setwd("C:/Users/julko253/Dropbox/_PhD/_Project_1_ADN_FVK_KOR/Sr")

library(readxl)
library(tidyverse)
library(colorspace)
library(ggtext)

# 1 ── Constants & NEW palettes ───────────────────────────────────────
xlsx       <- "Sr_values_Project1.xlsx"
sheet      <- 5
site_order <- c("Alvastra Dolmen","Fagervik","Korsnäs")

alt_cols <- list(
  "Alvastra Dolmen" = c("#5E7182","#6C5D6F"),
  "Fagervik"        = c("#4D774E", "#7B8C26"),
  "Korsnäs"         = c("#8B4513","#D4B290")
)

indiv_gap    <- 2.0
line_step    <- 0.08
label_offset <- 0.00040

# 2 ── Load & prep data ───────────────────────────────────────────────
raw <- read_excel(xlsx, sheet = sheet) %>%
  fill(`Lab ID`,Tooth,Individual,Site, .direction = "down") %>%
  filter(!is.na(`87Sr/86Sr`)) %>%
  mutate(
    Radiocarbon_date = as.numeric(Radiocarbon_date),
    ymax = `87Sr/86Sr` + `2SE`,
    ymin = `87Sr/86Sr` - `2SE`,
    newID = paste(Individual, Tooth, sep=":"),
    LineNumber = `Line Number`
  )

# 3 ── Determine newID order base on chronology ───────────────────────
indiv_order <- raw %>%
  distinct(Individual, Site, Radiocarbon_date) %>%
  mutate(site_rank = match(Site, site_order)) %>%
  arrange(site_rank, desc(Radiocarbon_date)) %>%
  pull(Individual)

# ── kor009 tooth‐development override ───────────────────────────────
tooth_levels <- list(
  kor009 = c("P2 mand dxt", "P2 mand sin", "M3 mand sin")
)

newID_order <- unlist(lapply(indiv_order, function(ind) {
  df <- raw %>%
    filter(Individual == ind) %>%
    distinct(Tooth, newID)
  if (ind %in% names(tooth_levels)) {
    df <- df %>%
      mutate(Tooth = factor(Tooth, levels = tooth_levels[[ind]]))
  }
  df %>%
    arrange(Tooth) %>%
    pull(newID)
}))

# 4 ── Assign alternate shade per tooth‐cluster newID ────────────────
newID_table <- raw %>%
  distinct(Site, newID) %>%
  mutate(newID = factor(newID, levels = newID_order)) %>%
  group_by(Site) %>%
  arrange(newID) %>%
  mutate(
    idx = row_number(),
    base_colour = map2_chr(Site, idx, ~{
      shades <- alt_cols[[.x]]
      shades[((.y - 1) %% length(shades)) + 1]
    })
  ) %>%
  ungroup() %>%
  select(Site, newID, base_colour)

# 5 ── Build study table & compute x_plot ────────────────────────────
study <- raw %>%
  left_join(newID_table, by = c("Site","newID")) %>%
  mutate(
    colour_pt = if_else(Exclude == "Yes",
                        lighten(base_colour, 0.45),
                        base_colour),
    x_center  = indiv_gap * match(newID, newID_order),
    x_plot    = x_center +
      (LineNumber - (max(LineNumber) + 1)/2) * line_step
  )

# 6 ── Label positions (one per newID) ───────────────────────────────
label_pos <- study %>%
  group_by(newID, base_colour, Individual, Tooth, Radiocarbon_date, x_center) %>%
  summarise(
    y_top = max(ymax) + label_offset,
    .groups = "drop"
  ) %>%
  mutate(
    label_md = sprintf("<b>%s</b><br>%s<br>%s&nbsp;BP",
                       Individual, Tooth, Radiocarbon_date)
  )

# 7 ── Manual dual‐shade legend coordinates ───────────────────────────
max_x <- max(study$x_center)
max_y <- max(study$ymax)
leg_y <- seq(max_y, max_y - 3*0.005, length.out = 3)
legend_df <- tibble(
  Site   = names(alt_cols),
  shade1 = map_chr(alt_cols, 1),
  shade2 = map_chr(alt_cols, ~ if(length(.x)>1) .x[2] else .x[1]),
  x1     = max_x + indiv_gap*0.5,
  x2     = max_x + indiv_gap*0.5 + line_step,
  y      = leg_y
)

# 8 ── Plot ───────────────────────────────────────────────────────────
p <- ggplot() +
  geom_errorbar(
    data = study,
    aes(x = x_plot, y = `87Sr/86Sr`,
        ymin = ymin, ymax = ymax,
        colour = base_colour),
    width = 0, show.legend = FALSE
  ) +
  geom_point(
    data = filter(study, Exclude == "No"),
    aes(x = x_plot, y = `87Sr/86Sr`, colour = base_colour),
    shape = 16, size = 3, show.legend = TRUE
  ) +
  geom_point(
    data = filter(study, Exclude == "Yes"),
    aes(x = x_plot, y = `87Sr/86Sr`, colour = base_colour),
    shape = 21, fill = "white", stroke = 0.8, size = 3, show.legend = FALSE
  ) +
  geom_richtext(
    data = label_pos,
    aes(x = x_center, y = y_top, label = label_md,
        colour = base_colour),
    fill = NA, label.color = NA,
    size = 3.2, lineheight = 0.9,
    hjust = 0.5, vjust = 0, show.legend = FALSE
  ) +
  # manual dual‐shade legend
  # geom_point(data = legend_df, aes(x = x1, y = y, colour = shade1), shape = 16, size = 4, inherit.aes = FALSE) +
  # geom_point(data = legend_df, aes(x = x2, y = y, colour = shade2), shape = 16, size = 4, inherit.aes = FALSE) +
  # geom_text(data = legend_df, aes(x = x2 + line_step*0.8, y = y, label = Site), hjust = 0, vjust = 0.5, size = 3.5, colour = "black", inherit.aes = FALSE) +
  scale_colour_identity(
    name   = "",
    breaks = site_cols,
    labels = names(site_cols),
    guide  = guide_legend(override.aes = list(shape = 16, size = 4))
  ) +
  scale_x_continuous(
    breaks = NULL,
    expand = expansion(mult = c(0.04, 0.05))
  ) +
  scale_y_continuous(
    breaks       = seq(
      floor(min(study$ymin)/0.01)*0.01,
      ceiling(max(study$ymax)/0.01)*0.01,
      by = 0.01
    ),
    minor_breaks = seq(
      floor(min(study$ymin)/0.002)*0.002,
      ceiling(max(study$ymax)/0.002)*0.002,
      by = 0.002
    ),
    expand       = expansion(mult = c(0.03, 0.12))
  ) +
  labs(
    x        = "Individual",
    y        = expression(""^{87}*Sr/""^{86}*Sr)
  ) +
  coord_cartesian(clip = "off") +
  theme_classic(base_size = 13) +
  theme(
    legend.position    = "right",
    plot.margin        = margin(5, 60, 5, 5, "pt"),
    axis.text.x        = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.ticks.x       = element_blank(),
    panel.grid.major.y = element_line(color = "grey70", size = 0.4),
    panel.grid.minor.y = element_line(color = "grey90", size = 0.2),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

print(p)

# 9 ── Export high‐res PNG ─────────────────────────────────────────────
ggsave(
  filename = "Sr_plot_teeth_clusters_ordered_col_alt.png",
  plot     = p,
  width    = 12,
  height   = 7,
  dpi      = 300
)
