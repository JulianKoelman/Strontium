# ─────────────────────────────────────────────────────────────────────
#  Sr isotope plot — lines ordered 1→2→3 across each tooth cluster
# ─────────────────────────────────────────────────────────────────────
#  • ambient muted shades per individual
#  • each tooth = its own cluster; lines sorted left→right
#  • hollow white‐filled for Exclude=="Yes"
#  • bold IDs on labels
#  • legend: one solid dot per site
#  • high‐res PNG output
# ─────────────────────────────────────────────────────────────────────
#  Requires: tidyverse, readxl, colorspace, ggtext
#  install.packages(c("tidyverse","readxl","colorspace","ggtext"))
# ─────────────────────────────────────────────────────────────────────

setwd("C:/Users/julko253/Dropbox/_PhD/_Project_1_ADN_FVK_KOR/Sr")

library(readxl)
library(tidyverse)
library(colorspace)
library(ggtext)

# 1 ── Palettes & Parameters ─────────────────────────────────────────
xlsx        <- "Sr_values_Project1.xlsx"
sheet       <- 5
site_order  <- c("Alvastra Dolmen","Fagervik","Korsnäs")

alt_cols <- list(
  "Alvastra Dolmen" = c("#6A7F92","#B2C2CC"),
  "Fagervik"        = c("#4D774E"),
  "Korsnäs"         = c("#8B4513","#D4B290")
)
site_cols <- map_chr(alt_cols, 1)   # first shade for legend

indiv_gap    <- 2.0
line_step    <- 0.08   # x‐offset per line number slot
label_offset <- 0.00040

# 2 ── Load & Basic Prep ─────────────────────────────────────────────
raw <- read_excel(xlsx, sheet = sheet) %>%
  fill(`Lab ID`,Tooth,Individual,Site, .direction = "down") %>%
  filter(!is.na(`87Sr/86Sr`)) %>%
  mutate(
    Radiocarbon_date = as.numeric(Radiocarbon_date),
    ymax = `87Sr/86Sr` + `2SE`,
    ymin = `87Sr/86Sr` - `2SE`
  )

# 3 ── Alternate Shade per Individual ────────────────────────────────
indiv_shade <- raw %>%
  distinct(Site, Individual) %>%
  group_by(Site) %>%
  mutate(i = row_number()) %>%
  ungroup() %>%
  mutate(
    base_colour = map2_chr(Site, i, ~{
      shades <- alt_cols[[.x]]
      shades[((.y-1) %% length(shades)) + 1]
    })
  ) %>%
  select(Site, Individual, base_colour)

# 4 ── Build Study & Tooth Indices ───────────────────────────────────
study <- raw %>%
  left_join(indiv_shade, by = c("Site","Individual")) %>%
  group_by(Individual, Tooth) %>%
  mutate(
    n_lines = n(),                     # how many ablation lines per tooth
    line_idx = `Line Number`           # explicit line order
  ) %>%
  ungroup() %>%
  mutate(
    colour_pt = if_else(Exclude == "Yes",
                        lighten(base_colour, 0.45),
                        base_colour),
    newID     = paste(Individual, Tooth, sep = ":")
  )

# 5 ── X‐axis Centres for Each newID ────────────────────────────────
# Order individuals oldest→youngest, then collect their teeth
indiv_order <- study %>%
  distinct(Individual, Site, Radiocarbon_date) %>%
  mutate(site_rank = match(Site, site_order)) %>%
  arrange(site_rank, desc(Radiocarbon_date)) %>%
  pull(Individual)

newID_order <- unlist(lapply(indiv_order, function(ind) {
  study %>%
    filter(Individual == ind) %>%
    distinct(newID, Tooth) %>%
    arrange(Tooth) %>%         # preserve lexicographic tooth order
    pull(newID)
}))

# numeric centre for each newID
newID_num <- setNames(seq_along(newID_order) * indiv_gap, newID_order)

# Now compute x_plot for every row:
study <- study %>%
  mutate(
    x_center = newID_num[newID],
    x_plot   = x_center + (line_idx - (n_lines+1)/2) * line_step
  )

# 6 ── Label positions & rich‐text ──────────────────────────────────
label_pos <- study %>%
  group_by(newID, base_colour, Individual, Tooth, Radiocarbon_date) %>%
  summarise(
    y_top = max(ymax) + label_offset,
    x_center = first(x_center),
    .groups = "drop"
  ) %>%
  mutate(
    label_md = sprintf("<b>%s</b><br>%s<br>%s&nbsp;BP",
                       Individual, Tooth, Radiocarbon_date)
  )

# 7 ── Plot ─────────────────────────────────────────────────────────
p <- ggplot() +
  # error-bars (all rows)
  geom_errorbar(
    data = study,
    aes(x = x_plot, y = `87Sr/86Sr`, ymin = ymin, ymax = ymax,
        colour = base_colour),
    width = 0, show.legend = FALSE
  ) +
  # solid points (included)
  geom_point(
    data = filter(study, Exclude == "No"),
    aes(x = x_plot, y = `87Sr/86Sr`, colour = base_colour),
    shape = 16, size = 3, show.legend = FALSE
  ) +
  # hollow points (excluded)
  geom_point(
    data = filter(study, Exclude == "Yes"),
    aes(x = x_plot, y = `87Sr/86Sr`, colour = base_colour),
    shape = 21, fill = "white", stroke = 0.8, size = 3,
    show.legend = FALSE
  ) +
  # labels atop each tooth cluster
  geom_richtext(
    data = label_pos,
    aes(x = x_center, y = y_top, label = label_md,
        colour = base_colour),
    fill = NA, label.color = NA,
    size = 3.2, lineheight = 0.9,
    hjust = 0.5, vjust = 0,
    show.legend = FALSE
  ) +
  # dummy layer for legend dots
  geom_point(
    data = tibble(colour = site_cols),
    aes(x = -Inf, y = -Inf, colour = colour),
    shape = 16, size = 4, inherit.aes = FALSE
  ) +
  scale_colour_identity(
    name   = "Site",
    breaks = unname(site_cols),
    labels = names(site_cols),
    guide  = guide_legend(override.aes = list(shape = 16, size = 4))
  ) +
  scale_x_continuous(
    breaks = newID_num,
    labels = newID_order,
    expand = expansion(mult = c(0.03, 0.05))
  ) +
  labs(
    x = "Individual:Tooth clusters (chronological by individual)",
    y = expression(""^{87}*Sr/""^{86}*Sr),
    title = "Strontium-isotope measurements – all teeth",
    subtitle = "Lines ordered L→R by Line Number; hollow = Exclude “Yes”"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "right",
    panel.grid.major.y = element_line(linetype = "dotted", linewidth = 0.3)
  )

print(p)

# Save high-res PNG
ggsave("Sr_plot_teeth_clusters_ordered.png",
       p, width = 12, height = 7, dpi = 300)
