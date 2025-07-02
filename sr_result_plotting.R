# ─────────────────────────────────────────────────────────────────────
#  Sr isotope plot — alternating by tooth, new purples & legend fixes
# ─────────────────────────────────────────────────────────────────────
#  • new Alvastra: #5E7182 / #6C5D6F
#  • new Korsnäs:  #8B4513 / #D4B290
#  • alternate shade per newID (tooth‐cluster) within each site
#  • hollow white‐filled circles for Exclude=="Yes"
#  • bold IDs in on-plot labels
#  • legend shows both shades per site
# ─────────────────────────────────────────────────────────────────────
#  Requires: tidyverse, readxl, colorspace, ggtext
#  install.packages(c("tidyverse","readxl","colorspace","ggtext"))
# ─────────────────────────────────────────────────────────────────────

setwd("C:/Users/julko253/Dropbox/_PhD/_Project_1_ADN_FVK_KOR/Sr")

library(readxl)
library(tidyverse)
library(colorspace)
library(ggtext)

# 1 ── Constants & your new palettes ─────────────────────────────────
xlsx       <- "Sr_values_Project1.xlsx"
sheet      <- 5
site_order <- c("Alvastra Dolmen","Fagervik","Korsnäs")

# NEW alternating palettes per site:
alt_cols <- list(
  "Alvastra Dolmen" = c("#5E7182","#6C5D6F"),  # slate-purple pair
  "Fagervik"        = c("#4D774E"),           # single muted green
  "Korsnäs"         = c("#8B4513","#D4B290")  # earth pair
)

# build legend entries: flatten shades and repeat site names
legend_cols   <- unlist(alt_cols)
legend_labels <- rep(names(alt_cols), lengths(alt_cols))

indiv_gap    <- 2.0
line_step    <- 0.08
label_offset <- 0.00040

# 2 ── Load & basic prep ─────────────────────────────────────────────
raw <- read_excel(xlsx, sheet = sheet) %>%
  fill(`Lab ID`,Tooth,Individual,Site, .direction="down") %>%
  filter(!is.na(`87Sr/86Sr`)) %>%
  mutate(
    Radiocarbon_date = as.numeric(Radiocarbon_date),
    ymax = `87Sr/86Sr` + `2SE`,
    ymin = `87Sr/86Sr` - `2SE`
  )

# 3 ── Build the newID (Individual:Tooth) and index clusters ────────
# This also gives us the chronological order of individuals internally.
study <- raw %>%
  mutate(
    newID = paste(Individual, Tooth, sep=":"),
    # we'll later factor newID in the correct order
    LineNumber = `Line Number`
  )

# Determine newID order: per individual in chrono order, then by Tooth
indiv_order <- study %>%
  distinct(Individual, Site, Radiocarbon_date) %>%
  mutate(site_rank = match(Site, site_order)) %>%
  arrange(site_rank, desc(Radiocarbon_date)) %>%
  pull(Individual)

newID_order <- unlist(lapply(indiv_order, function(ind) {
  study %>%
    filter(Individual == ind) %>%
    distinct(Tooth, newID) %>%
    arrange(Tooth) %>%
    pull(newID)
}))

study <- study %>%
  mutate(newID = factor(newID, levels = newID_order))

# 4 ── Assign an alternating shade per newID within each site ────────
newID_table <- study %>%
  distinct(Site, newID) %>%
  group_by(Site) %>%
  mutate(
    shade_idx  = ((row_number() - 1) %% length(alt_cols[[first(Site)]])) + 1,
    base_colour = alt_cols[[first(Site)]][shade_idx]
  ) %>%
  ungroup() %>%
  select(Site, newID, base_colour)

study <- study %>%
  left_join(newID_table, by = c("Site","newID")) %>%
  mutate(
    colour_pt = if_else(Exclude == "Yes",
                        lighten(base_colour, 0.45),
                        base_colour),
    # numeric x‐centre for each newID
    x_center = as.numeric(newID) * indiv_gap,
    # offset each ablation line by its LineNumber
    x_plot   = x_center +
      (LineNumber - (max(LineNumber) + 1)/2) * line_step
  )

# 5 ── Build labels (one per newID) at top of each cluster ──────────
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

y_min <- floor(min(study$ymin) / 0.002) * 0.002
y_max <- ceiling(max(study$ymax) / 0.002) * 0.002

# 6 ── Plot ──────────────────────────────────────────────────────────
p <- ggplot() +
  # 1) error-bars
  geom_errorbar(
    data = study,
    aes(x = x_plot, y = `87Sr/86Sr`,
        ymin = ymin, ymax = ymax,
        colour = base_colour),
    width = 0, show.legend = FALSE
  ) +
  # 2) solid points (Include)
  geom_point(
    data = filter(study, Exclude == "No"),
    aes(x = x_plot, y = `87Sr/86Sr`, colour = base_colour),
    shape = 16, size = 3, show.legend = FALSE
  ) +
  # 3) hollow points (Exclude)
  geom_point(
    data = filter(study, Exclude == "Yes"),
    aes(x = x_plot, y = `87Sr/86Sr`, colour = base_colour),
    shape = 21, fill = "white", stroke = 0.8, size = 3,
    show.legend = FALSE
  ) +
  # 4) rich-text labels
  geom_richtext(
    data = label_pos,
    aes(x = x_center, y = y_top,
        label = label_md,
        colour = base_colour),
    fill = NA, label.color = NA,
    size = 3.2, lineheight = 0.9,
    hjust = 0.5, vjust = 0,
    show.legend = FALSE
  ) +
  # 5) dummy layer to produce BOTH shades per site in the legend
  geom_point(
    data = tibble(colour = legend_cols),
    aes(x = -Inf, y = -Inf, colour = colour),
    shape = 16, size = 4, inherit.aes = FALSE
  ) +
  scale_colour_identity(
    name   = "Site",
    breaks = legend_cols,
    labels = legend_labels,
    guide  = guide_legend(override.aes = list(shape = 16, size = 4))
  ) +
  scale_x_continuous(
    breaks = as.numeric(factor(newID_order)) * indiv_gap,
#    labels = newID_order,
#    labels = sub(":.*", "", newID_order),
    labels = NULL,
    expand = expansion(mult = c(0.04, 0.05))
  ) +
  scale_y_continuous(
    breaks       = seq(y_min, y_max, by = 0.01),
    minor_breaks = seq(y_min, y_max, by = 0.002),
    expand       = expansion(mult = c(0.03, 0.08))
  ) +
  # 2) put back your x‐axis title
  labs(
    x = "Individuals (chronological)",
    y = expression(""^{87}*Sr/""^{86}*Sr)
  ) +
  # 3) style major (0.01) vs minor (0.002) grids
  theme_classic(base_size = 13) +
  theme(
    panel.grid.major.y = element_line(linetype = 'solid', color = "grey90", size = 0.4),
    panel.grid.minor.y = element_line(linetype = 'dashed' , color = "grey90", size = 0.2),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x       = element_blank(),          # you already had this
    axis.text.x        = element_text(angle = 90, hjust = 1)
  )

print(p)

# 7 ── Export high–res PNG ───────────────────────────────────────────
ggsave("Sr_plot.png",
       p, width = 12, height = 7, dpi = 300)
