#
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggsankey)
library(viridis)

sankey_4 <- read_csv("sankey_4.csv")

#
glimpse(sankey_4)
names(sankey_4)

# Remove structural blanks created by merges / grouping
sankey_4 <- sankey_4 %>%
  filter(
    !is.na(`Physical and Environmental Considerations`),
    trimws(`Physical and Environmental Considerations`) != ""
  )

# Identify columns
col_year  <- "Publication Year"
col_topic <- "General_Research_Area"
col_var   <- "Physical and Environmental Considerations"
col_id    <- "Article Title"

# Confirm columns exist
needed <- c(col_year, col_topic, col_var)
missing_cols <- setdiff(needed, names(sankey_4))
if (length(missing_cols) > 0) {
  stop("Missing required columns in sankey_4: ", paste(missing_cols, collapse = ", "))
} else {
  message("✅ Required columns found: ", paste(needed, collapse = ", "))
}

if (!is.null(col_id) && !(col_id %in% names(sankey_4))) {
  warning("⚠️ col_id was set to '", col_id, "' but that column isn't in sankey_4. Set col_id <- NULL or fix the name.")
}

# Missingness + blanks audit
audit_missing <- sankey_4 %>%
  summarise(
    n_rows = n(),
    year_na  = sum(is.na(.data[[col_year]])),
    topic_na = sum(is.na(.data[[col_topic]])),
    var_na   = sum(is.na(.data[[col_var]])),
    var_blank = sum(str_trim(coalesce(as.character(.data[[col_var]]), "")) == "")
  )
print(audit_missing)

# Quick summaries
sankey_4 %>%
  count(`Publication Year`, General_Research_Area) %>%
  arrange(desc(n))

sankey_4 %>%
  count(`Physical and Environmental Considerations`) %>%
  arrange(desc(n))

# Prep for Sankey
sankey_df <- sankey_4 %>%
  mutate(`Publication Year` = if_else(`Publication Year` == 2024, 2023L, `Publication Year`)) %>%
  mutate(
    General_Research_Area = case_when(
      General_Research_Area == "Accessibility and Equality of Blue/Green Spaces" ~
        "Social Justice of Blue/Green Spaces",
      TRUE ~ General_Research_Area
    ),
    `Physical and Environmental Considerations` = case_when(
      `Physical and Environmental Considerations` %in%
        c("BGS Spatiotemporal Change", "Soundscape", "Seasonality") ~ "Other",
      TRUE ~ `Physical and Environmental Considerations`
    )
  ) %>%
  count(`Publication Year`, General_Research_Area, `Physical and Environmental Considerations`,
        name = "Count")

# (Optional) If you want topic order, define it here (edit as needed)
# If you don't set this, it will plot in default/alphabetical order.
# desired_order <- c("Mental Health and Blue/Green Spaces",
#                    "General Health and Blue/Green Spaces",
#                    "Ecosystem Services and Blue/Green Spaces",
#                    "Social Justice of Blue/Green Spaces",
#                    "Perceptions and Preferences of Blue/Green Spaces")
# sankey_df <- sankey_df %>%
#   mutate(General_Research_Area = factor(General_Research_Area, levels = desired_order))

# 5) Long format for ggsankey + plot
sankey_long <- sankey_df %>%
  make_long(`Publication Year`, General_Research_Area, `Physical and Environmental Considerations`,
            value = Count)

all_nodes <- unique(c(sankey_long$node, sankey_long$next_node))
node_colors <- setNames(viridis::viridis(length(all_nodes)), all_nodes)

# Manual colour overrides
node_colors["Biodiversity and Ecological Quality"] <- "#F4A261"
# node_colors["Other"] <- "grey70"  # uncomment if you want "Other" greyed out

custom_x_labels <- c("Year\n", "General Topic\n", "Physical & Environmental\nFactors")

sankey_plot <- ggplot(
  sankey_long,
  aes(
    x = x, next_x = next_x,
    node = node, next_node = next_node,
    fill = node, label = node,
    value = value
  )
) +
  geom_sankey(flow.alpha = 0.6, node.color = "black") +
  geom_sankey_label(size = 4, color = "black", fill = "white") +
  scale_fill_manual(values = node_colors) +
  scale_x_discrete(labels = custom_x_labels) +
  theme_sankey(base_size = 16) +
  guides(fill = "none") +
  theme(
    axis.text.x = element_text(vjust = 5.5),
    axis.ticks.x = element_blank()
  )

sankey_plot

# ---- 6) Save (A4 landscape) ----
ggsave("sankey_diagram_a4.png", plot = sankey_plot,
       width = 11.69, height = 8.27, dpi = 300)