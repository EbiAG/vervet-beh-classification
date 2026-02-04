# We will make some plots for the publication figure 1 panel

library(tidyverse)
library(htmlwidgets)
library(networkD3)
library(webshot2) # For saving plot

# Load data ----
beh_annotations <- read.csv(
    "./data/raw/annotations/annotations_merged_majority.csv"
) %>% filter(attribution_merged != "Remove")

# Burst ids
burst_ids <- read.csv(
    "./data/raw/annotated_burst_id_lookup_table.csv"
)

# join to annotations
beh_annotations <- beh_annotations %>%
    left_join(
        burst_ids,
        by = c("burst", "Ind_ID", "new_burst")
    ) %>%
    # Select only digits in burst column
    mutate(
        burst = as.numeric(str_extract(burst, "\\d+")),
        attribution_merged = gsub(" ", "_", attribution_merged),
)

# Check that the data is as expected
beh_annotations %>%
    filter(burst_id == 70722)

# Check if each burst has as many items
beh_annotations %>%
    group_by(Ind_ID, burst, burst_id) %>%
    summarise(
        n = n(),
        .groups = "drop"
    ) %>%
    filter(n != burst)
# Not all bursts have same number of b urst_id items. We need to adapt to this

beh_order <- c(
    "Resting", "Eating", "Walking", "Grooming_actor",
    "Grooming_receiver", "Sleeping", "Self-scratching", "Running"
)

# Get bursts per category and burst_level
burst_n <- beh_annotations %>%
    group_by(attribution_merged, burst) %>%
    summarise(
        source_name = n(),
        .groups = "drop"
    )

# Sankey plot for changes in behaviour classes ----
## Convert to data for sankey diagram ----
# Create nodes for Sankey diagram
nodes_for_sankey <- beh_annotations %>%
    distinct(attribution_merged, burst) %>%
    mutate(node_label = paste0(attribution_merged, "_L", burst)) %>%
    arrange(burst, attribution_merged) %>%
    bind_rows(
        beh_annotations %>%
            distinct(attribution_merged) %>%
            mutate(
                burst = 0,
                node_label = paste0(attribution_merged, "_L", burst)
            )
    ) %>%
    # Arrange in order of burst and then specific order for attribution_merged
    arrange(burst, factor(attribution_merged, levels = beh_order)) %>%
    mutate(node_id = row_number() - 1) %>% # 0-indexed ID
    rename(
        id = node_id,
        name = node_label,
        group = attribution_merged
    ) %>%
    left_join(
        burst_n,
        by = c("burst", "group" = "attribution_merged")
    ) %>%
    mutate(source_name = ifelse(burst == 0, group, source_name)) %>%
    select(-burst) %>%
    mutate(
        source_name = gsub("_", " ", source_name)
    )

# Create links for Sankey diagram
beh_data_links <- beh_annotations %>%
    group_by(Ind_ID, burst_id, burst) %>%
    mutate(
        n_items_at_this_level = n(),
    )

# Create source and target dfs
source_df <- beh_data_links %>%
    select(
        Ind_ID,
        burst_id,
        source_level = burst,
        source_attribution = attribution_merged,
        source_new_burst = new_burst,
        n_source_items = n_items_at_this_level
    )

target_df <- beh_data_links %>%
    select(
        Ind_ID,
        burst_id,
        target_level = burst,
        target_attribution = attribution_merged,
        target_new_burst = new_burst, 
        n_target_items = n_items_at_this_level
    )

# Create inner join for all links
raw_micro_links <- source_df %>%
    inner_join(
        target_df,
        by = c("Ind_ID", "burst_id"),
        relationship = "many-to-many"
    ) %>%
    # Only link to immediate next level
    filter(source_level + 1 == target_level) %>% 
    mutate(
        value_per_micro_link = ifelse(n_source_items > 0, 1 / n_source_items, 0)
    )

# Aggregate microlinks
agg_links <- raw_micro_links %>%
    mutate(
        source_node_label = paste0(source_attribution, "_L", source_level),
        target_node_label = paste0(target_attribution, "_L", target_level)
    ) %>%
    group_by(source_node_label, target_node_label) %>%
    summarise(
        total_value = sum(value_per_micro_link, na.rm = TRUE),
        .groups = "drop"
    )

# Create links for L0 to L1
# These will be the same values as L1
agg_links_l0 <- beh_annotations %>%
    filter(burst == 1) %>%
    group_by(attribution_merged) %>%
    summarise(
        total_value = n(),
        .groups = "drop"
    ) %>%
    mutate(
        source_node_label = paste0(attribution_merged, "_L0"),
        target_node_label = paste0(attribution_merged, "_L1")
    ) %>%
    select(source_node_label, target_node_label, total_value)

# Combine agg_links
agg_links_all <- bind_rows(
    agg_links_l0,
    agg_links
)

# Join to nodes
links_nodes <- agg_links_all %>%
    left_join(
        nodes_for_sankey %>%
            select(name, source_id = id),
        by = c("source_node_label" = "name")
    ) %>%
    left_join(
        nodes_for_sankey %>%
            select(name, target_id = id),
        by = c("target_node_label" = "name")
    ) %>%
    # Remove any links with no values
    filter(!is.na(source_id) & !is.na(target_id) & total_value > 0) %>% 
    select(source = source_id, target = target_id, value = total_value) %>%
    # Add colour values
    left_join(
        nodes_for_sankey %>%
            select(id, source_group = group),
        by = c("source" = "id")
    )

head(links_nodes)
head(nodes_for_sankey)

## Create Sankey plot ----
# Create colour map
sankey_colour_map <- list(
    "Resting" = "#8DD3C7",
    "Eating" = "#FFFFB3",
    "Walking" = "#BEBADA",
    "Grooming_actor" = "#FB8072",
    "Grooming_receiver" = "#80B1D3",
    "Sleeping" = "#FDB462",
    "Self-scratching" = "#B3DE69",
    "Running" = "#FCCDE5"
)

domain_array <- c()
range_array <- c()
for (category in beh_order) {
    domain_array <- c(domain_array, category)
    range_array <- c(range_array, sankey_colour_map[[category]])
}

# Construct the JavaScript arrays for domain and range
js_domain_string <- paste0('["', paste(domain_array, collapse = '", "'), '"]')
js_range_string <- paste0('["', paste(range_array, collapse = '", "'), '"]')

# Create the D3 color scale function as a JavaScript string
# This uses htmlwidgets::JS to ensure the string is treated as JavaScript code
d3_color_scale_js <- JS(
    paste0(
        "d3.scaleOrdinal().domain(", js_domain_string, ").range(", js_range_string, ");"
    )
)
# print(d3_color_scale_js)

beh_attribution_sankey_plot <- sankeyNetwork(
    Links = links_nodes %>% as.data.frame(),
    Nodes = nodes_for_sankey,
    Source = "source",
    Target = "target",
    Value = "value",
    NodeID = "source_name",
    NodeGroup = "group",
    LinkGroup = "source_group",
    colourScale = d3_color_scale_js,
    units = "TWh",
    fontSize = 16,
    nodeWidth = 30,
    nodePadding = 10,
    iterations = 0
)
beh_attribution_sankey_plot

## Save Sankey Plot ----
# Save to temp html file
temp_html_file_path <- tempfile(fileext = ".html")
saveWidget(
    beh_attribution_sankey_plot, temp_html_file_path,
    selfcontained = TRUE
)

output_svg_path <- "./plots/Fig1_behaviour_attribution_burst_sankey_plot.svg"

# Save as SVG
library(rvest)

html_content <- read_html(temp_html_file_path)
svg_element <- html_content %>%
    html_element("svg")

write(svg_element, file = output_svg_path)

# Plot representative bursts ----
## Load data ----
# Load clean bursts
clean_bursts <- read.csv(
    "./data/raw/clean_burst_ids.csv"
)

# Load features to obtain vedba
features_b1_u <- arrow::read_parquet(
    "./data/raw/features/Burst_1/annotated_features_burst_1_uncorrected.parquet",
    col_select = c("Ind_ID", "new_burst", "mean_vedba")
)

# Get annotations
annotations_df <- read.csv(
    "./data/raw/annotations/annotations_merged_majority.csv"
) %>%
    filter(attribution_merged != "Remove") %>%
    filter(burst == "burst_1") %>%
    select(Ind_ID, new_burst, attribution_merged)

# Combine annotations to vedbas and filter clean bursts
annotations_vedba <- annotations_df %>%
    left_join(
        features_b1_u,
        by = c("Ind_ID", "new_burst")
    ) %>%
    semi_join(
        clean_bursts,
        by = c("Ind_ID", "new_burst" = "burst_id")
    ) %>%
    group_by(attribution_merged) %>%
    filter(
        mean_vedba == max(mean_vedba, na.rm = TRUE) |
        mean_vedba == min(mean_vedba, na.rm = TRUE)
    )

# Highest value is for walking/eating
# Lowest values for grooming reciever, sleeping, resting

# We will choose eating and sleeping
bursts_shortlist <- bind_rows(
    annotations_vedba %>%
        filter(
            attribution_merged == "Eating" & 
            mean_vedba == max(mean_vedba, na.rm = TRUE)
        ) %>%
        select(Ind_ID, new_burst, attribution_merged),
    annotations_vedba %>%
        filter(
            attribution_merged == "Walking" &
                mean_vedba == max(mean_vedba, na.rm = TRUE)
        ) %>%
        select(Ind_ID, new_burst, attribution_merged),
    annotations_vedba %>%
        filter(
            attribution_merged == "Sleeping" &
            mean_vedba == min(mean_vedba, na.rm = TRUE)
        ) %>%
        select(Ind_ID, new_burst, attribution_merged)
)

# Load burst data
b1_uncorrected <- arrow::read_parquet(
    "./data/raw/acc/Burst_1/annotated_acc_burst_1_uncorrected.parquet"
) %>%
    inner_join(
        bursts_shortlist,
        by = c("Ind_ID", "new_burst")
    ) %>%
    pivot_longer(
        cols = c("X", "Y", "Z"),
        names_to = "axis",
        values_to = "acceleration"
    ) %>%
    mutate(
        attribution_merged = factor(
            attribution_merged,
            levels = c("Eating", "Walking", "Sleeping")
        )
    )

## Plot ----
rep_bursts_plot <- ggplot(
    b1_uncorrected,
    aes(
        x = sample_number,
        y = acceleration,
        colour = axis,
        group = axis
    )
) +
    geom_point(
        size = 1, alpha = 0.5
    ) +
    geom_line(
        linewidth = 1, alpha = 0.75
    ) +
    scale_colour_manual(
        values = c("#FF4E50", "#FC913A", "#4AAAA5")
    ) +
    facet_wrap(
        ~attribution_merged,
        nrow = 1
    ) +
    theme_bw() +
    theme(
        panel.grid = element_blank(),
        axis.text = element_text(size = 24, colour = "black"),
        axis.title = element_text(size = 32, colour = "black"),
        strip.text = element_text(size = 24, colour = "black"),
        strip.background = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.3),
        legend.title = element_text(size = 18, colour = "black"),
        legend.text = element_text(size = 18, colour = "black"),
        # panel.border = element_blank(),
    ) +
    labs(
        x = "",
        y = "Acceleration",
        colour = "Axis"
    )
rep_bursts_plot
ggsave(
    "./plots/Fig1_example_behavioural_bursts.svg",
    plot = rep_bursts_plot,
    width = 16, height = 4, dpi = 300
)

# Note that these plots are saved as SVG so that they can be combined into a multipanel figure later.