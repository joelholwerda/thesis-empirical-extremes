# Experiments ------------------------------------------------------------------

# 1. `exp_skew_centre`: Manipulated rank and distance from the average by including an additional pair above or below the shared pair
# 2. `exp_variance`: Manipulated distance from the average by manipulating the variance of each option
# 3. `exp_skew_edges`: Manipulated distance from the average by including outcomes close to the best or worst outcome
# 4. `exp_skew_tokens`: Manipulated token-based extremity while controlling the number of types
# 5. `exp_skew_types`: Manipulated type-based extremity while controlling the number of tokens
# 6. `exp_temporal`: Manipulated alternating or random presentation of gains and losses

# Setup ------------------------------------------------------------------------

set.seed(1234)

library(tidyverse)
library(ggridges)
library(ggstance)
library(scales)
library(cowplot)
library(ggtext)
library(ggh4x)
library(rlang)
library(here)

here::here("src", "utilities", "load_tidy_data.R") %>% source()
here::here("src", "figures", "save_figures.R") %>% source()
here::here("src", "figures", "wrangle_choices_for_figures.R") %>% source()
here::here("src", "figures", "wrangle_ftm_for_figures.R") %>% source()
here::here("src", "figures", "wrangle_percent_for_figures.R") %>% source()

## Load data ----

data_names <-  c(
  "exp_skew_centre", 
  "exp_variance",
  "exp_skew_edges",
  "exp_skew_edges",
  "exp_skew_tokens", 
  "exp_skew_types", 
  "exp_temporal"
)

load_tidy_data(data_names)

## Options ----

save_figures_to_file <- TRUE
figure_width <- 80
figure_height <- c(75, 75, 75, 200)

# Function used to create figures ----------------------------------------------

create_figures <- function(data) {
  
  # Retrieve name of the experiment from the data argument
  experiment_name <- ensym(data) %>% expr_text()
  
  # Indicate experiments that did not have value skewness manipulations
  value <- ifelse(experiment_name == "exp_skew_types", FALSE, TRUE)
  skew <- ifelse(experiment_name %in% c("exp_variance", "exp_temporal"), FALSE, TRUE)
  
  # Wrangle data for figures
  choices_data <- wrangle_choices_for_figures(data$choices_summary, data$exclusions, value, skew)
  ftm_data <- wrangle_ftm_for_figures(data$ftm_recode, data$exclusions, value, skew)
  percent_data <- wrangle_percent_for_figures(data$percent_recode, data$exclusions, value, skew)
  
  # Assign theme elements that are common across figures
  shared_theme <- theme_bw() +
    theme(
      # Remove legend and grid
      legend.position = 'none',
      panel.grid = element_blank(),
      # Change facet colour
      strip.background = element_rect(fill = "#e8e8e8"),
      # Change font sizes
      axis.text = element_text(size = 6),
      axis.text.y = element_text(size = 6),
      axis.title = element_text(size = 6),
      strip.text = element_text(size = 6),
      strip.background.x = element_blank(),
      strip.text.x = element_blank(),
      panel.spacing = unit(4, "pt"),
      plot.margin = margin(2, 2, 2, 2),
      # Edit title appearance with ggtext
      plot.title.position = "plot",
      plot.title = element_textbox_simple(
        size = 8,
        lineheight = 1,
        padding = margin(2, 4, 2, 4),
        margin = margin(0, 0, 4, 0),
        fill = "#e8e8e8"
      )
    )
  
  # Colour blind friendly palette based on cookbook-r.com/Graphs/Colors_(ggplot2)
  shared_colours <- scale_fill_manual(values = c(`Low value` = "#D55E00", `High value` = "#0072B2", `Medium value` = "#009E73",  Extreme = "#009E73", Shared = "#009E73", Better = "#0072B2", Worse = "#56B4E9", Experienced = "#999999", `Not experienced` = "#e8e8e8"))
  
  # Expand the scale for exp_skew_edges because there is only one density plot in the top panels
  if (experiment_name == "exp_skew_edges") {
    scale_expansion <- facetted_pos_scales(
      y = list(
        scale_y_discrete(expand = expansion(add = 1.2)),
        scale_y_discrete(expand = expansion(add = 1.2)),
        scale_y_discrete(expand = expansion(mult = 0.7))
      )
    )
  } else {
    scale_expansion = NULL
  }
  
  # Write the condition names for each experiment to be include in the subfigure titles
  if (experiment_name == "exp_skew_edges") {
    
    condition_phrase <- "High value, Low value, or Shared"
    condition_phrase_colourful <- "<span style = 'color:#0072B2;'>High value</span>, <span style = 'color:#D55E00;'>Low value</span>, or <span style = 'color:#009E73;'>Shared</span>"
    
  } else if (experiment_name == "exp_skew_tokens") {
    
    condition_phrase <- "High value, Medium value, or Low value"
    condition_phrase_colourful <- "<span style = 'color:#0072B2;'>High value</span>, <span style = 'color:#009E73;'>Medium value</span>, or <span style = 'color:#D55E00;'>Low value</span>"
    
  } else if (experiment_name == "exp_skew_types") {
    
    condition_phrase <- "Shared"
    condition_phrase_colourful <- "<span style = 'color:#009E73;'>Shared</span>"
    
  } else {
    
    condition_phrase <- "High value or Low value"
    condition_phrase_colourful <- "<span style = 'color:#0072B2;'>High value</span> or <span style = 'color:#D55E00;'>Low value</span>"
    
  }
  
  # Subfigure titles
  choices_title <- paste("<b>A) Choices</b><br>
      <span style = 'font-size:6pt'>The proportion of choices for the risky option when participants were presented with the", condition_phrase_colourful, "pairs of options.</span>")
  ftm_title <- paste("<b>B) First to mind responses</b><br>
      <span style = 'font-size:6pt'>The proportion of participants that reported the <span style = 'color:#0072B2;'>Better outcome</span> or the <span style = 'color:#56B4E9;'>Worse outcome</span> associated with the", condition_phrase, "options.</span>")
  percent_title <- paste("<b>C) Percentage estimates</b><br>
      <span style = 'font-size:6pt'>The difference between percentage estimates (the better outcome minus the worse outcome) for the", condition_phrase_colourful, "risky options.</span>")
  
  # Create figure for choices data -------------------------------------------------------------------
  choices_figure <- ggplot(choices_data, aes(x = x_variable, y = y_variable, fill = fill)) +
    # Add vertical line at indifference
    geom_vline(xintercept = 0.5, colour = "grey80") +
    # Add density plots
    geom_density_ridges(
      # Make sure ridges don't overlap
      scale = 0.5,
      panel_scaling = FALSE,
      # Remove long tails
      rel_min_height = 0.01,
      # Add rug plot
      jittered_points = TRUE,
      position = position_points_jitter(width = 0.05, height = 0),
      point_shape = '|', point_size = 1, point_alpha = 0.5
    ) +
    # Make sure the jittered rug plot doesn't plot impossible values
    scale_x_continuous(
      limits = c(0, 1), oob = squish,
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      labels = c("0", ".25", ".5", ".75", "1")
    ) +
    # Add white circle for the median
    stat_summary(fun = median, geom = "point", fill = "white", shape = 21, size = 1) +
    # Split by left-skewed, right-skewed, and shared
    facet_grid(rows = vars(facet), scales = "free_y", space = "free_y") +
    scale_expansion +
    # Title and labels
    labs(title = choices_title, x = "Proportion of choices for the risky option", y = NULL) +
    shared_theme +
    shared_colours
  
  # Create figures for first to mind data -------------------------------------------------------------
  ftm_figure <- ggplot(ftm_data, aes(y = y_variable , fill = fill, group = fill)) +
    # Add vertical line at indifference
    geom_vline(data = ftm_data %>% filter(facet_cols == "outcome"), aes(xintercept = 0.5), colour = "grey80") +
    # Add bar plots
    geom_barh(stat = "count", position = position_fill(reverse = TRUE), colour = "black", linewidth = 0.5) +
    scale_x_continuous(
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      labels = c("0", ".25", ".5", ".75", "1")
    ) +
    # Split by left-skewed, right-skewed, and shared (rows); split by outcome and accuracy (columns)
    facet_grid(
      rows = vars(facet_rows), 
      cols = vars(facet_cols),
      scales = "free_y", 
      space = "free_y"
    ) +
    scale_expansion +
    # Title and labels
    labs(
      title = ftm_title, 
      x = paste0(
        "Proportion reported better outcome", 
        paste0(rep(" ", round(figure_width / 80 * 17)), collapse = ""),
        "Accuracy", 
        paste0(rep(" ", round(figure_width / 80 * 22)), collapse = "")
      ),
      y = NULL, 
      fill = NULL
    ) +
    shared_theme +
    shared_colours
  
  # Create figure for percentage estimate data --------------------------------------------------------
  percent_figure <- ggplot(percent_data, aes(x = x_variable, y = y_variable, fill = fill)) +
    # Add vertical line at indifference
    geom_vline(xintercept = 0, colour = "grey80") +
    # Add density plots
    geom_density_ridges(
      # Make sure ridges don't overlap
      scale = 0.5,
      panel_scaling = FALSE,
      # Remove long tails
      rel_min_height = 0.01,
      # Add rug plot
      jittered_points = TRUE,
      position = position_points_jitter(width = 1, height = 0),
      point_shape = '|', point_size = 1, point_alpha = 0.5
    ) +
    # Make sure the jittered rug plot doesn't plot impossible values
    scale_x_continuous(
      limits = c(-100, 100), oob = squish,
      breaks = c(-100, -50, 0, 50, 100),
      labels = c("-100 \n Worse higher", "-50", "0 \n Equal", "50", "100 \n Better higher")
    ) +
    # Add white circle for the median
    stat_summary(fun = median, geom = "point", fill = "white", shape = 21, size = 1) +
    # Split by left-skewed, right-skewed, and shared
    facet_grid(rows = vars(facet), scales = "free_y", space = "free_y") +
    scale_expansion +
    # Title and labels
    labs(title = percent_title, x = "Difference between percentage estimates", y = NULL) +
    shared_theme +
    shared_colours
  
  figures <- list(
    choices = choices_figure,
    ftm = ftm_figure,
    percent = percent_figure,
    combined = plot_grid(choices_figure, ftm_figure, percent_figure, nrow = 3, align = "h", axis = "lr")
  )
  
  return(figures)
  
}

# Create the figures for each experiment ---------------------------------------

exp_skew_centre$figures <- create_figures(exp_skew_centre)
exp_variance$figures <- create_figures(exp_variance)
exp_skew_edges$figures <- create_figures(exp_skew_edges)
exp_skew_tokens$figures <- create_figures(exp_skew_tokens)
exp_skew_types$figures <- create_figures(exp_skew_types)
exp_temporal$figures <- create_figures(exp_temporal)

# Save the figures as .rds files and as PDFs

if (save_figures_to_file) {
  
  save_figures(exp_skew_centre, width = figure_width, height = figure_height)
  save_figures(exp_variance, width = figure_width, height = figure_height * 0.8)
  save_figures(exp_skew_edges, width = figure_width, height = figure_height)
  save_figures(exp_skew_tokens, width = figure_width, height = figure_height)
  save_figures(exp_skew_types, width = figure_width, height = figure_height * 0.5)
  save_figures(exp_temporal, width = figure_width, height = figure_height * 0.8)
  
}
