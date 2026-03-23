## Helper Functions for Graph Creation

##############################################################################################################################
##############################################################################################################################

# Function to pull data from Haver
get_haver_data <- function(series_list, suffix = "USNA", rename_list = NULL) {
  series_code <- paste0(series_list, "@", suffix)
  print(series_code)
  data <- importhaver::import_haver(series = series_code)
  
  # Rename columns if rename_list is provided
  if (!is.null(rename_list)) {
    for (i in seq_along(series_list)) {
      data <- data %>% rename(!!rename_list[i] := series_list[i])
    }
  }
  
  return(data)
}

##############################################################################################################################
##############################################################################################################################

# Calculate year-over-year inflation rates
calc_infl <- function(data, comp_list = get0("components", ifnotfound = NULL), pi_suffix = "_pi", infl_suffix = "_infl") {
  data <- data %>% arrange(date)
  
  # If comp_list is NULL, try to find all columns ending with pi_suffix
  if (is.null(comp_list)) {
    pi_cols <- grep(paste0(pi_suffix, "$"), names(data), value = TRUE)
    comp_list <- sub(pi_suffix, "", pi_cols)
  }
  
  for (comp in comp_list) {
    # Construct column names
    pi_col <- paste0(comp, pi_suffix)
    infl_col <- paste0(comp, infl_suffix)
    
    # Check if pi column exists
    if (pi_col %in% names(data)) {
      # Calculate the year-over-year percentage change
      data <- data %>%
        mutate(!!infl_col := ((get(pi_col) / lag(get(pi_col), 12)) - 1) * 100)
      
      cat("Calculated inflation for:", comp, "\n")
    } else {
      cat("Warning: Missing price index column for", comp, "\n")
    }
  }
  
  return(data)
}


##############################################################################################################################
##############################################################################################################################

# Calculate expenditure shares
calc_shares <- function(data, comp_list = get0("components", ifnotfound = NULL), aggvar = NULL, ne_suffix = "_ne", share_suffix = "_expshare") {
  data <- data %>% arrange(date)
  
  # If comp_list is NULL, try to find all columns ending with ne_suffix
  if (is.null(comp_list)) {
    ne_cols <- grep(paste0(ne_suffix, "$"), names(data), value = TRUE)
    comp_list <- sub(ne_suffix, "", ne_cols)
  }
  
  # Try to infer aggregate variable if not provided
  if (is.null(aggvar)) {
    possible_aggs <- c("ovpce", "corepce", "totpce", "pce", "agg")
    for (agg in possible_aggs) {
      if (paste0(agg, ne_suffix) %in% names(data)) {
        aggvar <- agg
        cat("Using inferred aggregate variable:", aggvar, "\n")
        break
      }
    }
    if (is.null(aggvar)) {
      stop("Could not infer aggregate variable. Please specify 'aggvar'.")
    }
  }
  
  # Check if aggregate column exists
  agg_ne_col <- paste0(aggvar, ne_suffix)
  if (!(agg_ne_col %in% names(data))) {
    stop(paste("Aggregate nominal expenditure column", agg_ne_col, "not found."))
  }
  
  for (comp in comp_list) {
    # Construct column names
    ne_col <- paste0(comp, ne_suffix)
    share_col <- paste0(comp, share_suffix)
    
    # Check if nominal expenditure column exists
    if (ne_col %in% names(data)) {
      # Calculate expenditure share
      data <- data %>%
        mutate(!!share_col := get(ne_col) / get(agg_ne_col))
      
      cat("Calculated expenditure share for:", comp, "\n")
    } else {
      cat("Warning: Missing nominal expenditure column for", comp, "\n")
    }
  }
  
  return(data)
}

##############################################################################################################################
##############################################################################################################################

# Calculate contributions only
calc_contrib <- function(data, comp_list = get0("components", ifnotfound = NULL), infl_suffix = "_infl", share_suffix = "_expshare", contrib_suffix = "_contrib") {
  data <- data %>% arrange(date)
  
  # If comp_list is NULL, try to infer from columns with both inflation and share suffixes
  if (is.null(comp_list)) {
    infl_cols <- grep(paste0(infl_suffix, "$"), names(data), value = TRUE)
    share_cols <- grep(paste0(share_suffix, "$"), names(data), value = TRUE)
    
    infl_comps <- sub(infl_suffix, "", infl_cols)
    share_comps <- sub(share_suffix, "", share_cols)
    
    comp_list <- intersect(infl_comps, share_comps)
    
    if (length(comp_list) == 0) {
      stop("Could not infer component list. Please specify 'comp_list'.")
    }
    
    cat("Using inferred component list:", paste(comp_list, collapse=", "), "\n")
  }
  
  # Calculate contributions for each component
  contrib_cols <- character(length(comp_list))
  
  for (i in seq_along(comp_list)) {
    comp <- comp_list[i]
    
    # Construct column names
    infl_col <- paste0(comp, infl_suffix)
    share_col <- paste0(comp, share_suffix)
    contrib_col <- paste0(comp, contrib_suffix)
    
    # Check if required columns exist
    if (infl_col %in% names(data) && share_col %in% names(data)) {
      # Calculate contribution
      data <- data %>%
        mutate(!!contrib_col := get(share_col) * get(infl_col))
      
      # Store the contribution column name
      contrib_cols[i] <- contrib_col
      
      cat("Calculated contribution for component:", comp, "\n")
    } else {
      missing <- c()
      if (!(infl_col %in% names(data))) missing <- c(missing, infl_col)
      if (!(share_col %in% names(data))) missing <- c(missing, share_col)
      
      cat("Warning: Missing required columns for", comp, ":", paste(missing, collapse=", "), "\n")
      contrib_cols[i] <- NA
    }
  }
  
  # Remove any NA values from contrib_cols
  contrib_cols <- contrib_cols[!is.na(contrib_cols)]
  
  return(data)
}

##############################################################################################################################
##############################################################################################################################

# Function to calculate aggregate from contributions 
calc_aggregate <- function(data, comp_list = get0("components", ifnotfound = NULL), aggvar = "ovpce", agg_suffix = "_infl_est") {
  # Create contribution column names directly from comp_list
  contrib_cols <- paste0(comp_list, "_contrib")
  
  # Check if all contribution columns exist
  if (all(contrib_cols %in% names(data))) {
    # Create the aggregate column name
    agg_col <- paste0(aggvar, agg_suffix)
    
    # Calculate row sums WITHOUT na.rm=TRUE to preserve NAs
    data[[agg_col]] <- rowSums(as.matrix(data[, contrib_cols]), na.rm = FALSE)
  } else {
    missing_cols <- contrib_cols[!contrib_cols %in% names(data)]
    cat("Warning: Some contribution columns are missing:", paste(missing_cols, collapse=", "), "\n")
  }
  
  return(data)
}


##############################################################################################################################
##############################################################################################################################

# Function to prepare data for stacked bar charts (decompositions)
prep_stacked_data <- function(data, contrib_cols, display_names) {
  # Select relevant columns and transform to long format
  contrib_data <- data %>%
    select(date, all_of(contrib_cols)) %>%
    pivot_longer(
      cols = all_of(contrib_cols),
      names_to = "component",
      values_to = "contribution"
    )
  
  # Create mapping for display names
  name_mapping <- setNames(display_names, contrib_cols)
  
  # Map column names to display names using dplyr
  contrib_data <- contrib_data %>%
    mutate(component = case_when(
      component %in% contrib_cols ~ name_mapping[component],
      TRUE ~ component
    ))
  
  return(contrib_data)
}


##############################################################################################################################
##############################################################################################################################

# Corrected plot_line function with fixed y-axis limit calculation and original step sizes
plot_line <- function(
    data,                   # Input dataframe
    y_cols,                 # Named list of columns to plot (names are display names)
    x_col = "date",         # X-axis column (date)
    colors = NULL,          # Named vector of colors matching y_cols names
    title = "",             # Plot title
    subtitle = NULL,        # Optional subtitle
    x_label = "Period",     # X-axis label
    y_label = "Percent",    # Y-axis label
    y_limits = NULL,        # Optional manual y-axis limits
    y_breaks = NULL,        # Optional manual y-axis breaks
    start_date = NULL,      # Start date (if NULL, uses min date in data)
    end_date = NULL,        # End date (if NULL, uses max date in data)
    show_legend = TRUE,     # Whether to show the legend
    legend_position = "bottom", # Position of legend
    line_width = 1          # Width of lines
) {
  require(ggplot2)
  require(dplyr) # For better data filtering

  # Ensure data is properly formatted
  if (!inherits(data[[x_col]], "Date")) {
    stop("x_col must be a Date column")
  }

  # Extract the actual column names from y_cols (the values, not the names)
  column_names <- unlist(y_cols)

  # Check if columns exist in data
  missing_cols <- column_names[!column_names %in% names(data)]
  if (length(missing_cols) > 0) {
    stop(paste("Columns not found in data:", paste(missing_cols, collapse=", ")))
  }

  # Filter out rows with NA values in any of the plotted columns
  complete_data <- data %>%
    filter(if_all(all_of(column_names), ~!is.na(.)))

  # Check if we have any data left after filtering
  if (nrow(complete_data) == 0) {
    stop("No data remains after removing rows with missing values")
  }

  # Further filter data to the specified date range
  if (!is.null(start_date)) {
    complete_data <- complete_data %>% filter(!!sym(x_col) >= start_date)
  }
  if (!is.null(end_date)) {
    complete_data <- complete_data %>% filter(!!sym(x_col) <= end_date)
  }

  # Check again if we have data after date filtering
  if (nrow(complete_data) == 0) {
    stop("No data remains after filtering by date range")
  }

  # Automatically determine date range if not specified
  if (is.null(start_date)) start_date <- min(complete_data[[x_col]], na.rm = TRUE)
  if (is.null(end_date)) end_date <- max(complete_data[[x_col]], na.rm = TRUE)

  # Create initial plot using filtered data
  p <- ggplot(complete_data)

  # Add lines for each column
  for (i in seq_along(y_cols)) {
    col_name <- y_cols[[i]]
    display_name <- names(y_cols)[i]

    p <- p + geom_line(aes_string(x = x_col, y = col_name, color = shQuote(display_name)),
                       linewidth = line_width)
  }

  # Generate date breaks - always use 2-year intervals
  break_interval <- "2 years"

  date_breaks <- seq.Date(
    from = as.Date(paste0(format(start_date, "%Y"), "-01-31")),
    to = end_date,
    by = break_interval
  )

  # Add January of the current year if not already included
  current_year <- as.numeric(format(end_date, "%Y"))
  jan_current_year <- as.Date(paste0(current_year, "-01-31"))
  if (!any(format(date_breaks, "%Y-%m") == format(jan_current_year, "%Y-%m")) &&
      jan_current_year >= start_date && jan_current_year <= end_date) {
    date_breaks <- sort(c(date_breaks, jan_current_year))
  }

  # Add the latest month if not already included
  if (max(date_breaks) < end_date) {
    date_breaks <- sort(c(date_breaks, end_date))
  }

  # Add x-axis formatting
  p <- p + scale_x_date(
    limits = c(start_date, end_date),
    breaks = date_breaks,
    date_labels = "%b %Y",
    expand = c(0, 0)
  )

  # Calculate y-axis range if not specified
  if (is.null(y_limits) || is.null(y_breaks)) {
    # Extract values only for specified date range and columns
    filtered_data <- complete_data %>%
      filter(between(!!sym(x_col), start_date, end_date)) %>%
      select(all_of(column_names))

    # Get min and max values from the filtered data
    y_min <- min(unlist(filtered_data), na.rm = TRUE)
    y_max <- max(unlist(filtered_data), na.rm = TRUE)

    # Determine appropriate step size based on data range - KEEPING ORIGINAL LOGIC
    data_range <- y_max - y_min

    step <- if (data_range <= 2) {
      0.1  # For very small ranges (≤2), use 0.25 increments
    } else if (data_range <= 5) {
      0.5   # For small ranges (≤5), use 0.5 increments
    } else if (data_range <= 20) {
      1     # For medium-small ranges (≤10), use 1 increment
    } else if (data_range <= 30) {
      2     # For medium ranges (≤20), use 2 increments
    } else if (data_range <= 50) {
      5     # For large ranges (≤50), use 5 increments
    } else if (data_range <= 100) {
      10    # For very large ranges (>50), use 10 increments
    } else {
      20
    }

    # Calculate min and max that align with step
    if (is.null(y_limits)) {
      y_min <- floor(y_min / step) * step
      y_max <- ceiling(y_max / step) * step
      y_limits <- c(y_min, y_max)
    }

    # Create breaks if not specified
    if (is.null(y_breaks)) {
      y_breaks <- seq(y_limits[1], y_limits[2], by = step)
    }
  }

  # Add y-axis formatting
  p <- p + scale_y_continuous(
    limits = y_limits,
    breaks = y_breaks,
    expand = c(0, 0)
  )

  # Set theme and colors
  p <- p + theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
      legend.position = if (show_legend) legend_position else "none",
      legend.title = element_blank(),
      plot.title = element_text(size = 12, hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "gray90"),
      panel.grid.major.y = element_line(color = "gray90")
    )

  # Add title and labels
  p <- p + labs(
    title = title,
    subtitle = subtitle,
    x = x_label,
    y = y_label
  )

  # Add manual color scale if provided - with explicit breaks to control legend order
  if (!is.null(colors)) {
    p <- p + scale_color_manual(
      values = colors,
      breaks = names(y_cols)  # This ensures legend items appear in same order as y_cols
    )
  }

  return(p)
}


plot_line_year <- function(
    data,                   # Input dataframe
    y_cols,                 # Named list of columns to plot (names are display names)
    x_col = "year",         # X-axis column (year)
    colors = NULL,          # Named vector of colors matching y_cols names
    title = "",             # Plot title
    subtitle = NULL,        # Optional subtitle
    x_label = "Period",     # X-axis label
    y_label = "Percent",    # Y-axis label
    y_limits = NULL,        # Optional manual y-axis limits
    y_breaks = NULL,        # Optional manual y-axis breaks
    x_breaks = NULL,        # Optional manual x-axis breaks (specific years)
    start_date = NULL,      # Start year (if NULL, uses min year in data)
    end_date = NULL,        # End year (if NULL, uses max year in data)
    show_legend = TRUE,     # Whether to show the legend
    legend_position = "bottom", # Position of legend
    line_width = 1          # Width of lines
) {
  require(ggplot2)
  require(dplyr)
  
  # Ensure data is properly formatted - CHANGED
  if (!is.numeric(data[[x_col]]) && !is.character(data[[x_col]])) {
    stop("x_col must be a numeric or character year column")
  }
  
  # Convert to numeric if character - ADDED
  if (is.character(data[[x_col]])) {
    data[[x_col]] <- as.numeric(data[[x_col]])
  }
  
  # Extract the actual column names from y_cols (the values, not the names)
  column_names <- unlist(y_cols)
  
  # Check if columns exist in data
  missing_cols <- column_names[!column_names %in% names(data)]
  if (length(missing_cols) > 0) {
    stop(paste("Columns not found in data:", paste(missing_cols, collapse=", ")))
  }
  
  # Filter out rows with NA values in any of the plotted columns
  complete_data <- data %>%
    filter(if_all(all_of(column_names), ~!is.na(.)))
  
  # Check if we have any data left after filtering
  if (nrow(complete_data) == 0) {
    stop("No data remains after removing rows with missing values")
  }
  
  # Further filter data to the specified date range
  if (!is.null(start_date)) {
    complete_data <- complete_data %>% filter(!!sym(x_col) >= start_date)
  }
  if (!is.null(end_date)) {
    complete_data <- complete_data %>% filter(!!sym(x_col) <= end_date)
  }
  
  # Check again if we have data after date filtering
  if (nrow(complete_data) == 0) {
    stop("No data remains after filtering by date range")
  }
  
  # Automatically determine date range if not specified
  if (is.null(start_date)) start_date <- min(complete_data[[x_col]], na.rm = TRUE)
  if (is.null(end_date)) end_date <- max(complete_data[[x_col]], na.rm = TRUE)
  
  # Create initial plot using filtered data
  p <- ggplot(complete_data)
  
  # Add lines for each column
  for (i in seq_along(y_cols)) {
    col_name <- y_cols[[i]]
    display_name <- names(y_cols)[i]
    
    p <- p + geom_line(aes_string(x = x_col, y = col_name, color = shQuote(display_name)),
                       linewidth = line_width)
  }
  
  # Generate year breaks - CHANGED ENTIRE SECTION
  if (!is.null(x_breaks)) {
    year_breaks <- x_breaks
  } else {
    year_range <- end_date - start_date
    
    break_interval <- if (year_range <= 10) {
      1
    } else if (year_range <= 30) {
      2
    } else if (year_range <= 50) {
      5
    } else {
      10
    }
    
    year_breaks <- seq(
      from = ceiling(start_date / break_interval) * break_interval,
      to = floor(end_date / break_interval) * break_interval,
      by = break_interval
    )
    
    if (!any(year_breaks == end_date)) {
      year_breaks <- c(year_breaks, end_date)
    }
  }
  
  # Add x-axis formatting - CHANGED
  p <- p + scale_x_continuous(
    limits = c(start_date, end_date),
    breaks = year_breaks,
    expand = c(0, 0)
  )
  
  # Calculate y-axis range if not specified
  if (is.null(y_limits) || is.null(y_breaks)) {
    # Extract values only for specified date range and columns
    filtered_data <- complete_data %>%
      filter(between(!!sym(x_col), start_date, end_date)) %>%
      select(all_of(column_names))
    
    # Get min and max values from the filtered data
    y_min <- min(unlist(filtered_data), na.rm = TRUE)
    y_max <- max(unlist(filtered_data), na.rm = TRUE)
    
    # Determine appropriate step size based on data range - KEEPING ORIGINAL LOGIC
    data_range <- y_max - y_min
    
    step <- if (data_range <= 2) {
      0.1  # For very small ranges (≤2), use 0.25 increments
    } else if (data_range <= 5) {
      0.5   # For small ranges (≤5), use 0.5 increments
    } else if (data_range <= 20) {
      1     # For medium-small ranges (≤10), use 1 increment
    } else if (data_range <= 30) {
      2     # For medium ranges (≤20), use 2 increments
    } else if (data_range <= 50) {
      5     # For large ranges (≤50), use 5 increments
    } else if (data_range <= 100) {
      10    # For very large ranges (>50), use 10 increments
    } else {
      20
    }
    
    # Calculate min and max that align with step
    if (is.null(y_limits)) {
      y_min <- floor(y_min / step) * step
      y_max <- ceiling(y_max / step) * step
      y_limits <- c(y_min, y_max)
    }
    
    # Create breaks if not specified
    if (is.null(y_breaks)) {
      y_breaks <- seq(y_limits[1], y_limits[2], by = step)
    }
  }
  
  # Add y-axis formatting
  p <- p + scale_y_continuous(
    limits = y_limits,
    breaks = y_breaks,
    expand = c(0, 0)
  )
  
  # Set theme and colors - UNCHANGED
  p <- p + theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
      legend.position = if (show_legend) legend_position else "none",
      legend.title = element_blank(),
      plot.title = element_text(size = 12, hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "gray90"),
      panel.grid.major.y = element_line(color = "gray90")
    )
  
  # Add title and labels
  p <- p + labs(
    title = title,
    subtitle = subtitle,
    x = x_label,
    y = y_label
  )
  
  # Add manual color scale if provided - with explicit breaks to control legend order
  if (!is.null(colors)) {
    p <- p + scale_color_manual(
      values = colors,
      breaks = names(y_cols)  # This ensures legend items appear in same order as y_cols
    )
  }
  
  return(p)
}



##############################################################################################################################
##############################################################################################################################
# Plot the Decomposition
plot_decomp <- function(data,
                        component_order = NULL,
                        colors = NULL,
                        title = "",
                        y_limits = NULL,
                        y_breaks = NULL,
                        start_date = NULL,
                        end_date = NULL) {
  require(ggplot2)
  
  # Ensure we have required columns
  if (!all(c("date", "component", "contribution") %in% names(data))) {
    stop("Data must contain columns named: date, component, contribution")
  }
  
  # Remove rows with NA or non-finite values
  plot_data <- data[!is.na(data$contribution) & is.finite(data$contribution), ]
  
  # Find valid date range
  if (nrow(plot_data) == 0) {
    stop("No valid data points found")
  }
  
  # Determine valid date range
  valid_start <- min(plot_data$date, na.rm = TRUE)
  valid_end <- max(plot_data$date, na.rm = TRUE)
  
  # Set default date range using valid dates
  if (is.null(start_date)) {
    start_date <- valid_start
  } else {
    # If provided start_date is earlier than first valid date, adjust it
    start_date <- max(start_date, valid_start)
  }
  
  if (is.null(end_date)) {
    end_date <- valid_end
  }
  
  # Filter data to the date range
  plot_data <- subset(plot_data, date >= start_date & date <= end_date)
  
  # Apply component ordering if provided
  if (!is.null(component_order)) {
    plot_data$component <- factor(plot_data$component, levels = component_order)
  }
  
  # Calculate EXACT min/max values needed for the plot
  
  # For individual components
  min_comp <- min(plot_data$contribution, na.rm = TRUE)
  max_comp <- max(plot_data$contribution, na.rm = TRUE)
  
  # For stacked totals by date
  totals <- aggregate(contribution ~ date, plot_data, sum)
  min_total <- min(totals$contribution, na.rm = TRUE)
  max_total <- max(totals$contribution, na.rm = TRUE)
  
  # Find the actual min/max needed for the data
  actual_min <- min(0, min_comp, min_total)
  actual_max <- max(0, max_comp, max_total)
  
  # Print the actual data range for debugging
  cat("Actual data range:", actual_min, "to", actual_max, "\n")
  
  # Get raw y-limits (either provided or based on data)
  if (is.null(y_limits)) {
    # Start with actual data range
    raw_y_min <- actual_min
    raw_y_max <- actual_max
    
    # Add very minimal padding only if needed
    if (raw_y_min == raw_y_max) {
      # Handle edge case of a flat line
      raw_y_min <- raw_y_min - 0.5
      raw_y_max <- raw_y_max + 0.5
    }
  } else {
    # Use provided y_limits
    raw_y_min <- y_limits[1]
    raw_y_max <- y_limits[2]
  }
  
  # Determine appropriate step size based on the data range
  range <- raw_y_max - raw_y_min
  
  step <- if (range <= 5) {
    0.5  # For very small ranges (≤5), use 0.5 increments
  } else if (range <= 20) {
    1    # For small ranges (≤10), use 1 increment  
  } else if (range <= 50) {
    5    # For large ranges (≤50), use 5 increments
  } else {
    10   # For very large ranges (>50), use 10 increments
  }
  
  # Calculate aligned limits - ensure they fully contain the data range
  aligned_y_min <- floor(raw_y_min / step) * step
  aligned_y_max <- ceiling(raw_y_max / step) * step
  
  # Special case: if actual min is very close to 0, use 0 as the lower limit
  if (actual_min > -0.1 && actual_min < 0.1) {
    aligned_y_min <- 0
  }
  
  # Now set the final y-limits
  final_y_limits <- c(aligned_y_min, aligned_y_max)
  
  # Print the calculated limits for debugging
  cat("Calculated y-limits:", final_y_limits[1], "to", final_y_limits[2], "\n")
  cat("Using step size:", step, "\n")
  
  # Set y_breaks based on the aligned limits and step
  if (is.null(y_breaks)) {
    y_breaks <- seq(final_y_limits[1], final_y_limits[2], by = step)
  }
  
  # Generate date breaks (always 2 years)
  date_breaks <- seq.Date(
    from = as.Date(paste0(format(start_date, "%Y"), "-01-31")),
    to = end_date,
    by = "2 years"
  )
  
  # Ensure the latest month is included in breaks
  if (!end_date %in% date_breaks) {
    date_breaks <- sort(c(date_breaks, end_date))
  }
  
  # Add January of the current year if not already included
  current_year <- as.numeric(format(end_date, "%Y"))
  jan_current_year <- as.Date(paste0(current_year, "-01-31"))
  if (!any(format(date_breaks, "%Y-%m") == format(jan_current_year, "%Y-%m")) &&
      jan_current_year >= start_date && jan_current_year <= end_date) {
    date_breaks <- sort(c(date_breaks, jan_current_year))
  }
  
  # Create the basic plot
  p <- ggplot(plot_data) +
    geom_area(aes(x = date, y = contribution, fill = component), position = "stack")
  
  # Add colors if provided
  if (!is.null(colors)) {
    p <- p + scale_fill_manual(values = colors)
  }
  
  # Complete the plot with formatting
  p <- p +
    scale_x_date(
      limits = c(start_date, end_date),
      breaks = date_breaks,
      date_labels = "%b %Y",
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = final_y_limits,
      breaks = y_breaks,
      expand = c(0, 0)
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.title = element_text(size = 12, hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "gray90")
    ) +
    labs(
      title = title,
      x = "Period",
      y = "Percentage Points"
    ) +
    coord_cartesian(clip = "off")
  
  return(p)
}

###############################################################################################################################
###############################################################################################################################

# Specific function to generate these one time plots
plot_price_indices <- function(
    series_data,           # Data frame with code, name, display, color columns
    index_type = "PCE",    # Type of index: "PCE", "PPI", or "CPI" for all series
    haver_suffix = NULL,   # Database suffix for Haver Analytics (optional override)
    save_path = "."        # Where to save plots
) {
  results <- list()
  
  # Set appropriate labels and default database based on index type
  index_type <- toupper(index_type)
  if (index_type == "PPI") {
    index_label <- "Producer Price Index"
    infl_label <- "Producer Price Inflation"
    base_year_label <- ""  # Remove 2017=100 label for PPI
    if (is.null(haver_suffix)) haver_suffix <- "PPIR"
  } else if (index_type == "CPI") {
    index_label <- "Consumer Price Index"
    infl_label <- "Consumer Price Inflation"
    base_year_label <- ""  # Remove 2017=100 label for CPI
    if (is.null(haver_suffix)) haver_suffix <- "CPIDATA"
  } else {
    index_label <- "PCE Price Index"
    infl_label <- "Inflation"
    base_year_label <- "\n2017=100"  # Keep for PCE
    if (is.null(haver_suffix)) haver_suffix <- "USNA"
  }
  
  for (i in 1:nrow(series_data)) {
    series_code <- series_data$code[i]
    series_name <- series_data$name[i]
    display_name <- series_data$display[i]
    color <- series_data$color[i]
    
    cat("Processing", display_name, "...\n")
    
    # Step 1: Get data from Haver with appropriate suffix
    renamed_col <- paste0(series_name, "_pi")
    data <- get_haver_data(
      series_list = series_code, 
      rename_list = renamed_col,
      suffix = haver_suffix
    )
    
    # Determine the earliest date with valid data
    valid_data <- data[!is.na(data[[renamed_col]]), ]
    if (nrow(valid_data) == 0) {
      cat("Warning: No valid data found for", series_name, "\n")
      next
    }
    earliest_date <- min(valid_data$date, na.rm = TRUE)
    
    # Step 2: Create full history plot
    full_plot <- plot_line(
      data = data,
      y_cols = setNames(list(renamed_col), paste0(display_name, " price index")),
      colors = setNames(c(color), paste0(display_name, " price index")),
      title = paste0(display_name, " ", index_label, " Value", base_year_label),
      y_label = "Index Value",
      show_legend = FALSE
    )
    
    filename <- paste0(series_name, "_", tolower(index_type), ".png")
    save_plot(full_plot, file.path(save_path, filename))
    
    # Step 3: Create recent history plot
    # Use the later of 2000-01-31 or the earliest valid date
    recent_start_date <- max(as.Date("2000-01-31"), earliest_date)
    
    recent_plot <- plot_line(
      data = data,
      y_cols = setNames(list(renamed_col), paste0(display_name, " price index")),
      colors = setNames(c(color), paste0(display_name, " price index")),
      title = paste0(display_name, " ", index_label, " Value Since ", format(recent_start_date, "%Y"), base_year_label),
      y_label = "Index Value",
      show_legend = FALSE,
      start_date = recent_start_date
    )
    
    filename <- paste0(series_name, "_", tolower(index_type), "_2000.png")
    save_plot(recent_plot, file.path(save_path, filename))
    
    # Step 4: Calculate inflation
    comp_list <- series_name  # Set comp_list for calc_infl
    data <- calc_infl(data)
    
    # Step 5: Create inflation plot
    infl_col <- paste0(series_name, "_infl")
    
    # Check if inflation column exists
    if (!infl_col %in% names(data)) {
      cat("Warning: Column", infl_col, "not found in data. Skipping inflation plot for", series_name, "\n")
      next
    }
    
    # Determine the earliest date with valid inflation data
    valid_infl_data <- data[!is.na(data[[infl_col]]), ]
    if (nrow(valid_infl_data) == 0) {
      cat("Warning: No valid inflation data found for", series_name, "\n")
      next
    }
    earliest_infl_date <- min(valid_infl_data$date, na.rm = TRUE)
    
    # Use the later of 2000-01-31 or the earliest valid inflation date
    infl_start_date <- max(as.Date("2000-01-31"), earliest_infl_date)
    
    infl_plot <- plot_line(
      data = data,
      y_cols = setNames(list(infl_col), paste0(display_name, " ", infl_label)),
      colors = setNames(c(color), paste0(display_name, " ", infl_label)),
      title = paste0(display_name, " ", infl_label, " Since ", format(infl_start_date, "%Y")),
      y_label = "Percent",
      show_legend = FALSE,
      start_date = infl_start_date
    )
    
    filename <- paste0(series_name, "_", tolower(index_type), "_infl_2000.png")
    save_plot(infl_plot, file.path(save_path, filename))
    
    # Store results
    results[[series_name]] <- list(
      full_plot = full_plot,
      recent_plot = recent_plot,
      infl_plot = infl_plot,
      data = data
    )
    
    cat("Completed", display_name, "\n\n")
  }
  
  return(results)
}


# ##############################################################################################################################
# ##############################################################################################################################

# Save plots with consistent settings using specific default path
save_plot <- function(plot, filename, 
                      path = here::here("Healthcare PCE/graphs"), 
                      width = 10, height = 6, dpi = 300) {
  
  # Create the directory if it doesn't exist
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
    cat("Created directory:", path, "\n")
  }
  
  # Combine path and filename
  full_path <- file.path(path, filename)
  
  # Save the plot
  ggsave(full_path, plot = plot, width = width, height = height, dpi = dpi)
  
  # Print confirmation message
  cat("Plot saved to:", full_path, "\n")
  
  # Return the full path to the saved file
  return(invisible(full_path))
}
