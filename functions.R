
#variables

vc_pal<-c("#ca5733","#68a6d5", "#3b914e","#fbc61d")


#===============================================================================
# ---- Install Required Packages ----
required_packages <- c(
  "sf", "ggplot2", "dplyr", "scales", "classInt", "stringr",
  "here", "ggspatial", "MetBrewer", "leaflet", "htmlwidgets"
)

# Install any that are missing
installed <- rownames(installed.packages())
to_install <- setdiff(required_packages, installed)
if (length(to_install) > 0) {
  install.packages(to_install)
}


library(here)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)
library(grid)

#use the overwrite argument as needed

cache_or_compute <- function(object_name, compute_expr, cache_dir = here::here("cache"), overwrite = FALSE) {
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  cache_file <- file.path(cache_dir, paste0(object_name, ".rds"))
  
  if (file.exists(cache_file) && !overwrite) {
    message("✅ Loaded from cache: ", object_name)
    readRDS(cache_file)
  } else {
    message("⚙️  Computing and caching: ", object_name)
    result <- compute_expr
    saveRDS(result, cache_file)
    message("💾 Saved to cache: ", cache_file)
    result
  }
}

#===============================================================================


#' Extract a zip file with base R unzip() and PowerShell fallback (Windows only)
#' 
#' @param zip_path Path to the zip file (relative or absolute)
#' @param output_dir Folder to extract to (default is "temp_unzipped" in project root)
#' 
extract_zip_with_fallback <- function(zip_path, output_dir = here::here("temp_unzipped")) {
  zip_path <- normalizePath(zip_path, mustWork = FALSE)
  output_dir <- normalizePath(output_dir, mustWork = FALSE)
  
  if (!file.exists(zip_path)) {
    stop("❌ Zip file does not exist at: ", zip_path)
  }
  
  tryCatch({
    unzip(zip_path, exdir = output_dir)
    TRUE
  }, warning = function(w) {
    message("⚠️ Base unzip failed, falling back to PowerShell...")
    shell(sprintf(
      'powershell -Command "Expand-Archive -Path %s -DestinationPath %s -Force"',
      shQuote(zip_path), shQuote(output_dir)
    ))
    TRUE
  }, error = function(e) {
    stop("❌ Unzip failed completely: ", e$message)
  })
  
  # Return the shapefile path directly for immediate use, then auto-clean
  shp_path <- list.files(output_dir, pattern = "\\.shp$", full.names = TRUE)[1]
  
  if (is.na(shp_path)) {
    stop("❌ No .shp file found in the extracted ZIP.")
  }
  
  # Read shapefile
  shp_data <- st_read(shp_path, quiet = TRUE)
  
  # Delete extracted temp folder after reading
  unlink(output_dir, recursive = TRUE)
  
  return(shp_data)
}

#===============================================================================


#A wrapper to print remove census variables due to a specified threshold of rows in a variable column with relative MOE's >.5
#input 1 is a raw census data output with E and M for error column designation
#sapply hits all coumns and cluculates for each row the rel value into a new column. if the rel value is >.5 it retruens True. At the end, if >50% of rows have rel >. 5 the variable is not reliable.
#data final is the renamed variables object (or potentially a look up tool) that can be matched to get the text for the variable that is removed.
#Takes a renamed and original dataframe

#IT returns what can be mutated still for the final dataset's function

##Takes the rel threshold as an argument**key
# === Function 1: Remove Variables with High Relative MOE ===
remove_high_moe <- function(original_df, renamed_df, rel_threshold = 0.1, derived_vars_used = NULL) {
  estimate_columns <- grep("E$", names(original_df), value = TRUE)
  
  rel_moe <- sapply(estimate_columns, function(est_col) {
    moe_col <- sub("E$", "M", est_col)
    if (moe_col %in% names(original_df)) {
      rel <- original_df[[moe_col]] / original_df[[est_col]]
      return(mean(rel > rel_threshold, na.rm = TRUE))
    }
    return(NA)
  })
  
  vars_to_remove <- names(rel_moe[rel_moe > 0.5])
  moe_to_remove <- sub("E$", "M", vars_to_remove)
  readable_removed <- names(renamed_df)[match(vars_to_remove, names(original_df))]
  cleaned_df <- original_df[ , !(names(original_df) %in% c(vars_to_remove, moe_to_remove))]
  
  cat(sprintf("✅ Removed %d variables due to high relative MOE (>50%% of rows > %.0f%%):\n", length(readable_removed), rel_threshold * 100))
  print(readable_removed)
  
  missing_inputs <- NULL
  if (!is.null(derived_vars_used)) {
    missing_inputs <- derived_vars_used[!derived_vars_used %in% names(cleaned_df)]
    if (length(missing_inputs) > 0) {
      cat("\n⚠️ Missing inputs for derived metrics:\n")
      print(missing_inputs)
    }
  }
  
  return(list(
    cleaned_df = cleaned_df,
    removed_vars = readable_removed,
    missing_inputs = missing_inputs
  ))
}

# === Function 2: Safely Add Derived Metrics ===
safely_add_derived_metrics <- function(df, available_vars) {
  var_ok <- function(vars) all(vars %in% available_vars)
  
  df %>%
    mutate(
      income_75k_plus = if (var_ok(c("income_75k_99k", "income_100k_124k", "income_125k_149k", "income_150k_199k", "income_200k_plus"))) income_75k_99k + income_100k_124k + income_125k_149k + income_150k_199k + income_200k_plus else NA,
      income_125k_plus = if (var_ok(c("income_125k_149k", "income_150k_199k", "income_200k_plus"))) income_125k_149k + income_150k_199k + income_200k_plus else NA,
      pct_income_75k_plus = if (var_ok(c("income_75k_plus", "aggregate_household_income"))) income_75k_plus / aggregate_household_income * 100 else NA,
      pct_income_125k_plus = if (var_ok(c("income_125k_plus", "aggregate_household_income"))) income_125k_plus / aggregate_household_income * 100 else NA,
      poc_total = if (var_ok(c("black_alone", "two_or_more_races", "hispanic_or_latino"))) black_alone + two_or_more_races + hispanic_or_latino else NA,
      pct_white = if (var_ok(c("white_alone", "total_population"))) white_alone / total_population * 100 else NA,
      pct_asian = if (var_ok(c("asian_alone", "total_population"))) asian_alone / total_population * 100 else NA,
      pct_poc = if (var_ok(c("poc_total", "total_population"))) poc_total / total_population * 100 else NA,
      bachelors_only = if (var_ok("bachelors_degree")) bachelors_degree else NA,
      grad_plus = if (var_ok(c("masters_degree", "professional_degree", "doctorate"))) rowSums(across(c(masters_degree, professional_degree, doctorate)), na.rm = TRUE) else NA,
      pct_bachelors = if (var_ok(c("bachelors_only", "total_population"))) bachelors_only / total_population * 100 else NA,
      pct_gradplus = if (var_ok(c("grad_plus", "total_population"))) grad_plus / total_population * 100 else NA,
      living_alone_total = if (var_ok(c("male_living_alone", "female_living_alone"))) rowSums(across(c(male_living_alone, female_living_alone)), na.rm = TRUE) else NA,
      pct_living_alone = if (var_ok(c("living_alone_total", "total_population"))) living_alone_total / total_population * 100 else NA,
      male_30_54 = if (var_ok(grep("^male_.*(never|sep|wid|div)$", names(df), value = TRUE))) rowSums(across(grep("^male_.*(never|sep|wid|div)$", names(df), value = TRUE)), na.rm = TRUE) else NA,
      female_30_54 = if (var_ok(grep("^female_.*(never|sep|wid|div)$", names(df), value = TRUE))) rowSums(across(grep("^female_.*(never|sep|wid|div)$", names(df), value = TRUE)), na.rm = TRUE) else NA,
      pct_male_30_54 = if (var_ok(c("male_30_54", "total_population"))) male_30_54 / total_population * 100 else NA,
      pct_female_30_54 = if (var_ok(c("female_30_54", "total_population"))) female_30_54 / total_population * 100 else NA,
      combined_singles = if (var_ok(c("female_30_54", "male_30_54"))) female_30_54 + male_30_54 else NA,
      combined_percent_singles = if (var_ok(c("pct_male_30_54", "pct_female_30_54"))) pct_male_30_54 + pct_female_30_54 else NA
    )
}
#This returns a cleaned dataset, and a list of the variabels removed
#=============================

#TMAP THEME

tmap_theme_custom <- function(title_size = 16, base_size = 12) {
  tm_layout(
    main.title.size = title_size,
    fontfamily = "sans",
    legend.text.size = base_size,
    legend.title.size = base_size + 1,
    frame = TRUE,
    title.position = c("center", "top"),
    legend.position = c("right", "bottom"),
    inner.margins = c(0.05, 0.05, 0.05, 0.05),
    panel.label.size = 1.2,
    panel.label.bg.color = "white"
  )
}



#===============================================================================
#===============================================================================

#change end place if neccesary before referencing
  
#wrapper function to generate pdfs of histograms and scatter plots for an analysis
generate_data_summary_pdf_safe <- function(df, filename = "eda_output.pdf", output_dir = "outputs/pdf", overwrite = FALSE) {
  library(ggplot2)
  library(gridExtra)
  library(grid)
  library(dplyr)
  library(tidyr)
  library(sf)
  
  # === Cleaning Step ===
  df <- df %>% select(-matches("margin|M$"))  # Remove margin/MoE columns
  
  if ("geometry" %in% names(df)) {
    geom_index <- which(names(df) == "geometry")
    df <- df[, seq_len(geom_index - 1), drop = FALSE]
  }
  
  df <- sf::st_drop_geometry(df)  # Drop sf geometry attribute if present
  
  # === Keep only numeric columns ===
  num_vars <- names(df)[sapply(df, is.numeric)]
  df <- df[, num_vars, drop = FALSE]
  
  if (length(num_vars) == 0) {
    warning("⚠️ No numeric variables found for analysis.")
    return(NULL)
  }
  
  # === Set up output path ===
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  output_path <- file.path(output_dir, filename)
  
  if (file.exists(output_path) && !overwrite) {
    message("❌ File already exists and overwrite = FALSE. Skipping PDF generation.")
    return(NULL)
  }
  
  pdf_started <- FALSE
  
  tryCatch({
    pdf(output_path, width = 11, height = 8.5)
    pdf_started <- TRUE
    
    # === Histograms, Boxplots, Density Plots ===
    for (var in num_vars) {
      plots <- list()
      
      hist <- tryCatch(
        ggplot(df, aes(x = .data[[var]])) +
          geom_histogram(fill = "steelblue", color = "white", bins = 30) +
          ggtitle(paste("Histogram of", var)),
        error = function(e) NULL
      )
      
      box <- tryCatch(
        ggplot(df, aes(y = .data[[var]])) +
          geom_boxplot(fill = "tomato", color = "black") +
          ggtitle(paste("Boxplot of", var)),
        error = function(e) NULL
      )
      
      dens <- tryCatch(
        ggplot(df, aes(x = .data[[var]])) +
          geom_density(fill = "forestgreen", alpha = 0.5) +
          ggtitle(paste("Density of", var)),
        error = function(e) NULL
      )
      
      plots <- Filter(Negate(is.null), list(hist, box, dens))
      
      if (length(plots) > 0) {
        grid.arrange(grobs = plots, ncol = length(plots))
      }
    }
    
    # === Scatter Plots ===
    scatter_plots <- list()
    if (length(num_vars) > 1) {
      combos <- combn(num_vars, 2, simplify = FALSE)
      for (pair in combos) {
        p <- tryCatch(
          ggplot(df, aes(x = .data[[pair[1]]], y = .data[[pair[2]]])) +
            geom_point(alpha = 0.6) +
            ggtitle(paste(pair[1], "vs", pair[2])),
          error = function(e) NULL
        )
        if (!is.null(p)) scatter_plots[[length(scatter_plots) + 1]] <- p
      }
    }
    
    if (length(scatter_plots) > 0) {
      for (i in seq(1, length(scatter_plots), by = 2)) {
        plots <- scatter_plots[i:min(i + 1, length(scatter_plots))]
        grid.arrange(grobs = plots, ncol = 2)
      }
    } else {
      message("ℹ️ No scatterplots to display.")
    }
    
    # === Summary Table ===
    summary_df <- df %>%
      summarise(across(everything(), list(mean = ~mean(.x, na.rm = TRUE),
                                          sd = ~sd(.x, na.rm = TRUE)),
                       .names = "{.fn}||{.col}")) %>%
      pivot_longer(everything(),
                   names_to = c("Stat", "Variable"),
                   names_sep = "\\|\\|") %>%
      pivot_wider(names_from = Stat, values_from = value) %>%
      select(Variable, mean, sd)
    
    grid.newpage()
    grid.text("Summary Table: Mean and SD", x = 0.5, y = 0.95, gp = gpar(fontsize = 14))
    grid.table(summary_df)
    
    cat("✅ PDF saved to:", output_path, "\n")
    
  }, finally = {
    if (pdf_started) dev.off()
  })
}



#================================================================================
#================================================================================
#This function is important part of pre-model/spatial statistics portion of the process.
##It takes a dataframe and then, based on the neccesary cleaning transformation runs functions on designated columns. The columns are designated as vectors.
#Base
clean_acs_data <- function(df,
                           log_vars = NULL,
                           clip_vars = NULL,
                           normalize_vars = NULL,
                           remove_vars = NULL,
                           population_var = "total_population") {
  library(dplyr)
  library(rlang)
  
  # --- Log Transform Safely ---
  log_transform <- function(df, vars) {
    for (v in vars) {
      if (v %in% names(df)) {
        df[[paste0("log_", v)]] <- log1p(df[[v]]) #assigns vallue of element in variable of data frame to the log column of the variable. creates column and assigns it value as it iterates. gives the value to each element in that var
      }
    }
    df
  }
  
  # --- Clip top 5%, preserving row ---
  clip_top_5 <- function(df, vars) {
    for (v in vars) {
      if (v %in% names(df)) {
        upper <- quantile(df[[v]], 0.95, na.rm = TRUE)
        df[[paste0(v, "_clipped")]] <- pmin(df[[v]], upper)
      }
    }
    df
  }
  
  # --- Normalize by population, preserving NA where population is 0 or NA ---
  normalize_by_population <- function(df, vars, pop_col) {
    for (v in vars) {
      if (v %in% names(df)) {
        new_var <- paste0(v, "_pct")
        df[[new_var]] <- ifelse(df[[pop_col]] > 0, df[[v]] / df[[pop_col]], NA)
      }
    }
    df
  }
  
  # --- Apply transformations ---
  df <- df %>%
    log_transform(log_vars) %>%
    clip_top_5(clip_vars) %>%
    normalize_by_population(normalize_vars, population_var)
  
  # --- Educational attainment variable ---
  if (all(c("bachelors_degree", "masters_degree") %in% names(df))) {
    df$edu_attain_pct <- ifelse(df[[population_var]] > 0,
                                (df$bachelors_degree + df$masters_degree) / df[[population_var]],
                                NA)
  }
  
  # --- Clean suspicious inputs (set to NA, don't drop) ---
  if ("male_30_54" %in% names(df)) {
    df$male_30_54 <- ifelse(df$male_30_54 <= 0 | is.na(df$male_30_54), NA, df$male_30_54)
  }
  if ("female_30_54" %in% names(df)) {
    df$female_30_54 <- ifelse(df$female_30_54 <= 0 | is.na(df$female_30_54), NA, df$female_30_54)
  }
  
  # --- Remove variables (if requested) ---
  if (!is.null(remove_vars)) {
    df <- df %>% select(-any_of(remove_vars))
  }
  
  return(df)
}


#=================================================================

#=================================================================
#This code generates chloropleths using jenks analysis of numeric variables
#consider reviewing
# ---- COMBINED STATIC & INTERACTIVE CHOROPLETH GENERATOR ----

generate_choropleth_maps <- function(geodata,
                                     site_points,
                                     water,
                                     roads,
                                     cbsa_boundary,
                                     site_label_col = "home",
                                     img_dir = here::here("clients", "arshad", "outputs", "img"),
                                     pdf_dir = here::here("clients", "arshad", "outputs", "pdf", "choropleth"),
                                     html_dir = here::here("clients", "arshad", "outputs", "html"),
                                     width = 12, height = 8, dpi = 300, overwrite = TRUE,
                                     run_leaflet = TRUE) {
  library(sf)
  library(ggplot2)
  library(dplyr)
  library(scales)
  library(classInt)
  library(stringr)
  library(here)
  library(ggspatial)
  library(MetBrewer)
  library(leaflet)
  library(htmlwidgets)
  
  for (d in list(img_dir, pdf_dir, html_dir)) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  }
  
  roads <- st_intersection(roads, cbsa_boundary)
  water <- st_intersection(water %>% filter(AWATER > 1e5), cbsa_boundary)
  geodata_clean <- st_difference(geodata, st_union(water))
  
  interstates <- roads %>% filter(RTTYP == "I")
  us_highways <- roads %>% filter(RTTYP == "U")
  
  bbox_zoom <- st_bbox(cbsa_boundary)
  expand <- 3000
  bbox_zoom[c("xmin", "ymin")] <- bbox_zoom[c("xmin", "ymin")] - expand
  bbox_zoom[c("xmax", "ymax")] <- bbox_zoom[c("xmax", "ymax")] + expand
  
  numeric_vars <- geodata %>%
    st_drop_geometry() %>%
    select(where(is.numeric)) %>%
    names()
  
  for (var in numeric_vars) {
    values <- geodata[[var]]
    if (all(is.na(values))) next
    
    breaks <- tryCatch(classIntervals(values, n = 5, style = "jenks")$brks, error = function(e) NULL)
    if (is.null(breaks) || length(unique(breaks)) < 2) next
    breaks <- sort(pretty(unique(breaks), n = 5))
    
    labels <- paste0(breaks[-length(breaks)], "–", breaks[-1])
    labels <- factor(labels, levels = labels, ordered = TRUE)
    geodata_clean$fill_class <- cut(values, breaks = breaks, labels = labels, include.lowest = TRUE, ordered_result = TRUE)
    
    legend_title <- str_replace_all(var, "_", " ") %>% str_to_title()
    file_base <- str_replace_all(var, "_", "-")
    pdf_path <- file.path(pdf_dir, paste0(file_base, ".pdf"))
    png_path <- file.path(img_dir, paste0(file_base, ".png"))
    html_path <- file.path(html_dir, paste0(file_base, ".html"))
    
    if (!overwrite && any(file.exists(c(pdf_path, png_path, html_path)))) next
    
    palette_values <- MetBrewer::met.brewer("Signac", length(labels))
    
    p <- ggplot() +
      geom_sf(data = geodata_clean, aes(fill = fill_class), color = "black", size= 0.015) +
      scale_fill_manual(values = palette_values, na.value = "grey90", drop = FALSE) +
      geom_sf(data = water, fill = "lightblue", color = "lightblue", alpha = 0.5, linewidth = 0.1) +
      geom_sf(data = interstates, color = "darkgrey", linewidth = 0.6) +
      geom_sf(data = us_highways, color = "darkgrey", linewidth = 0.6) +
      geom_sf(data = site_points, shape = 21, fill = "lightgreen", color = "black", size = 1.4, stroke = 0.25) +
      coord_sf(xlim = c(bbox_zoom["xmin"], bbox_zoom["xmax"]),
               ylim = c(bbox_zoom["ymin"], bbox_zoom["ymax"])) +
      annotation_scale(location = "bl", width_hint = 0.2, line_col = "black", text_col = "black") +
      annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering()) +
      labs(title = legend_title, fill = legend_title, caption = "○ Green: Home\n— Yellow: Primary Roads") +
      theme_void(base_size = 14) +
      theme(
        legend.position = "bottom",
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9),
        plot.margin = margin(10, 10, 10, 10),
        plot.title = element_text(size = 16, face = "bold"),
        plot.caption = element_text(hjust = 0.5, size = 9, face = "italic")
      )
    
    ggsave(pdf_path, p, width = width, height = height)
    ggsave(png_path, p, width = width, height = height, dpi = dpi)
    
    if (run_leaflet) {
      fill_pal <- colorBin(
        palette = palette_values,
        domain = values,
        bins = breaks,
        na.color = "gray80",
        reverse = FALSE
      )
      
      labels <- sprintf("<strong>Tract:</strong> %s<br/><strong>%s:</strong> %s",
                        geodata$NAME,
                        legend_title,
                        prettyNum(values, big.mark = ",")) %>%
        lapply(htmltools::HTML)
      
      m <- leaflet(geodata) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(
          fillColor = ~fill_pal(values),
          fillOpacity = 0.8,
          weight = 0.4,
          color = "black",
          label = labels,
          highlightOptions = highlightOptions(weight = 2, color = "black", bringToFront = TRUE)
        ) %>%
        addLegend(
          pal = fill_pal,
          values = values,
          title = legend_title,
          position = "bottomright"
        ) %>%
        addControl("<strong>Legend:</strong><br/>
                    <span style='color:yellow;'>&#9632;</span> Primary Roads<br/>
                    <span style='color:green;'>&#9679;</span> Home",
                   position = "bottomleft") %>%
        addCircleMarkers(data = site_points, color = "green", radius = 3, label = "Home", stroke = TRUE)
      
      saveWidget(m, file = html_path, selfcontained = TRUE)
    }
    
    message("✅ Exported: ", var)
  }
}



#==============================================================================
#==============================================================================

generate_faceted_hotspot_maps <- function(hotspot_data,
                                         site_points,
                                         water,
                                         roads,
                                         cbsa_boundary,
                                         file_basename = "hotspot_facets_with_context",
                                         img_dir = here::here("clients", "arshad", "outputs", "img"),
                                         pdf_dir = here::here("clients", "arshad", "outputs", "pdf", "hotspots"),
                                         width = 14, height = 8, dpi = 300, overwrite = TRUE) {
  library(ggplot2)
  library(sf)
  library(ggforce)
  library(dplyr)
  library(here)
  library(ggspatial)

  if (!dir.exists(img_dir)) dir.create(img_dir, recursive = TRUE)
  if (!dir.exists(pdf_dir)) dir.create(pdf_dir, recursive = TRUE)

  # Use cbsa_boundary as the clipping boundary
  roads <- st_intersection(roads, cbsa_boundary)
  water <- st_intersection(water %>% filter(AWATER > 1e5), cbsa_boundary)

  # Prepare roads for consistent symbology
  interstates <- roads %>% filter(RTTYP == "I")
  us_highways <- roads %>% filter(RTTYP == "U")

  # Create individual page for each variable
  for (var in unique(hotspot_data$variable)) {
    filtered_data <- hotspot_data %>% filter(variable == var)
    pdf_path <- file.path(pdf_dir, paste0(file_basename, "_", var, ".pdf"))
    png_path <- file.path(img_dir, paste0(file_basename, "_", var, ".png"))

    if (!overwrite && (file.exists(pdf_path) || file.exists(png_path))) {
      message("⏭️ Skipping existing: ", var)
      next
    }

    if (overwrite) {
      if (file.exists(pdf_path)) file.remove(pdf_path)
      if (file.exists(png_path)) file.remove(png_path)
    }

    p <- ggplot(filtered_data) +
      geom_sf(aes(fill = hotspot_status), color = "grey90", size = 0.1) +
      scale_fill_manual(
        values = c("Hotspot" = "red", "Not Hotspot" = "white", "Unavailable" = "lightgrey"),
        na.value = "grey80", drop = FALSE
      ) +
      geom_sf(data = water, fill = "lightblue", color = NA, alpha = 0.5) +
      geom_sf(data = interstates, color = "yellow", linewidth = 0.6) +
      geom_sf(data = us_highways, color = "yellow", linewidth = 0.6) +
      geom_sf(data = site_points, shape = 21, fill = "lightgreen", color = "black", size = 1.2, stroke = 0.25) +
      annotation_scale(location = "bl", width_hint = 0.2, line_col = "black", text_col = "black") +
      annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering()) +
      labs(title = var, fill = "Hotspot Status", caption = "○ Green: Home\n— Yellow: Primary Roads") +
      theme_void(base_size = 14) +
      theme(
        strip.text = element_text(size = 13, face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        plot.caption = element_text(hjust = 0.5, size = 9, face = "italic"),
        plot.margin = margin(10, 10, 10, 10)
      )

    ggsave(png_path, plot = p, width = width, height = height, dpi = dpi)
    ggsave(pdf_path, plot = p, width = width, height = height)
    message("✅ Saved: ", var)
  }

  message("🎉 All hotspot maps saved.")
}



#==============================================================================
#==============================================================================

combine_choropleth_and_hotspot_pdfs <- function(
    choropleth_dir = here::here("clients", "arshad", "outputs", "pdf", "choropleth"),
    hotspot_dir = here::here("clients", "arshad", "outputs", "pdf", "hotspots"),
    output_path = here::here("clients", "arshad", "outputs", "pdf", "combined_choropleth_hotspots.pdf")) {
  library(pdftools)
  
  choropleth_pdfs <- list.files(choropleth_dir, pattern = "\\.pdf$", full.names = TRUE)
  hotspot_pdfs <- list.files(hotspot_dir, pattern = "\\.pdf$", full.names = TRUE)
  
  all_pdfs <- c(choropleth_pdfs, hotspot_pdfs)
  
  pdf_combine(input = all_pdfs, output = output_path)
  message("✅ Combined PDF saved to: ", output_path)
}


#===============================================================================
#===============================================================================

' Generate Composite Tier Map for Client Arshad
#'
#' Creates a professional PDF choropleth of a tiered composite score, clipped to
#' a study boundary, with waterbodies, major roads, and a home‑site marker. A
#' confidentiality footer is added. Uses tidy‑evaluation throughout and aligns
#' factor levels to the **names of `tier_palette`** so legend and fill colours
#' always match.  POINT geometries in the roads layer are removed to avoid
#' "little circles" artefacts.
#'
#' @param composite_sf  sf object containing a *tier* column.
#' @param tier_col      Column name holding tier values (default "tier").
#' @param water         sf multipolygon of waterbodies.
#' @param roads         sf LINESTRING roads; RTTYP codes "I" and "U" are kept.
#' @param site_points   sf POINT for the home/site marker.
#' @param cbsa_boundary sf polygon of the study area.
#' @param tier_palette  **Named** character vector of colours (names = tier levels).
#' @param pdf_dir       Output folder for PDF.
#' @param filename      PDF filename.
#' @param width,height  Plot size (inches).
#' @param dpi           Resolution for `ggsave()`.
#' @param overwrite     Overwrite existing file? (default TRUE).
#'
#' @return Invisibly, the path to the saved PDF.
#' @export

generate_composite_tier_map <- function(composite_sf,
                                        tier_col = "tier",
                                        water,
                                        roads,
                                        site_points,
                                        cbsa_boundary,
                                        tier_palette = MetBrewer::met.brewer("Signac", 5),
                                        pdf_dir = here::here("clients", "arshad", "outputs", "pdf", "choropleth"),
                                        filename = "composite_tier_map.pdf",
                                        width = 12,
                                        height = 8,
                                        dpi = 300,
                                        overwrite = TRUE) {
  # ---- Libraries ----
  library(sf)
  library(ggplot2)
  library(dplyr)
  library(ggspatial)
  library(rlang)            # tidy‑eval helpers
  
  if (!dir.exists(pdf_dir)) dir.create(pdf_dir, recursive = TRUE)
  
  # ---- Clip layers to study area ----
  water_clip <- st_intersection(water, composite_sf)
  roads_clip <- st_intersection(roads, composite_sf)
  comp_clip  <- composite_sf
  site_clip  <- st_intersection(site_points, composite_sf)
  
  # ---- Keep only linework for major roads ----
  major_roads <- roads_clip %>%
    filter(RTTYP %in% c("I", "U")) %>%           # Interstates & U.S. highways
    st_collection_extract("LINESTRING") %>%       # drop POINT parts
    filter(st_length(.) > units::set_units(0, "m"))
  
  # ---- Bounding box padding ----
  bb  <- st_bbox(cbsa_boundary)
  pad <- 1000                        # adjust if needed (same units as CRS)
  bb[c("xmin", "ymin")] <- bb[c("xmin", "ymin")] - pad
  bb[c("xmax", "ymax")] <- bb[c("xmax", "ymax")] + pad
  
  # ---- Align factor levels to palette names ----
  lvl <- if (!is.null(names(tier_palette)) && length(names(tier_palette)) > 0) {
    names(tier_palette)
  } else {
    sort(unique(comp_clip[[tier_col]]))
  }
  comp_clip[[tier_col]] <- factor(comp_clip[[tier_col]], levels = lvl, ordered = TRUE)
  
  # ---- Output path ----
  pdf_path <- file.path(pdf_dir, filename)
  if (file.exists(pdf_path) && !overwrite) {
    message("⚠️  ", filename, " exists & overwrite = FALSE – skipping.")
    return(invisible(pdf_path))
  }
  
  # ---- Construct map ----
  library(scales)
  library(grid)          # for unit()
  
  p <- ggplot() +
    #— main polygons —#
    suppressWarnings(                       # silences “spatially constant” msg
      geom_sf(
        data   = comp_clip,
        aes(fill = .data[[tier_col]]),
        colour = alpha("black", 0.3),
        size   = 0.05
      )
    ) +
    scale_fill_manual(values = tier_palette,
                      name   = "Composite Tier",
                      drop   = FALSE) +
    #— overlays —#
    geom_sf(data = water_clip,  fill = "lightblue",
            colour = "lightblue", linewidth = 0.15) +
    geom_sf(data = major_roads, colour = "black",  linewidth = 0.3) +
    geom_sf(data = site_clip,   shape  = 21, fill = "lightgreen",
            colour = "black", size = 2, stroke = 0.3) +
    
    #— view window —#
    coord_sf(xlim = c(bb["xmin"], bb["xmax"]),
             ylim = c(bb["ymin"], bb["ymax"]),
             expand = FALSE) +
    
    #— scalebar & north arrow with nudge —#
    annotation_scale(location = "bl", width_hint = 0.2,
                     pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm")) +
    annotation_north_arrow(location = "tr", which_north = "true",
                           style = north_arrow_fancy_orienteering(),
                           pad_x = unit(0.4, "cm"), pad_y = unit(0.6, "cm")) +
    
    #— titles —#
    labs(
      title   = "Composite Suitability Tier Map",
      caption = "*CONFIDENTIAL* This map is property of Arshad Bacchus, Saifally Khan, and Gabriel Hernandez Dominguez."
    ) +
    
    #— theme tweaks —#
    theme_void(base_size = 14) +
    theme(
      # bigger title plus space above
      plot.title = element_text(size = 22, face = "bold", hjust = 0.5,
                                margin = margin(t = 15, b = 10)),
      
      # legend: keep bottom anchor, but place inside with new arg
      legend.position         = "bottom",
      legend.position.inside  = c(0.05, 0.05),       # <- NEW (x,y in npc)
      legend.justification.inside = "center",
      legend.direction        = "horizontal",
      legend.title            = element_text(size = 11),
      legend.text             = element_text(size = 9),
      
      plot.caption = element_text(size = 8, face = "italic", hjust = 0.5),
      plot.margin  = margin(5, 5, 5, 5)
    )
  
  ggsave(pdf_path, p, width = width, height = height, dpi = dpi)
  message("✅ Map saved to: ", pdf_path)
  invisible(pdf_path)
}

