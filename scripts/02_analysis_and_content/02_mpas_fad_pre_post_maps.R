################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  rnaturalearth,
  tidyverse,
  cowplot,
  sf
)

# Load data --------------------------------------------------------------------
coast <- ne_countries()

rfmo_data <- readRDS(file = here("processed_data", "annual_rfmo_effort_1deg.rds"))

mpas <- st_read(dsn = here("processed_data/selected_LSMPAs_viz.gpkg"))

theme_set(theme_linedraw(base_size = 8) +
            theme(axis.title = element_blank(),
                  text = element_text(family = "Helvetica",
                                      color = 'black'),
                  legend.position = "top",
                  legend.background = element_blank(),
                  legend.key.width = unit(15, "pt"),
                  legend.key.height = unit(10, "pt"),
                  legend.box.spacing = element_blank(),
                  legend.title = element_text(hjust = 0.5),
                  legend.title.position = "top",
                  strip.background = element_rect(fill = "transparent"),
                  strip.text = element_text(color = "black")))

# Needed to extrac tthe legned for later ---------------------------------------
# The function below comes form teunbrand on GitHub: https://github.com/wilkelab/cowplot/issues/202#issuecomment-1981802127
get_legend <- function(plot, legend = NULL) {
  
  gt <- ggplotGrob(plot)
  
  pattern <- "guide-box"
  if (!is.null(legend)) {
    pattern <- paste0(pattern, "-", legend)
  }
  
  indices <- grep(pattern, gt$layout$name)
  
  not_empty <- !vapply(
    gt$grobs[indices], 
    inherits, what = "zeroGrob", 
    FUN.VALUE = logical(1)
  )
  indices <- indices[not_empty]
  
  if (length(indices) > 0) {
    return(gt$grobs[[indices[1]]])
  }
  return(NULL)
}

## PROCESSING ##################################################################

# Work with MPAS ---------------------------------------------------------------
# Subset MPAS of interest
select_mpas <- mpas |> 
  filter(wdpaid %in% c(
    "11753",         # Galapagos
    "309888",        # PIPA
    "555629385",     # Revillagigedo
    "555651558",     # Asención
    "555512151"      # Chagos - Included for completeness:  - Can't do Chagos because 1) there is no data before it was implemented an 2) there is no "other sets" data to calculate dFAD set as % of total
  )) |> 
  nngeo::st_remove_holes()

# Build buffer of 200 NM
buf_200 <- select_mpas |> 
  st_buffer(dist = units::as_units(200, "nautical_mile")) |> 
  mutate(dist_200 = 1) |>
  select(dist_200, wdpaid, name, year_enforced)

buf_100 <- select_mpas |> 
  st_buffer(dist = units::as_units(100, "nautical_mile")) |> 
  mutate(dist_100 = 1) |>
  select(dist_100, wdpaid, name, year_enforced)

# Work with catch and effort data ----------------------------------------------
annual_pre_post <- rfmo_data |> 
  st_as_sf(coords = c("lon", "lat"),
           crs = "EPSG:4326") |> 
  st_join(buf_200) |> 
  st_filter(buf_200) |>
  st_filter(st_union(select_mpas), .predicate = st_disjoint) %>%
  bind_cols(st_coordinates(.)) |> 
  st_drop_geometry() |> 
  mutate(event = year - year_enforced,
         post = ifelse(event >= 0, "After", "Before"),
         post = fct_relevel(post, "Before", "After")) |> 
  rename(lon = X, lat = Y)

# We keep data from 5 years before and 5 years after. event = 0 is the year enforced
pre_post <- annual_pre_post |> 
  filter(between(event, -5, 4)) |> 
  group_by(src, wdpaid, name, year_enforced, post, lon, lat) |> 
  summarize_all(sum, na.rm = T) |> 
  ungroup() |> 
  mutate(fad_pct_of_total = sets_dfad / sets_tot)


# Export -------
saveRDS(annual_pre_post, file = here("processed_data/annual_pre_post_activity_by_select_mpa.rds"))
saveRDS(pre_post, file = here("processed_data/pre_post_activity_by_select_mpa.rds"))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
before_after_plot <- function(data, hack = F) {
  # Take all of the MPAs and buffers and keep only the one we are plotting
  this_mpa <- filter(select_mpas, wdpaid == data$wdpaid[1])
  this_buffer_100 <- filter(buf_100, wdpaid == data$wdpaid[1])
  this_buffer_200 <- filter(buf_200, wdpaid == data$wdpaid[1])
  
  # # Calculate % to overlay as text on the Beofre and After plot
  means <- data |> 
    group_by(wdpaid, post) |> 
    summarize(sets_tot = sum(sets_tot), sets_dfad = sum(sets_dfad),
              .groups = "drop") |> 
    mutate(m = paste0(round(sets_dfad / sets_tot, 3) * 100, "%")) |>
    left_join(this_mpa, by = join_by(wdpaid)) |> 
    st_as_sf(sf_column_name = "geometry")
  
  pre_post <- ggplot() +
    geom_tile(data = data,
              aes(x = lon, y = lat, fill = fad_pct_of_total)) +
    geom_sf(data = this_mpa, fill = "gray50", color = "black") +
    geom_sf(data = this_buffer_100, fill = "transparent", color = "black") +
    geom_sf(data = this_buffer_200, fill = "transparent", color = "black") +
    geom_sf_text(data = means, aes(label = m),
                 color = "white",
                 size = 2) +
    facet_wrap(~post) +
    scale_fill_viridis_c(option = "cividis",
                         name = "dFAD sets (% of total)",
                         limits = c(0, 1),
                         labels = scales::percent,
                         guide = guide_colorbar(frame.colour = "black",
                                                ticks.colour = "black")) +
    scale_x_continuous(expand = c(0, 0.2),
                       labels = ~ ifelse(seq_along(.x) %% 2 == 1, .x, "")) +
    scale_y_continuous(expand = c(0, 0.1),
                       labels = ~ ifelse(seq_along(.x) %% 2 == 1, .x, "")) +
    theme(legend.position = "none")
  
  # This is just a hack to return the legend instead of a plot, used when building the panel
  if(isTRUE(hack)) {
    return(get_legend(
      pre_post +
        theme(legend.position = "bottom")
    ))
  }
  
  return(pre_post)
  
}

change_plot <- function(data, hack = F) {
  # Take all of the MPAs and buffers and keep only the one we are plotting
  this_mpa <- filter(select_mpas, wdpaid == data$wdpaid[1])
  this_buffer_100 <- filter(buf_100, wdpaid == data$wdpaid[1])
  this_buffer_200 <- filter(buf_200, wdpaid == data$wdpaid[1])
  
  # Change in means to overlay on the Difference plot
  dif_means <- data |> 
    group_by(wdpaid, post) |> 
    summarize(sets_tot = sum(sets_tot), sets_dfad = sum(sets_dfad),
              .groups = "drop") |> 
    mutate(m = sets_dfad / sets_tot) |> 
    select(wdpaid, post, m) |> 
    pivot_wider(names_from = post,
                values_from = m) |> 
    mutate(dif = paste0(round((After - Before), 3) * 100, "%")) |> 
    left_join(this_mpa, by = join_by(wdpaid)) |> 
    st_as_sf(sf_column_name = "geometry")
  
  
  change <- data |> 
    select(post, lon, lat, fad_pct_of_total) |> 
    pivot_wider(names_from = post,
                values_from = fad_pct_of_total,
                values_fill = 0) |> 
    mutate(delta = After - Before,
           strip = "After - Before") |> 
    ggplot() +
    geom_tile(aes(x = lon, y = lat, fill = delta)) +
    geom_sf(data = this_mpa, fill = "gray50", color = "black") +
    geom_sf(data = this_buffer_100, fill = "transparent", color = "black") +
    geom_sf(data = this_buffer_200, fill = "transparent", color = "black") +
    geom_sf_text(data = dif_means, aes(label = dif),
                 color = "white",
                 size = 2) +
    facet_wrap(~strip) +
    scale_fill_gradient2(name = "Change",
                         low = scales::muted("blue"),
                         mid = "white",
                         high = scales::muted("red"),
                         midpoint = 0,
                         limits = c(-1, 1),
                         labels = scales::percent,
                         guide = guide_colorbar(frame.colour = "black",
                                                ticks.colour = "black")) +
    scale_x_continuous(expand = c(0, 0.2),
                       labels = ~ ifelse(seq_along(.x) %% 2 == 1, .x, "")) +
    scale_y_continuous(expand = c(0, 0.1),
                       labels = ~ ifelse(seq_along(.x) %% 2 == 1, .x, "")) +
    theme(legend.position = "none")
  
  if(isTRUE(hack)) {
    return(get_legend(
      change +
        theme(legend.position = "bottom")
    ))}
  
  return(change)
  
}

nested_data <- pre_post |> 
  filter(!wdpaid == "555512151") |> 
  mutate(name = fct_reorder(factor(name), year_enforced)) |> 
  group_by(name) |> 
  nest() |> 
  arrange(name)

pre_post_plots <- nested_data |> 
  mutate(p = map(data, .f = before_after_plot))

change_plots <- nested_data |> 
  mutate(p = map(data, .f = change_plot))

# Build legends ----------------------------
legend_pre_post <- nested_data |> 
  head(1) |> 
  unnest(data) |> 
  before_after_plot(hack = T)

legend_change <- nested_data |> 
  head(1) |> 
  unnest(data) |> 
  ungroup() |> 
  change_plot(hack = T)

# Combine all ------------------------------------------------------------------
final_pre_post_plot <- cowplot::plot_grid(legend_pre_post,
                                          pre_post_plots$p[[1]],
                                          pre_post_plots$p[[2]],
                                          pre_post_plots$p[[3]],
                                          pre_post_plots$p[[4]],
                                          rel_heights = c(0.45, 1, 1, 1, 1),
                                          ncol = 1)

final_change_plot <- cowplot::plot_grid(legend_change,
                                        change_plots$p[[1]],
                                        change_plots$p[[2]],
                                        change_plots$p[[3]],
                                        change_plots$p[[4]],
                                        rel_heights = c(0.45, 1, 1, 1, 1),
                                        ncol = 1)


ggsave(plot = final_pre_post_plot,
       filename = here("results", "figs", "mpas_fad_pre_post.pdf"),
       units = "cm",
       width = 6,
       height = 12)

ggsave(plot = final_change_plot,
       filename = here("results", "figs", "mpas_fad_dif.pdf"),
       units = "cm",
       width = 3,
       height = 12)
