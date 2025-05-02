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
  tidyverse
  )

theme_set(theme_linedraw(base_size = 9) +
            theme(text = element_text(family = "Helvetica",
                                      color = 'black'),
                  legend.position = "inside",
                  legend.background = element_blank(),
                  legend.title = element_text(hjust = 0.5),
                  legend.position.inside = c(0, 1),
                  legend.justification.inside = c(0, 1)))


# Load data --------------------------------------------------------------------
rfmo <- readRDS(file = here("processed_data/annual_rfmo_effort_1deg.rds"))

## PROCESSING ##################################################################

# annual RFMO ------------------------------------------------------------------
annual_rfmo <- rfmo |> 
  group_by(year, src) |> 
  summarize(sets_tot = sum(sets_tot),
            sets_dfad = sum(sets_dfad, na.rm = T),
            .groups = "drop") |> 
  mutate(src = toupper(src))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
pal <- c("IATTC" = "lightblue",
         "ICCAT" = "steelblue",
         "IOTC" = "cadetblue",
         "WCPFC" = "darkblue")

rel <- ggplot(data = annual_rfmo |> 
         mutate(dfad_prop_tot = sets_dfad / sets_tot) |> 
         drop_na(dfad_prop_tot),
       mapping = aes(x = year, y = dfad_prop_tot, color = src)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = pal) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Year",
       y = "dFAD sets as \nproportion of total sets",
       color = "RFMO") +
  theme(legend.position = "None")

abs <- ggplot(data = annual_rfmo,
       mapping = aes(x = year, y = sets_dfad, color = src)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = pal) +
  labs(x = "Year",
       y = "dFAD sets (#)",
       color = "RFMO")

p <- cowplot::plot_grid(rel, abs, labels = "AUTO")


## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
ggsave(plot = p,
       filename = here("results", "figs", "time_series.pdf"),
       units = "cm",
       width = 18,
       height = 6.5)
