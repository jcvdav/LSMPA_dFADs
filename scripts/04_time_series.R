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
            theme(axis.title = element_blank(),
                  axis.text = element_blank(),
                  text = element_text(family = "Helvetica",
                                      color = 'black'),
                  legend.position = "top",
                  legend.background = element_blank(),
                  legend.key.width = unit(30, "pt"),
                  legend.key.height = unit(10, "pt"),
                  legend.box.spacing = element_blank(),
                  panel.spacing = element_blank(),,
                  legend.title = element_text(hjust = 0.5)))


# Load data --------------------------------------------------------------------
rfmo <- readRDS(file = here("processed_data/annual_rfmo_effort_1deg.rds"))

## PROCESSING ##################################################################

# annual RFMO ------------------------------------------------------------------
annual_rfmo <- rfmo |> 
  group_by(year, src) |> 
  summarize(sets_tot = sum(sets_tot, na.rm = T),
            sets_dfad = sum(sets_dfad, na.rm = T),
            .groups = "drop")

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
ggplot(data = annual_rfmo |> 
         mutate(dfad_prop_tot = sets_dfad / sets_tot) |> 
         drop_na(dfad_prop_tot),
       mapping = aes(x = year, y = dfad_prop_tot, color = src)) +
  geom_line() +
  geom_point() +
  scale_color_viridis_d(option = "viridis",
                        aesthetics = c("color", "fill"))

ggplot(data = annual_rfmo,
       mapping = aes(x = year, y = sets_dfad, color = src)) +
  geom_line() +
  geom_point() +
  scale_color_viridis_d(option = "viridis",
                        aesthetics = c("color", "fill"))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------