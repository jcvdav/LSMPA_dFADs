
con <- readRDS(file = here("raw_data/rfmo_data/ICCAT/ICCAT_database.rds"))

# Extract the data
data <- con$t2ce %>%
  clean_names()

annual_iccat <- data %>%
  filter(gear_code %in% c(
    "PS",     #Purse seine
    "PSD",     #Purse seine: Double-boats
    "PSFB",     #Purse seine: Catching large fish
    # "PSFS",     #Purse seine: Catching small fish
    "PSG",     #Purse seine: Large scale (over 200 MT capacity)
    "PSLB",     #Purse seine: Using live bait
    "PSM",     #Purse seine: Medium scale (between 50 and 200 MT capacity)
    "PSS"     #Purse seine: Small scale (less than 50 MT capacity)
  ),
  square_type_code == "1x1") %>%
  filter(d_set_type_id == ".w", # Keep data reported as weight
         catch_unit == "kg",    #
         eff1 > 0,
         quad_id > 0,
         eff1type %in% c("NO.HOOKS", "NO.SETS") | eff2type %in% c("NO.HOOKS", "NO.SETS"),
         # time_period_id <= 12, #This would keep only data that are reported monthly, but we're using it all to keep up to annual
         !(eff1 == 0 & eff1type == "-none-"),
         square_type_code %in% c("1x1", "5x5")) %>%
  mutate(effort_measure = case_when(gear_grp_code == "PS" & eff1type == "NO.SETS" ~ eff1type,
                                    gear_grp_code == "PS" & eff2type == "NO.SETS" ~ eff2type,
                                    T ~ NA_character_),
         effort = case_when(gear_grp_code == "PS" & eff1type == "NO.SETS" ~ eff1,
                            gear_grp_code == "PS" & eff2type == "NO.SETS" ~ eff2,
                            T ~ NA)) %>% 
  drop_na(effort_measure) %>%
  drop_na(effort) %>%
  filter(effort > 0) %>%
  select(-c(eff1, eff1type, eff2, eff2type)) %>% 
  mutate(
    # Data are reported in Kg so we convert to MT
    bft_mt = bft / 1e3,
    alb_mt = alb / 1e3,
    yft_mt = yft / 1e3,
    bet_mt = bet / 1e3,
    skj_mt = skj / 1e3) %>%
  #This PDF has information on how to handle ICCAT's geographical data
  # https://www.iccat.int/Data/ICCAT_maps.pdf
  # It also uses "of corner of the Rectangle closest to the equator"
  mutate(lat_mult = ifelse(quad_id %in% c(1, 4), 1, -1),
         lon_mult = ifelse(quad_id %in% c(1, 2), 1, -1),
         # Assign sign to proper hemisphere
         lat = (lat * lat_mult),
         lon = (lon * lon_mult),
         # Move the coordinate to the center of the grid
         lat = case_when(quad_id == 1 ~ lat + 0.5,
                         quad_id == 2 ~ lat - 0.5,
                         quad_id == 3 ~ lat - 0.5,
                         quad_id == 4 ~ lat + 0.5),
         lon = case_when(quad_id == 1 ~ lon + 0.5,
                         quad_id == 2 ~ lon + 0.5,
                         quad_id == 3 ~ lon - 0.5,
                         quad_id == 4 ~ lon - 0.5)) %>%
  mutate(catch_tot = skj_mt + yft_mt + bet_mt + bft_mt + alb_mt) %>% 
  select(year = year_c,
         lat, lon,
         set_type = school_type_code,
         num_sets = effort,
         catch_tot) %>%
  group_by(year, lat, lon) %>% 
  summarize(sets_tot = sum(num_sets, na.rm = T),
            sets_dfad = sum(num_sets[set_type == "FAD"], na.rm = T),
            catch_tot = sum(catch_tot, na.rm = T),
            catch_dfad = sum(num_sets[set_type == "FAD"], na.rm = T),
            .groups = "drop") %>% 
  filter(sets_tot > 0,
         catch_tot > 0) %>% 
  mutate(cpue_tot = catch_tot / sets_tot,
         cpue_dfad = catch_dfad / sets_dfad,
         dfad_prop_tot = sets_dfad / sets_tot,
         src = "iccat")


saveRDS(annual_iccat, file = here("processed_data", "annual_iccat_effort_1deg.rds"))
