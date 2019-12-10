huc12 = st_read("WBD_HU_08_10_12/WBD_HU_12_IA.shp", stringsAsFactors = FALSE)


usethis::use_data("huc12")


ggplot() + geom_polygon(data = huc12, aes(x = long, y = lat, group = group), colour = "black", fill = NA)


ia <- maps::map("state", region="iowa", plot = FALSE, fill = TRUE)  %>%
  st_as_sf() %>%
  st_transform(crs = st_crs(huc12))

# Cuts the HUC12s to be within IA
IA_HUC12_IDs = st_intersection(huc12, ia) %>%
  st_drop_geometry() %>%
  pull(HUC_12)

# Want those HUC12s that touch IA
st_touches(huc12,ia)


huc12_ia = huc12 %>%
  select(HUC_12 %in% IA_HUC12_IDs)


ggplot(huc12_ia) +
  geom_sf(fill= NA) +
  # geom_sf(data = ia, fill = NA, color = "black", size=1.5) +
  theme_void()

  geom_polygon(data = ia,
               aes(x = long, y = lat, group = group),
               color = "black", fill = NA) +
  ylim(40,44)


ggplot(ia, aes(x=long, y=lat)) +
  geom_polygon(color="black", fill = NA)

ggplot(ia) +
  geom_sf(color = "black", fill = NA)
