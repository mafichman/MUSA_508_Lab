ggplot() +
  geom_sf(data = studyAreaTowns, fill = "black", color = "grey") +
  geom_sf(data = filter(buffersAndTowns, Legend == "Inside"), aes(fill = MUNI), 
          alpha = 0.3, color = "transparent") +
  geom_sf(data = filter(buffersAndTowns, Legend == "Outside"), aes(fill = MUNI),
          color = "transparent") +
  scale_fill_viridis_d() +
  geom_sf_label(data = studyAreaTowns %>% 
                  group_by(MUNI) %>% 
                  summarise(), 
                aes(label = str_remove(MUNI," TOWNSHIP")), 
                size = 3) +
  mapTheme() +
  theme(
    legend.position = "none"
  )