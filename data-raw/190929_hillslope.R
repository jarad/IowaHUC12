library("tidyverse")
library("gridExtra")

blank <- grid::grid.rect(gp=grid::gpar(col="white"))

set.seed(20191209)
hillslopes <- readr::read_csv("190929_hillslope.txt") %>%
  rename(HUC_12 = `HUC12 ID`,
         soil_loss = `soil loss`) %>%
  group_by(HUC_12) %>%
  mutate(train = HUC_12 %in% sample(HUC_12, min(n(), 2+ceiling(n()/6))))

train = hillslopes %>% filter( train)
test  = hillslopes %>% filter(!train)

hillslope_mean <- hillslopes %>%
  group_by(HUC_12) %>%
  summarize(
    precipitation = mean(precipitation),
    runoff        = mean(runoff),
    detachment    = mean(detachment),
    soil_loss     = mean(soil_loss)
  ) %>%
  ungroup()


hillslope_random <- train %>%
  group_by(HUC_12) %>%
  summarize(
    precipitation_random = mean(precipitation),
    runoff_random        = mean(runoff),
    detachment_random    = mean(detachment),
    soil_loss_random     = mean(soil_loss)
  ) %>%
  ungroup()

regression_prediction <- function(d) {
  m <- lm(log(soil_loss) ~ log(precipitation), data = d %>% filter(train))
  p <- predict(m, d)

  d %>% mutate(soil_loss = ifelse(train, soil_loss, p))
}

d2 <- st_read("idepv2_20190929/idepv2_20190929.shp", stringsAsFactors = FALSE) %>%
  left_join(hillslope_mean,   by="HUC_12") %>%
  left_join(hillslope_random, by = "HUC_12")


# p_dep <- ggplot(d2 %>% select(PREC_MM, precipitation, precipitation_random)) +
#   geom_sf(aes(fill = PREC_MM)) +
#   scale_fill_distiller(palette = "YlGnBu", direction=1) +
#   labs(title = "DEP") +
#   theme_void() +
#   theme(legend.position = "none")

p_mean <- ggplot(d2) +
  geom_sf(aes(fill = precipitation)) +
  scale_fill_distiller(palette = "YlGnBu", direction=1, na.value = "white") +
  labs(title = "Precipitation - Exact") +
  theme_void() +
  theme(legend.position = "none")

p_random <- ggplot(d2) +
  geom_sf(aes(fill = precipitation_random)) +
  scale_fill_distiller(palette = "YlGnBu", direction=1, na.value = "white") +
  labs(title = "Precipitation - Random subset") +
  theme_void() +
  theme(legend.position = "none")

s_mean <- ggplot(d2) +
  geom_sf(aes(fill = soil_loss)) +
  scale_fill_distiller(palette = "YlOrBr", direction=1, na.value = "white") +
  labs(title = "Soil loss - Exact") +
  theme_void() +
  theme(legend.position = "none")

s_random <- ggplot(d2) +
  geom_sf(aes(fill = soil_loss_random)) +
  scale_fill_distiller(palette = "YlOrBr", direction=1, na.value = "white") +
  labs(title = "Soil loss - Random subset") +
  theme_void() +
  theme(legend.position = "none")

d_mean <- ggplot(d2) +
  geom_sf(aes(fill = detachment)) +
  scale_fill_distiller(palette = "YlOrBr", direction=1, na.value = "white") +
  labs(title = "Detachment - Exact") +
  theme_void() +
  theme(legend.position = "none")

d_random <- ggplot(d2) +
  geom_sf(aes(fill = detachment_random)) +
  scale_fill_distiller(palette = "YlOrBr", direction=1, na.value = "white") +
  labs(title = "Detachment - Random subset") +
  theme_void() +
  theme(legend.position = "none")

grid.arrange(p_mean, p_random, s_mean, s_random, d_mean, d_random, ncol=2)

