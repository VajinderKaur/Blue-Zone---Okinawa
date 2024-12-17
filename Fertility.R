library(ggplot2)
fertility <- read.csv("./data/Fertility.csv")

fertility <- fertility |> rename(`Total Birth` = A4101_Number.of.live.births.person., 
                                 `Male Birth` = A410101_Number.of.live.births..Male..person., 
                                 `Female Birth` = A410102_Number.of.live.births..Female..person., 
                                 `Fertility Rate` = A4103_Total.fertility.rate...)

selected_regions <- c("All Japan", "Okinawa-ken", "Tokyo-to", "Osaka-fu", "Kyoto-fu", 
                      "Hiroshima-ken", "Fukuoka-ken", "Aichi-ken")
filtered_fertility <- fertility[fertility$AREA %in% selected_regions, ]

# Create the bar chart
ggplot(filtered_fertility, aes(x = AREA, y = `Fertility Rate`, fill = AREA)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(title = "Fertility Rates in Selected Japanese Regions",
       x = "Region",
       y = "Fertility Rate") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  geom_text(aes(label = `Fertility Rate`), vjust = -0.3, size = 4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))