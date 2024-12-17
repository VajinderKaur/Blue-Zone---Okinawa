library(dplyr)
library(scales)
library(ggplot2)
population <- read.csv("./data/Population.csv")

population <- population |> rename(`Total Population`  = A1101_Total.population..Both.sexes..person.,
                                   `Male Population` = A110101_Total.population..Male..person.,
                                   `Female Population` = A110102_Total.population..Female..person.  ,
                                   `Under 15` = A1304_Ratio.of.population..under15.years.old.... ,
                                   `15 - 64 Old` = A1305_Ratio.of.population..15.64.years.old....  ,
                                   `Over 65` = A1306_Ratio.of.population..65.years.old.and.over....,
                                   `Total Deaths` = A4200_Number.of.deaths.person.    ,
                                   `Male Deaths` = A420001_Number.of.deaths..Male..person. ,
                                   `Female Deaths` = A420002_Number.of.deaths..Female..person. )


population1 <- population %>%
  filter(YEAR >= 2005 & YEAR <= 2021) |> filter(AREA != "All Japan")

population1$`Total Population` <- as.numeric(gsub(",", "", population1$`Total Population`))
population1$`Male Population` <- as.numeric(gsub(",", "", population1$`Male Population`))
population1$`Female Population` <- as.numeric(gsub(",", "", population1$`Female Population`))

population1$`Total Deaths` <- as.numeric(gsub(",", "", population1$`Total Deaths`))
population1$`Male Deaths` <- as.numeric(gsub(",", "", population1$`Male Deaths`))
population1$`Female Deaths` <- as.numeric(gsub(",", "", population1$`Female Deaths`))
colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467be", "#8c564b", "#e377c2", "#7f7f7f")
line_plot <- ggplot(population1, aes(x = YEAR, y = `Total Deaths`, color = AREA)) +
  geom_line(size = 1.2) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = colors) + 
  labs(title = "Total Deaths Over Years for Selected Prefectures",
       x = "Year",
       y = "Total Deaths",
       color = "Prefecture") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

line_plot2 <- ggplot(population1, aes(x = YEAR, y = `Total Population`, color = AREA)) +
  geom_line(size = 1.2) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = colors) + 
  labs(title = "Total Population Over Years for Selected Prefectures",
       x = "Year",
       y = "Total Population",
       color = "Prefecture") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


population_age <- population %>% filter(YEAR==2022) |> 
  group_by(AREA) %>%
  summarise(
    Under_15 = sum(as.numeric(`Under 15`)),
    `15-64` = sum(as.numeric(`15 - 64 Old`)),
    Over_65 = sum(as.numeric(`Over 65`))
  )

# Melting data for ggplot
population_age_melt <- reshape2::melt(population_age, id.vars = "AREA")

# Plotting
ggplot(population_age_melt, aes(x = AREA, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Population Distribution Across Age Groups in Prefectures", 
       x = "Prefecture", y = "Population (%)") +
  theme_minimal() +
  scale_fill_manual(values = c("Under_15" = "skyblue", "15-64" = "orange", "Over_65" = "green"))


okinawa_data <- population1[population1$AREA == "Okinawa-ken", ]

# Create the line plot
line_plot_okinawa <- ggplot(okinawa_data, aes(x = YEAR)) +
  geom_line(aes(y = `Total Population`, color = "Total Population"), size = 1.2) +
  geom_line(aes(y = `Male Population`, color = "Male Population"), size = 1.2) +
  geom_line(aes(y = `Female Population`, color = "Female Population"), size = 1.2) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total Population Over Years for Okinawa",
       x = "Year",
       y = "Population",
       color = "Population Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

line_plot_okinawa
