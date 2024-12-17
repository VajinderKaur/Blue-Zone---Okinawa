library(ggplot2)
library(dplyr)

mortality <- read.csv("./data/Mortality_Okinawa.csv")
mortality <- na.omit(mortality)
mortality$Year <- as.numeric(mortality$Year)
females <- mortality[mortality$Sex == "Females", ]
males <- mortality[mortality$Sex == "Males", ]

mortality_age_trend <- ggplot() +
  geom_line(data = females, aes(x = as.numeric(Year), y = Age, color = "Females"), size = 1.2) +
  geom_line(data = males, aes(x = as.numeric(Year), y = Age, color = "Males"), size = 1.2) +
  scale_y_continuous(limits = c(60, 100)) +
  scale_x_continuous(breaks = unique(mortality$Year)) +
  labs(
    title = "Mortality Age Trends by Year",
    x = "Year",
    y = "Age"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(
    values = c("Females" = "pink", "Males" = "blue"),
    name = "Gender"
  )


mor_5 <- read.csv("./data/Okinawa-mortality-under-five-line.csv")
mor_5 <- na.omit(mor_5)
mor_5$Year <- as.numeric(mor_5$Year)

under1 <- mor_5[mor_5$Age == "Under-1", ]
under5 <- mor_5[mor_5$Age == "Under-5", ]
under_five_trend <- ggplot() +
  geom_line(data = under1, aes(x = Year, y = Deaths.per.1.000.live.births, color = "Under-1"), size = 1.2) +
  geom_line(data = under5, aes(x = Year, y = Deaths.per.1.000.live.births, color = "Under-5"), size = 1.2) +
  labs(title = "Mortality Rate for Under-1 and Under-5 Children",
       x = "Year",
       y = "Deaths per 1,000 Live Births") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("Under-1" = "blue", "Under-5" = "red"), name = "Age Group")

world <- read.csv("./data/World.csv")
US <- read.csv("./data/United States.csv")

# Transforming the data
US <- US %>%
  mutate(Year = as.numeric(substr(Date, 7, 10)),  # Extracting Year from Date
         Age = GDP..Billions.of.US...) %>%  # Renaming GDP..Billions.of.US... to Age
  select(Year, Age)  # Selecting only Year and Age columns

world <- world %>%
  mutate(Year = as.numeric(substr(Date, 7, 10)),  # Extracting Year from Date
         Age = GDP..Billions.of.US...) %>%  # Renaming GDP..Billions.of.US... to Age
  select(Year, Age)  # Selecting only Year and Age columns

# Creating a simple plot
plot(world$Year, world$Age, type = "l", col = "blue", lwd = 2, 
     xlab = "Year", ylab = "Age", 
     main = "World and US Mortality Age Trends")
lines(US$Year, US$Age, col = "red", lwd = 2)

legend("bottomright", legend = c("World", "US"), col = c("blue", "red"), lwd = 2, cex = 0.8)

