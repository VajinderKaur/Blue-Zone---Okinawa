library(kableExtra)
library(dplyr)
library(ggplot2)
library(reshape2)

death_cause <- read.csv("./data/Okinawa-deaths-rank-chart.csv")
death_cause <- na.omit(death_cause)

death_cause <- death_cause |> rename(`2011 Rank` = X2011.rank, `2021 Rank` = X2021.rank, 'Cause Type' = Cause.type, `Change in deaths per 100k` = Change.in.deaths.per.100k..2011.2021 )

death_cause %>%
  kable(format = "html", 
        caption = "Ranking of Causes of Death in 2011 vs. 2021") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

death_disable <- read.csv("./data/Okinawa-daly-hcbar-chart.csv")
death_disable <- na.omit(death_disable)
death_disable <- death_disable |> rename(`2021 Rank` = X2021.rank, 'Cause Type' = Cause.type, Change = X..change.2011.2021)
death_disable$`2021 rank`<- as.numeric(death_disable$`2021 rank`)

ggplot(death_disable, aes(x = Change, y = Cause, fill = Change)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low = "red", high = "green") +
  theme_minimal() +
  labs(title = "Change in Deaths by Cause Type",
       x = "Percentage Change",
       y = NULL) +  # Remove Y axis label
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate labels vertically
  geom_text(aes(label = Change), position = position_stack(vjust = 0.5), size = 3) 

death_others <- read.csv("./data/Okinawa-benchmark-table-DALY.csv") 
death_others <- na.omit(death_others)
df_heatmap <- melt(death_others, id.vars = "Location")
ggplot(df_heatmap, aes(x = Location, y = variable, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  geom_text(aes(label = value), color = "black") +
  labs(title = "Death and Disability by Cause Type compared to other places", x = "Location", y = "Cause", fill = "Higher rank (more death and disability)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

other_factors <- read.csv("./data/Okinawa-daly-rank-chart.csv")
other_factors <- na.omit(other_factors)

other_factors <- other_factors |> rename(`2011 Rank` = X2011.rank, `2021 Rank` = X2021.rank, 'Cause Type' = Cause.type, `Change in DALY per 100K` = Change.in.DALYs.per.100k..2011.2021 )

other_factors %>%
  kable(format = "html", 
        caption = "Ranking of Causes of Death in 2011 vs. 2021") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))