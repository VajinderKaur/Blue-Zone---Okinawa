library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(kableExtra)
library(reshape2)

setwd("C:/Users/Lenovo/Project/Data Science in R/Blue-Zone---Okinawa")

mortality <- read.csv("./data/Mortality_Okinawa.csv")
mortality <- na.omit(mortality)
mortality$Year <- as.numeric(mortality$Year)

mor_5 <- read.csv("./data/Okinawa-mortality-under-five-line.csv")
mor_5 <- na.omit(mor_5)
mor_5$Year <- as.numeric(mor_5$Year)

world <- read.csv("./data/World.csv")
US <- read.csv("./data/United States.csv")

US <- US %>%
  mutate(Year = as.numeric(substr(Date, 7, 10)),
         Age = GDP..Billions.of.US...) %>%
  select(Year, Age)

world <- world %>%
  mutate(Year = as.numeric(substr(Date, 7, 10)),
         Age = GDP..Billions.of.US...) %>%
  select(Year, Age)

death_cause <- read.csv("./data/Okinawa-deaths-rank-chart.csv") %>% 
  na.omit() %>% 
  rename(`2011 Rank` = X2011.rank, `2021 Rank` = X2021.rank, 'Cause Type' = Cause.type, `Change in deaths per 100k` = Change.in.deaths.per.100k..2011.2021)

death_disable <- read.csv("./data/Okinawa-daly-hcbar-chart.csv") %>% 
  na.omit() %>%
  rename(`2021 Rank` = X2021.rank, 'Cause Type' = Cause.type, Change = X..change.2011.2021)

death_others <- read.csv("./data/Okinawa-benchmark-table-DALY.csv") %>% 
  na.omit()
df_heatmap <- melt(death_others, id.vars = "Location")

fertility <- read.csv("./data/Fertility.csv")

fertility <- fertility |> rename(`Total Birth` = A4101_Number.of.live.births.person., 
                                 `Male Birth` = A410101_Number.of.live.births..Male..person., 
                                 `Female Birth` = A410102_Number.of.live.births..Female..person., 
                                 `Fertility Rate` = A4103_Total.fertility.rate...)

selected_regions <- c("All Japan", "Okinawa-ken", "Tokyo-to", "Osaka-fu", "Kyoto-fu", 
                      "Hiroshima-ken", "Fukuoka-ken", "Aichi-ken")
filtered_fertility <- fertility[fertility$AREA %in% selected_regions, ]
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

population_age <- population1 %>%
  group_by(AREA) %>%
  summarise(
    Under_15 = sum(as.numeric(`Under 15`)),
    `15-64` = sum(as.numeric(`15 - 64 Old`)),
    Over_65 = sum(as.numeric(`Over 65`))
  )

# Melting data for ggplot
population_age_melt <- reshape2::melt(population_age, id.vars = "AREA")
okinawa_data <- population1[population1$AREA == "Okinawa-ken", ]

ui <- fluidPage(
  titlePanel("Okinawa: Blue Zone - Is it truly that wonderful?"),
  
  navbarPage(
    title = "Explore Okinawa",
    
    # About Panel
    tabPanel(
      title = "About",
      fluidRow(
        column(
          width = 12,
          div(style = "background-color: #007BFF; color: white; padding: 10px; font-size: 16px;",
              tags$b("Area: "), "1,206.98 km² | ",
              tags$b("Population: "), "1,457,162 | ",
              tags$b("Fertility Rate: "), "2.20"
          )
        )
      ),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            h3("Explore the Map"),
            p("This map highlights Okinawa, Japan. You can zoom in and out and explore the area."),
            br(),
            h3("Blue Zone"),
            p("A Blue Zone refers to areas where people tend to live longer, healthier lives. 
              These regions have been identified through research by Dan Buettner and the 
              National Geographic Society, focusing on areas with high longevity rates. 
              Key characteristics of Blue Zones include healthy lifestyles, strong social connections, 
              low-stress environments, and diets rich in whole foods. Examples of well-known Blue 
              Zones include Okinawa, Japan; Sardinia, Italy; and Ikaria, Greece")
            ),
          h3("About Okinawa:"),
          h4("Government and Administration"),
          p("Okinawa is a prefecture of Japan, located in the southernmost part of the country. The local government is led by a governor, elected by the residents of Okinawa. It has a distinct administrative system from the main island of Japan, with a focus on fostering local culture and heritage.
Okinawa is located in Japan and is known for its beautiful islands and the longevity of its residents. 
            Okinawa has been known for its status as Blue zones for long or it could be said vice versa as well. Recently, 
            it has been on news due to its astonishing high fertility rate ( which is still low compared to the rest of the world),
            as compared to the rest of aging Japan. Of the 50 locations with highest fertility rate in the Japan, 44 are in Okinawa."),
          h4("Economy"),
          p("Okinawa’s economy is diverse, with a mix of tourism, agriculture, fisheries, and a growing focus on research and technology.
Tourism is a significant economic driver, contributing to the local economy with attractions such as beaches, historical sites, and natural beauty.
OIST (Okinawa Institute of Science and Technology) has become a key institution promoting innovation and helping in economic growth through research and development. ")
        ),
        column(width = 8, leafletOutput("map", height = 600))
      ),
      fluidRow(
        column(
          width = 12,
          h4("People and Culture"),
          p("Okinawa has a unique cultural heritage, influenced by its long history of independence before becoming a part of Japan.
The population consists of both indigenous Ryukyuan people and those who moved from mainland Japan, creating a rich cultural tapestry.
Okinawans are known for their unique dialect, traditions, and a strong sense of community. Okinawa has a rich history that dates back over 2,000 years, including its time as the Ryukyu Kingdom.
The region played a significant role in various historical events, including World War II, with several U.S. military bases still present today.")
        )
      ),
      
      fluidRow(
        column(
          width = 12,
          h3("Population Trend in Okinawa"),
          plotOutput("Population", height = 400)
        )
      )
    ),
    
    # Mortality Trend Panel
    tabPanel(
      title = "Strength",
      fluidRow(
        column(
          width = 12,
          h3("Longevity in Okinawa - A formidable strength"),
          plotOutput("mortalityAgeTrend", height = 500),
          p("Okinawa is renowned worldwide for its exceptionally high life expectancy and remarkable longevity rates. 
            Known as one of the “Blue Zones,” regions with an unusually high number of centenarians and healthy, long-lived residents, Okinawa has attracted significant attention for its unique lifestyle factors that contribute to extended lifespans. Most importantly, this streak has been stable over the years without any decrease."),
          p("Okinawa’s average life expectancy for women is approximately 87 years, and for men, it is around 81 years—significantly higher than the national average in Japan and far beyond the global average.
            Additionally, Okinawa has one of the highest concentrations of centenarians per capita, with many residents living well into their 90s and even surpassing 100 years of age.")
        )
      ),
      fluidRow(
        column(
          width = 12,
          h3("Under-1 and Under-5 Mortality Trends"),
          plotOutput("underFiveTrend", height = 400)
        )
      ),
      fluidRow(
        column(
          width = 12,
          h3("World vs. US Mortality Age Trends"),
          plotOutput("globalMortalityTrend", height = 400),
          p("As compared to leading country like US and rest of the world, which is forcasted to touch the life expectancy of 80 years in the nearby future, we can see the clear difference between Healthy lifestyle in Okinawa and the rest of the world.")
        )
      )), 
    
    tabPanel(
      title = "Weakness",
      fluidRow(
        column(
          width = 12,
          h3("Ranking of Causes of Death in 2011 vs. 2021"),
          tableOutput("deathCauseRanking"),
          p("Alzheimer’s Disease has emerged as a significant health concern in Okinawa, reflecting broader global trends in aging populations. 
            The region, known for its longevity and healthy lifestyle, faces challenges associated with neurodegenerative diseases, 
            with Alzheimer's ranking as the leading cause of death among non-communicable diseases.
            Okinawa, being located far from mainland Japan, faces challenges in accessing healthcare resources. 
            The limited number of hospitals and specialized healthcare facilities on the islands can exacerbate the difficulty in managing complex diseases such as Alzheimer's.")
        )
      ),
      fluidRow(
        column(
          width = 12,
          h3("Causes of Death and Disability combined"),
          plotOutput("causeChangeBar", height = 400),
          p("Okinawa has a relatively high rate of diabetes compared to other regions in Japan. Factors contributing to this include dietary habits, sedentary lifestyles, and genetic predispositions. 
            The traditional Okinawan diet, while nutritious in many ways, has shifted over time towards higher sugar and refined carbohydrate consumption, leading to increased risks of developing type 2 diabetes.")
        )
      ),
      fluidRow(
        column(
          width = 12,
          h3("Death and Disability Causes comparison to other places in Japan"),
          plotOutput("causeHeatmap", height = 800)
        )
      ),
      fluidRow(
        column(
          width = 12,
          h3("Corona cases in Okinawa"),
          HTML('<iframe width="560" height="315" src="https://okinawa.stopcovid19.jp/en/cards/number-of-confirmed-cases?embed=true" frameborder="0"></iframe>')
        )
      )
      ),
    
    tabPanel(
      title = "Opportunities",
      fluidRow(
        column(
          width = 12,
          h3("Fertility Rate in Okinawa and Rest of Japan"),
          p("It is quite known that Japan has long faced issues related to fertility rate which has been declining since 1970s.
            This is however not the case for Okinawa, where fertility rate is higher than national average of 1.4. It is often around 1.8, which has
            given hope to possible solution to ageing problem in the Japan. Thus, Okinawa has been recently center of studies being conducted on fertility rate in Japan which seems like an 
            exciting opportunity to explore as Okinawa Institute of Technology has been conducting a lot of studies on it"),
          plotOutput("Fertility", height = 400),
        )
      ),
      fluidRow(
        column(
          width = 12,
          h3("Age group ratio in Okinawa and Japan, 2022"),
          p("In addition to the Fertility rate, Okinawa surpriginly has more younger generation than the rest of Japan. Even though ageing issue still persists, statistics show that
            there is slight improvement in the ratios when it comes to Okinawa. This has also been one of the exciting opportunities for Okinawans."),
          plotOutput("Age", height = 400),
        )
      ),
      fluidRow(
        column(
          width = 12, 
          h3("Tourism in Okinawa"),
          HTML('<img src="https://roadgenius.com/wp-content/uploads/2024/08/tourist-expenditure-okinawa.jpg" alt="Okinawa Tourism" style="height: 400px; width: 600px;">'),
          p(" Data is not quite available for COVID-19 period as tourism decreased by 86% in Okinawa due to being close to China where COVID outbreak happened. However it still remains one of the tourist attractions due to following reasons:"),
          p("1. Beaches : Okinawa is home to some of Japan’s most beautiful beaches, including popular spots like Manza Beach, Emerald Beach, and Sunabe Seawall. The crystal-clear waters and white sandy beaches make it a top destination for snorkeling, diving, and water sports.Skyscanner, a leading UK travel website, ranked Okinawa 6th in the top seven holiday destinations. Okinawa, the only Asian destination on the list, was praised for its 'most beautiful and mysterious beaches' "),

          p("2. Historical and Cultural Sites: Okinawa’s rich history is reflected in its many castles, such as Shuri Castle—a UNESCO World Heritage Site—and historic districts like Naha's Kokusai Street. The region also celebrates unique festivals, such as the Eisa Festival, which showcases traditional Okinawan music and dance."),
          p("3. Home to Oldest People : As a Blue zone, Okinawa has been home to oldest people in the world and many people out of curiosity visit to know what is different on the island that results longetivity")
          )
        )
      ),
    tabPanel(
      title = "Threats",
      fluidRow(
        column(
          width = 12,
          h3("East China Sea : Natural Disasters"),
          p("Okinawa is situated in the East China sea which makes it susceptible to natural disasters such as typhoons, tsunamis, and rising sea levels, which pose significant risks to the local population and infrastructure. This is the most significant threat to the Okinawa. Additionally, due to its location being far away from the mainland Japan, it makes things even harder."),
          HTML('<img src="https://media.npr.org/assets/img/2014/07/08/japan_okinawa-124c7317969c1dec4cb58269e4f8e45a2945c655.jpg?s=1000" alt="Okinawa Typhoon" style="height: 400px; width: 600px;">'),
          p("For example, Typhoon 10W Maysak significantly impacted Okinawa in 2020, bringing severe weather conditions, including strong winds, heavy rainfall, and storm surges, causing widespread damage to the islands.Maysak brought winds of up to 165 km/h (102 mph) and heavy rainfall, which led to widespread flooding, landslides, and damage to infrastructure. Recovery from this took a lot of resources."),
          HTML('<img src="https://www.severe-weather.eu/wp-content/gallery/tropical/cache/typhoon-maysak-ensemble-tracks.png-nggid0519077-ngg0dyn-700x700x100-00f0w010c010r110f110r010t010.png" alt="Okinawa Typhoon" style="height: 400px; width: 600px;">'),
          HTML('<img src="https://www.severe-weather.eu/wp-content/gallery/tropical/cache/typhoon-maysak-future-track.gif-nggid0519076-ngg0dyn-700x700x100-00f0w010c010r110f110r010t010.gif" alt="Okinawa Typhoon" style="height: 400px; width: 600px;">')
        )
      )
    ))
    
  )


server <- function(input, output) {

  output$map <- renderLeaflet({
    okinawa_coords <- c(26.2124, 127.6809)
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = okinawa_coords[2], lat = okinawa_coords[1], zoom = 10) %>%
      addMarkers(lng = okinawa_coords[2], lat = okinawa_coords[1], popup = "Okinawa, Japan") %>%
      addCircleMarkers(lng = okinawa_coords[2], lat = okinawa_coords[1], radius = 10, color = "blue", fillOpacity = 0.5, label = "Okinawa")
  })

  output$mortalityAgeTrend <- renderPlot({
    females <- mortality[mortality$Sex == "Females", ]
    males <- mortality[mortality$Sex == "Males", ]
    ggplot() +
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
  })
  
  output$underFiveTrend <- renderPlot({
    under1 <- mor_5[mor_5$Age == "Under-1", ]
    under5 <- mor_5[mor_5$Age == "Under-5", ]
    
    ggplot() +
      geom_line(data = under1, aes(x = as.numeric(Year), y = Deaths.per.1.000.live.births, color = "Under-1"), size = 1.2) +
      geom_line(data = under5, aes(x = as.numeric(Year), y = Deaths.per.1.000.live.births, color = "Under-5"), size = 1.2) +
      labs(
        title = "Mortality Rate for Under-1 and Under-5 Children",
        x = "Year",
        y = "Deaths per 1,000 Live Births"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      scale_color_manual(
        values = c("Under-1" = "blue", "Under-5" = "red"),
        name = "Age Group"
      )
  })

  output$globalMortalityTrend <- renderPlot({
    plot(world$Year, world$Age, type = "l", col = "blue", lwd = 2, xlab = "Year", ylab = "Age", main = "World and US Mortality Age Trends")
    lines(US$Year, US$Age, col = "red", lwd = 2)
    legend("bottomright", legend = c("World", "US"), col = c("blue", "red"), lwd = 2, cex = 0.8)
  })
  
  output$deathCauseRanking <- renderTable(death_cause, striped = TRUE)
  
  output$causeChangeBar <- renderPlot({
    ggplot(death_disable, aes(x = Change, y = Cause, fill = Change)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_gradient(low = "red", high = "green") +
      theme_minimal() +
      labs(title = "Causes of Death and Disability combined",
           x = "Percentage Change",
           y = NULL) +  
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      geom_text(aes(label = Change), position = position_stack(vjust = 0.5), size = 3)
  })
  
  output$causeHeatmap <- renderPlot({
    ggplot(df_heatmap, aes(x = Location, y = variable, fill = value)) +
      geom_tile() +
      scale_fill_gradient(low = "blue", high = "red") +
      geom_text(aes(label = value), color = "black") +
      labs(title = "Death and Disability by Cause Type compared to other places", x = "Location", y = "Cause", fill = "Higher rank (more death and disability)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$Fertility <- renderPlot({
    ggplot(filtered_fertility, aes(x = AREA, y = `Fertility Rate`, fill = AREA)) +
      geom_bar(stat = "identity", width = 0.7) +
      labs(title = "Fertility Rates in Selected Japanese Regions",
           x = "Region",
           y = "Fertility Rate") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set2") +
      geom_text(aes(label = `Fertility Rate`), vjust = -0.3, size = 4) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$Death <- renderPlot({
    ggplot(population1, aes(x = YEAR, y = `Total Deaths`, color = AREA)) +
      geom_line(size = 1.2) +
      scale_y_continuous(labels = scales::comma) +
      scale_color_manual(values = colors) + 
      labs(title = "Total Deaths Over Years for Selected Prefectures",
           x = "Year",
           y = "Total Deaths",
           color = "Prefecture") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  output$Population <- renderPlot({ 
    ggplot(okinawa_data, aes(x = YEAR)) +
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
    
  })
  output$Age <- renderPlot({
    ggplot(population_age_melt, aes(x = AREA, y = value, fill = variable)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = "Population Distribution Across Age Groups in Prefectures", 
           x = "Prefecture", y = "Population (%)") +
      theme_minimal() +
      scale_fill_manual(values = c("Under_15" = "skyblue", "15-64" = "orange", "Over_65" = "green"))
  })
  
}

shinyApp(ui = ui, server = server)



