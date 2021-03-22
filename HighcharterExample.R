library(tidyverse)
library(forecast)
library(highcharter)

hchart(mpg, "bar", 
       hcaes(x = displ, y = hwy, group = class))

forecast(AirPassengers) %>% hchart()


hchart(mpg, "bar", hcaes(x=class))


data(unemployment)

hcmap("countries/us/us-all-all", data = unemployment,
      name = "Unemployment", value = "value", joinBy = c("hc-key", "code"),
      borderColor = "transparent") %>%
  hc_colorAxis(dataClasses = color_classes(c(seq(0, 10, by = 2), 50))) %>% 
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0, valueSuffix = "%") 


libr

mapdata <- get_data_from_map(download_map_data("countries/br/br-all"))

dados_falsos <- mapdata %>% 
  select(code = `hc-a2`) %>% 
  mutate(taxa = round(rbeta(n = nrow(.), shape1 = 1, shape2 = 1), 2))

dclass <- tibble(from = seq(0, 0.9, by = 0.1),
                 to = seq(0.1, 1, by = 0.1),
                 color = colorRampPalette(colors = c("SteelBlue",
                                                     "IndianRed"))(length(from)))
dclass <- list_parse(dclass)

hcmap("countries/br/br-all", data = dados_falsos,
      name = "Estado",
      value = "taxa", joinBy = c("hc-a2", "code")
      ) %>%
  hc_colorAxis(dataClasses = dclass) 
# %>%
#   frameWidget()


hcmap("countries/br/br-all", data = dados_falsos,
      name = "Estado",
      value = "taxa", joinBy = c("hc-a2", "code")
      ) %>%
  hc_colorAxis( minColor = "#FFFFFF", maxColor = "#434348",
                type = "logarithmic")
# %>%
#   frameWidget()


nyears <- 5

df <- expand.grid(seq(12) - 1, seq(nyears) - 1)
df$value <- abs(seq(nrow(df)) + 10 * rnorm(nrow(df))) + 10
df$value <- round(df$value, 2)
ds <- list_parse2(df)


hc <- highchart() %>%
  hc_chart(type = "heatmap") %>%
  hc_title(text = "Simulated values by years and months") %>%
  hc_xAxis(categories = month.abb) %>%
  hc_yAxis(categories = 2016 - nyears + seq(nyears)) %>%
  hc_add_series(name = "value", data = ds)

hc_colorAxis(hc, minColor = "#FFFFFF", maxColor = "#434348")

hc_colorAxis(hc, minColor = "#FFFFFF", maxColor = "#434348",
             type = "logarithmic")


require("viridisLite")

n <- 4
stops <- data.frame(q = 0:n/n,
                    c = substring(viridis(n + 1), 0, 7),
                    stringsAsFactors = FALSE)
stops <- list_parse2(stops)

hc_colorAxis(hc, stops = stops, max = 75)



library(RGtk2)
