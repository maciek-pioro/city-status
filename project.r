library(rvest)
library(stringr)
library(dplyr)

function downloadData() {
  data <- tibble(city=character(), year=numeric(), latitude=numeric(), longitude=numeric())
  url <- url("https://pl.wikipedia.org/wiki/Miasta_w_Polsce", 'rb')
  src <- read_html(url)
  close(url)
  cities <- src %>%  html_elements('.mw-parser-output div ul li a')
  cities <- cities[37:990]
  links <- cities %>% html_attr('href')
  for (link in links) {
    url <- url(paste("https://pl.wikipedia.org", link, sep=''), 'rb')
    src <- read_html(url)
    close(url)
    city_name <- src %>% html_element(".naglowek > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(1) > span:nth-child(1)") %>%  html_text2
    fact_rows <- src %>% html_elements(".infobox > tbody > tr")
    print(city_name)
    for(row in fact_rows) {
      row_label <- row %>% html_element("td") %>% html_text2
      if(row_label == "Prawa miejskie") {
        row_content <- ((row %>% html_elements("td"))[2] %>% html_text2)
        year <- row_content %>% str_extract_all(., '[0-9]{4}') %>% unlist %>% strtoi %>% min
        print(year)
      }
    }
    latitude <- (src %>% html_elements('.latitude'))[2] %>% html_text2 %>%  str_replace(., ',', '.') %>% as.double
    longitude <- (src %>% html_elements('.longitude'))[2] %>% html_text2 %>%  str_replace(., ',', '.') %>% as.double
    data <- data %>% add_row(city=city_name, year=year, latitude=latitude, longitude=longitude)
  }
}

map('world2Hires', 'Poland')

for(i in seq(nrow(data))) {
  if(data[[i, 'year']] != Inf){
    intensity = (data[[i, 'year']] - 1209) / 809
    points(data[[i, 'longitude']], data[[i, 'latitude']], pch=19, col=rgb(intensity, 0, 1-intensity))
  }
}