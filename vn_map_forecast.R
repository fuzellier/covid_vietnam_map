packages <- c('tidyverse', 'rvest', 'rnaturalearth', 'tmap', 'stringi', 'rstan', 'prophet')

## Load packages, or install then load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

url <- 'https://ncov.moh.gov.vn/'
webpage <- read_html(url)

# Number of items on the page
iter <- webpage %>% 
  html_nodes("#sailorTable tbody tr") %>% length()

# Scraping function
covnData <- function() {
  
  province_list <- list()
  cases_list    <- list()
  death_list    <- list()
  
  for (j in 2:iter) { # Skip the first <tr> (= column names)
    
    province <- webpage %>% 
      html_nodes(paste0("#sailorTable tr:nth-child(", j, ") td:first-child")) %>% 
      html_text(trim = TRUE)
    
    province_list <- c(province_list, province)
    
    cases <- webpage %>% 
      html_nodes(paste0("#sailorTable tr:nth-child(", j, ") td:nth-child(2)")) %>% 
      html_text(trim = TRUE) %>% gsub("\\.", "", .)
    
    cases_list <- c(cases_list, cases)
    
    death <- webpage %>% 
      html_nodes(paste0("#sailorTable tr:nth-child(", j, ") td:last-child")) %>% 
      html_text(trim = TRUE)
    
    death_list <- c(death_list, death)
    
  }
  
  data <- list(province_list, cases_list, death_list)
  
  # Building the df from lists
  df <- data %>% do.call(cbind, .) %>% as_tibble()
  colnames(df) <- c("province", "cases", "death")
  
  # Parsing columns
  df <- df %>% mutate_at(vars(cases, death), as.integer)
  df <- df %>% mutate_at(vars(province), as.character)
  
  return(df)
  
}

# COVID VN data
covn_data <- covnData()

# Remove vietnamese diacritics
covn_data$province <- stringi::stri_trans_general(covn_data$province, "Latin-ASCII")

covn_data$province <- gsub('^(TP.)\\s', '', covn_data$province)

# Vietnam country data
vietnam <- ne_states(country = "Vietnam", returnclass = "sf")

# Subset of vietnam data
vietnam <- vietnam %>%
  select(gn_name, latitude, longitude)

colnames(vietnam)[1] <- "province"

# Formatting province names
vietnam$province <- gsub('^(Tinh|Thanh Pho|Thanh pho|Huyen)\\s', '', vietnam$province)
vietnam$province <- gsub('-', ' - ', vietnam$province)
vietnam$province <- gsub('- Hue', 'Hue', vietnam$province)

# Outer join between Vietname and COVID data
vn_data <- merge(x=vietnam, y=covn_data, by="province", all=TRUE)

# Mapping data
tmap_mode(mode = "view")

tm_basemap("Stamen.Watercolor") +
  tm_shape(vn_data) +
  tm_fill(col="cases", breaks = c(0, 25, 100, 250, 500, 1000, 5000, 10000, Inf), palette="PuRd", alpha=0.8) +
  tm_borders("white", lwd=1)


## FORECASTING

# Get detailed COVID data from GitHub repo
df <- read.csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv')

# Subset where location: desired value (here "Vietnam")
df <- df[which(df$location == "Vietnam"), ]

# Row index = date
rownames(df) <- df$date

# Building df for Prophet
df_fb <- data.frame(
  ds=as.Date(rownames(df)),
  y=as.numeric(df$total_cases),
  stringsAsFactors=FALSE
)

model <- prophet(df_fb)

# Forecasting over the next 30 days
future <- make_future_dataframe(model, periods = 30)
tail(future)

# Prediction
forecast <- predict(model, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

# Plotting
dyplot.prophet(model, forecast)
prophet_plot_components(model, forecast)