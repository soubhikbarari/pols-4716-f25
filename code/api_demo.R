library(tidyverse)
library(httr2)

# Basic REST APIs --------------------------------------------------------------

request("https://disease.sh/v3/covid-19/countries/usa") %>%
  req_perform() %>%
  resp_body_json()

request("https://restcountries.com/v3.1/name/india") %>%
  req_perform() %>%
  resp_body_json()

# APIs with request parameters -------------------------------------------------

request("https://api.open-meteo.com/v1/forecast?latitude=40.71&longitude=-74.01&current=temperature_2m") %>%
  req_perform() %>%
  resp_body_json() 

request("https://api.open-meteo.com/v1/forecast") %>%
  req_url_query(latitude = 40.71, longitude = -74.01,  # NYC
                hourly = "temperature_2m") %>%
  req_perform() %>%
  resp_body_json()

# API with custom request headers ----------------------------------------------

request("https://www.reddit.com/r/politics/top.json?limit=5&t=day") %>%
  # Not all APIs trust that you're human!
  req_headers(
    "User-Agent" = "POLS 4716"
  ) %>%
  req_perform() %>%
  resp_body_json()

# API with API key -------------------------------------------------------------

# You'll need to get (pay for) your own API key for this to work 
# (https://platform.openai.com/)
OPENAI_API_KEY <- readLines("code/openai_api_key.secret") 

request("https://api.openai.com/v1/chat/completions") %>%
  req_headers(
    Authorization = paste("Bearer", OPENAI_API_KEY),
    "Content-Type" = "application/json"
  ) %>%
  # Sometimes parameters need to go in 'body' of request, not URL
  req_body_json(list(
    model = "gpt-4o-mini",
    messages = list(
      list(role = "user", content = "Write a haiku about how wonderful my students are.")
    )
  )) %>% 
  req_perform() %>%
  resp_body_json()

# Extract the output into a dataframe
response <- request("https://api.openai.com/v1/chat/completions") %>%
  req_headers(
    Authorization = paste("Bearer", OPENAI_API_KEY),
    "Content-Type" = "application/json"
  ) %>%
  # Sometimes parameters need to go in 'body' of request, not URL
  req_body_json(list(
    model = "gpt-4o-mini",
    messages = list(
      list(role = "user", content = "Write a haiku about how wonderful my students are.")
    )
  )) %>% 
  req_perform() %>%
  resp_body_json()

# Extract key pieces and flatten
print(response)
df <- tibble(
  model = response$model,
  output = response$choices[[1]]$message$content
)
print(df)


