# kladow house prices

# ---- packages ----

library(tidyverse)
library(rvest)


# ---- downloading data ----

url1 <- "https://www.immobilienscout24.de/Suche/de/haus-kaufen?geocodes=120540000886,110000000505,110000000506,120540000114&enteredFrom=result_list#/"
webpage1 <- read_html(url)
price_data_html1 <- html_nodes(webpage1, ".gutter-vertical-s")
price_data1 <- html_text(price_data_html1)

url2 <- "https://www.immobilienscout24.de/Suche/de/haus-kaufen?geocodes=120540000886,110000000505,110000000506,120540000114&pagenumber=2"
webpage2 <- read_html(url2)
price_data_html2 <- html_nodes(webpage2, ".gutter-vertical-s")
price_data2 <- html_text(price_data_html2)

url3 <- "https://www.immobilienscout24.de/Suche/de/haus-kaufen?geocodes=120540000886,110000000505,110000000506,120540000114&pagenumber=3"
webpage3 <- read_html(url3)
price_data_html3 <- html_nodes(webpage3, ".gutter-vertical-s")
price_data3 <- html_text(price_data_html3)

price_data <- c(price_data1, price_data2, price_data3)


# ---- function that extracts house price from scraped string ----
priceextract <- function(string){
  str_extract(string, "\\d*\\.*\\d+\\.\\d+")
}


# ---- vector of house prices ----

# char vector of house prices in form "d.ddd.ddd"
preise_char <- priceextract(price_data)

# removing dots and converting to numeric vector
preise_temp1 <- str_remove(preise_char, "\\.")
preise_temp1 <- str_remove(preise_temp1, "\\.")
preise_num <- as.numeric(preise_temp1)
preise_num


# ---- vector of house-size ----

# extracting from price_data
livingArea_char <-  str_extract(price_data, "\\d++\\s[m]") # => für kein Komma
temp2 <- str_extract(price_data, "\\d++[,]\\d++\\s[m]") # => nur für mit Komma
for(i in 1:length(livingArea_char)){
  ifelse(!is.na(temp2[i]), livingArea_char[i] <- temp2[i], NA)
}

# removing " m"
temp3 <- str_remove(livingArea_char, " m")
livingArea_char <- str_replace(temp3, ",", ".")

# converting to num
livingArea_num <- as.numeric(livingArea_char)
livingArea_num


# ---- vector of number of rooms ----
noOfRooms_char <- str_extract(price_data, "..Z")
noOfRooms_char <- str_remove(noOfRooms_char, ".Z")
noOfRooms_num <- as.numeric(noOfRooms_char)


# ---- vector of plot area ----
plotArea_char <- str_extract(price_data, "Zi.\\d*[.]*\\d++\\s")
plotArea_char <- str_remove(plotArea_char, "Zi.")
plotArea_char <- str_remove(plotArea_char, " ")
plotArea_char <- str_remove(plotArea_char, "[.]")
plotArea_num <- as.numeric(plotArea_char)
plotArea_num


# ---- complete data frame

haus_data <- data.frame(preis = preise_num, wohnflaeche = livingArea_num, zimmer = noOfRooms_num, grundstuecksflaeche = plotArea_num)
View(haus_data)

# data frame without rows with NAs
house_data <- drop_na(haus_data)


# ---- building a model etc. ----

# exploratory data analysis
summary(house_data)
cor(house_data)

# plots of the data
par(mfrow=c(3,1))
plot(house_data$wohnflaeche, house_data$preis, main = "Wohnfläche vs. Preis")
plot(house_data$zimmer, house_data$preis, main = "Zimmer vs. Preis")
plot(house_data$grundstuecksflaeche, house_data$preis, main = "Grundstücksfläche vs. Preis")

# building a linear regression model
model1 <- lm(preis ~ wohnflaeche, data = house_data)
summary(model1)

model2 <- lm(preis ~ zimmer, data = house_data)
summary(model2)

model3 <- lm(preis ~ grundstuecksflaeche, data = house_data)
summary(model3)

B1 <- model1$coefficients[2]


# ---

full_model <- lm(preis ~ ., data = house_data)
predictions <- predict(full_model)
predictions <- as.data.frame(predictions) %>%
  mutate(id = 1:length(predictions))

predictions <- predictions %>%
  mutate(reihenfolge = order(predictions))

predictions <- predictions %>%
  mutate(true = house_data$preis)

plot(x=predictions$reihenfolge, predictions$predictions, col="red")
#lines(x=predictions$reihenfolge, predictions$predictions, col="red")
points(x=predictions$reihenfolge, predictions$true, col ="blue")
lines(predictions$true, col ="blue")
