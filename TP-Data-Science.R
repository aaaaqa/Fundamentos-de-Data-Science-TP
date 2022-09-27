rm(list=ls(all=TRUE))
cat("\014")
graphics.off()

library(gplots)
library(dplyr)
library(ggplot2) 
library(scales)
library(RColorBrewer)
library(lubridate)
library(tidyverse)

datos <- read.csv('C:/Users/User/Downloads/hotel_bookings.csv', header=TRUE, sep=',',dec='.')

#FACTORIZATION DE VALORES

datos$hotel <- as.factor(datos$hotel)
datos$is_canceled <- as.factor(datos$is_canceled)
datos$meal <- as.factor(datos$meal)
#datos$market_segment <- as.factor(datos$market_segment)
#datos$distribution_channel <- as.factor(datos$distribution_channel)
datos$reserved_room_type <- as.factor(datos$reserved_room_type)
datos$assigned_room_type <- as.factor(datos$assigned_room_type)
datos$deposit_type <- as.factor(datos$deposit_type)
#datos$agent <- as.factor(datos$agent)
datos$company <- as.factor(datos$company)
datos$customer_type <- as.factor(datos$customer_type)
datos$reservation_status <- as.factor(datos$reservation_status)
datos$reservation_status_date <- as.Date(datos$reservation_status_date)

summary(datos$company)

#REORGANIZATION DE FECHA DE LLEGADA

datos$arrival_date_month[datos$arrival_date_month == "January"] <- 1
datos$arrival_date_month[datos$arrival_date_month == "February"] <- 2
datos$arrival_date_month[datos$arrival_date_month == "March"] <- 3
datos$arrival_date_month[datos$arrival_date_month == "April"] <- 4
datos$arrival_date_month[datos$arrival_date_month == "May"] <- 5
datos$arrival_date_month[datos$arrival_date_month == "June"] <- 6
datos$arrival_date_month[datos$arrival_date_month == "July"] <- 7
datos$arrival_date_month[datos$arrival_date_month == "August"] <- 8
datos$arrival_date_month[datos$arrival_date_month == "September"] <- 9
datos$arrival_date_month[datos$arrival_date_month == "October"] <- 10
datos$arrival_date_month[datos$arrival_date_month == "November"] <- 11
datos$arrival_date_month[datos$arrival_date_month == "December"] <- 12

datos$arrival_date <- as.Date(with(datos, paste(datos$arrival_date_year, datos$arrival_date_month, 
                                                datos$arrival_date_day_of_month, sep="-")), "%Y-%m-%d")

#DESHACERSE DE FECHAS DE LLEGADA SEPARADAS

datos <- datos[-c(4:5, 7)]

#REORGANIZATION DE MEAL

datos$meal[datos$meal == "Undefined"] <- "SC"

#REMOVE DE LEVEL NO UTILIZADO

datos$meal <- droplevels(datos$meal)

#REMOVER UNDEFINED DE MARKET_SEGMENT

get_mode <- function(x){
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}

result <- get_mode(datos$market_segment)

datos$market_segment[datos$market_segment == "Undefined"] <- result

datos$market_segment <- as.factor(datos$market_segment)

#REMOVER UNDEFINED DE DISTRIBUTION_CHANNEL

datos$distribution_channel[datos$distribution_channel == "Undefined"] <- get_mode(datos$distribution_channel)

datos$distribution_channel <- as.factor(datos$distribution_channel)

#REMOVER NULL DE AGENT

datos$agent[datos$agent == 'NULL'] <- NA

rand.valor <- function(x){ 
  faltantes <- is.na(x)
  tot.faltantes <- sum(faltantes) 
  x.obs <- x[!faltantes]
  valorado <- x 
  valorado[faltantes] <- sample(x.obs, tot.faltantes, replace = TRUE)
  return (valorado) 
}

datos$agent <- rand.valor(datos$agent)

#Comparison between distribution channels

ggplot(data = datos,aes(x=distribution_channel,fill=distribution_channel))+
  geom_bar()+
  labs(title = "Comparison de los canales de distribución")

# Density per days table

datos$arrival_date <- as.factor(datos$arrival_date)
dates <- summary(as.factor(format(as.Date(datos$arrival_date, format="%Y-%m-%d"), format= "%d")))
day_numbers <- matrix(NA,ncol=7,nrow=5)

b <- 1
for(x in 1:5){
  for(y in 1:7){
    if(b <= 31){
      day_numbers[x,y] <- b
      b <- b+1
    }
  }
}

M <- matrix(0, ncol=7, nrow=5)
a <- 1

for(x in 1:5){
  for(y in 1:7){
    if(a <= length(dates)){
      M[x, y] <- dates[[a]]
      a <- a+1
    }
  }
}

colnames(M) <- paste0(1:7)
rownames(M) <- paste0(1:5)

heatmap.2(M, main = "Density per days", col=colorRampPalette(brewer.pal(8, "Blues"))(25), Rowv=FALSE, Colv=FALSE, dendrogram="none", colsep=1:6, rowsep=1:4,
          sepcolor="black", density.info="none", trace="none", labRow=FALSE, labCol=FALSE, key=FALSE, 
          cellnote=day_numbers, notecol="black")

arrival_date_month <- month.abb[as.factor(format(as.Date(datos$arrival_date, format="%Y-%m-%d"), format= "%m"))]

str_stack <- function(x) {
  x %>% str_split("") %>% map(~ .x %>% paste(collapse = "\n"))
}

# Booking by month (An hotel comparison)

ggplot(datos, aes(arrival_date_month, fill = hotel)) +
  geom_bar(position = position_dodge()) +
  scale_x_discrete(label = str_stack)
labs(title = "Booking by Month",
     x = "Month",
     y = "Count") + theme(axis.text.x=element_text(angle=90,hjust=1))+
  theme_bw(base_size = 16) 

# Amount of special requirements per market segment

total_special_requirements <- as.factor(datos$total_of_special_requests)

ggplot(datos, aes(total_special_requirements, fill = market_segment)) +
  geom_bar(position = position_dodge()) +
  scale_x_discrete(label = str_stack)
labs(title = "Special requirements per market segment",
     x = "Special requirements",
     y = "Count") + theme(axis.text.x=element_text(angle=90,hjust=1))+
  theme_bw(base_size = 16) 