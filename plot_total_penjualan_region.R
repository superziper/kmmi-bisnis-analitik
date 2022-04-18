data1 <- read.csv("D:/Kulyah/kmmi bisnis analitik/Dataset 1.csv",sep=",")
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(hrbrthemes)
data <- select(data1,tanggal.order..DateOrders.,pasar)

data <- separate(data, tanggal.order..DateOrders., c("date", "time"), sep = " ")
data$jumlah.penjualan <- as.numeric(ave(data$date, data$date, FUN=length))
data <- data %>% drop_na()
data$date <- as.Date(data$date,format='%d/%m/%Y')

data %>%
  group_by(Monthly=floor_date(date,unit = "month"),pasar) %>% 
  summarise(Total_Sales=sum(jumlah.penjualan)) %>% 
  ggplot(aes(Monthly,Total_Sales))+
  geom_line(aes(color=pasar),size=1.5)+
  theme_light()+
  scale_y_comma()+
  scale_x_date(date_labels = "%b-%Y",breaks="3 month") +
  labs(title = "Total penjualan per region",y="Total penjualan",x="")
