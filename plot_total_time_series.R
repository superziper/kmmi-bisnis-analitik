data1 <- read.csv("D:/Kulyah/kmmi bisnis analitik/Dataset 1.csv",sep=",")
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(hrbrthemes)
data <- select(data1,tanggal.order..DateOrders.,Negara.order)

data <- separate(data, tanggal.order..DateOrders., c("date", "time"), sep = " ")
data$date <- as.Date(data$date,format='%d/%m/%Y')
data.india <- data[grep("India",data$Negara.order),]
data.china <- data[grep("China",data$Negara.order),]
data.prancis <- data[grep("Francia",data$Negara.order),]
data.jerman <- data[grep("Alemania",data$Negara.order),]
data.australia <- data[grep("Australia",data$Negara.order),]

jumlah.india <- data.india %>% count(date)
jumlah.china <- data.china %>% count(date)
jumlah.prancis <- data.prancis %>% count(date)
jumlah.jerman <- data.jerman %>% count(date)
jumlah.australia <- data.australia %>% count(date)
jumlah.india$negara <- rep("India",nrow(jumlah.india))
jumlah.china$negara <- rep("China",nrow(jumlah.china))
jumlah.prancis$negara <- rep("Prancis",nrow(jumlah.prancis))
jumlah.jerman$negara <- rep("Jerman",nrow(jumlah.jerman))
jumlah.australia$negara <- rep("Australia",nrow(jumlah.australia))
data <- rbind(jumlah.china,jumlah.india,jumlah.prancis,jumlah.jerman,jumlah.australia)
data <- data %>% drop_na()

data %>%
  group_by(Monthly=floor_date(date,unit = "1 weeks"),negara) %>% 
  summarise(Total_Sales=sum(n)) %>% 
  ggplot(aes(Monthly,Total_Sales))+
  geom_line(aes(color=negara),size=1)+
  theme_light()+
  scale_y_comma()+
  scale_x_date(date_labels = "%m-%Y") +
  labs(title = "Total penjualan per negara",subtitle = "Ada penurunan di 2017",y="Total penjualan",x="")
