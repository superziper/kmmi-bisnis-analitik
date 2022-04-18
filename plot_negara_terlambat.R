data1 <- read.csv("D:/Kulyah/kmmi bisnis analitik/Dataset 1.csv",sep=",")
library(dplyr)
library(ggplot2)

data.negara.terlambat <- select(data1, Negara.order, Status.terkirim)
data.negara.terlambat <- data.negara.terlambat[grep("Late",data.negara.terlambat$Status.terkirim),]

jumlah.negara.terlambat <- data.negara.terlambat %>%
  count(Negara.order)

jumlah.negara.terlambat <- jumlah.negara.terlambat[order(-jumlah.negara.terlambat$n),]

ggplot(head(jumlah.negara.terlambat,10),aes(x=reorder(Negara.order,n),y=n),fill=Negara.order) +
  geom_bar(stat="identity",fill="lightgreen") +
  coord_flip() + 
  geom_text(aes(label=n),hjust=1.2) +
  labs(x="",
        y="Jumlah order terlambat datang",
        title="Jumlah order terlambat per negara"
  )
