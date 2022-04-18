data1 <- read.csv("D:/Kulyah/kmmi bisnis analitik/Dataset 1.csv",sep=",")
produk.terjual <- data1[-grep("canceled",data1$Status.terkirim),]
library(dplyr)
library(ggplot2)
library(scales)

jumlah.produk.terjual <- as.data.frame(table(produk.terjual$Product.Name))
jumlah.produk.terjual <- jumlah.produk.terjual[order(-jumlah.produk.terjual$Freq),]
jumlah.produk.terjual <- jumlah.produk.terjual %>%
  mutate(persen=label_percent(accuracy=0.01)(jumlah.produk.terjual$Freq/sum(jumlah.produk.terjual$Freq)))

ggplot(head(jumlah.produk.terjual,10),aes(x=reorder(Var1,Freq),25,y=Freq)) +
  geom_bar(stat="identity",fill="magenta") +
  coord_flip() +
  geom_text(aes(label=Freq),hjust=0,colour="blue") +
  geom_text(aes(label=persen),hjust=1.1,colour="black") +
  labs(x="",
       y="Jumlah order",
       title="Jumlah barang terjual"
  ) 

