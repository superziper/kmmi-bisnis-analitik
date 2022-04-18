data1 <- read.csv("D:/Kulyah/kmmi bisnis analitik/Dataset 1.csv",sep=",")
library(dplyr)
library(ggplot2)

negara.status <- select(data1, Negara.order, Status.terkirim)
negara.terkirim <- negara.status[-grep("canceled",negara.status$Status.terkirim),]

jumlah.negara.terkirim <- negara.terkirim %>%
  count(Negara.order)
  
jumlah.negara.terkirim <- jumlah.negara.terkirim[order(-jumlah.negara.terkirim$n),]
jumlah.negara.terkirim <- jumlah.negara.terkirim %>%
  mutate(persen=label_percent(accuracy=0.01)(jumlah.negara.terkirim$n/sum(jumlah.negara.terkirim$n)))

ggplot(head(jumlah.negara.terkirim,10),aes(x=reorder(Negara.order,n),y=n)) +
  geom_bar(stat = "identity",fill="lightblue") +
  coord_flip() +
  geom_text(aes(label=n),hjust=-0.2) +
  geom_text(aes(label=persen),hjust=1.5,colour="magenta") +
  labs(x="",
       y="Jumlah order berhasil",
       title="Jumlah order berhasil per negara"
  )


