library(arules)
transaksi <- read.transactions(file="transaksi_dqlab_retail.tsv", format="single", sep="\t", cols=c(1,2), skip="1")
data_item <- itemFrequency(transaksi, type="absolute")
#TOP 10
data_item <- sort(data_item, decreasing = TRUE)
data_item <- data_item[1:10]
data_item <- data.frame("Nama Produk"=names(data_item), "Jumlah"=data_item, row.names=NULL)
write.csv(data_item, file="top10_item_retail.txt", sep=",")
#Bottom 10
data_item <- sort(data_item)
data_item <- data_item[1:10]
data_item <- data.frame("Nama Produk"=names(data_item), "Jumlah"=data_item, row.names=NULL)
write.csv(data_item, file="bottom10_item_retail.txt", sep=",")
#kombinasi menarik
mba <- apriori(transaksi,parameter = list(supp = 10/length(transaksi), confidence = 0.5, minlen=2, maxlen=3))
data_item <- subset(mba, lift>1)
data_item <- sort(data_item,by="lift",descreasing=TRUE)
data_item <- data_item[1:10]
write(data_item, file="kombinasi_retail.txt")
#Mencari Paket Produk yang bisa dipasangkan dengan Item Slow-Moving (penjualannya lambat)
data_item1 <- subset(mba, (rhs %in% "Tas Makeup") & lift>1)
data_item1 <- sort(data_item1, by="lift",descreasing=TRUE)
data_item1 <- data_item1[1:3]

data_item2 <- subset(mba, (rhs %in% "Baju Renang Pria Anak-anak") & lift>1)
data_item2 <- sort(data_item2, by="lift",descreasing=TRUE)
data_item2 <- data_item2[1:3]
hasil <- c(data_item1,data_item2)
write(hasil, file="kombinasi_retail_slow_moving.txt")