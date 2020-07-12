#Menggunakan library arules
library(arules)

#Membaca transaksi dari file data_transaksi.txt
transaksi <- read.transactions(file="https://academy.dqlab.id/dataset/data_transaksi.txt", format="single", sep="\t", cols=c(1,2), skip=1)

#Menampilkan jumlah kombinasi dari produk yang terdapat pada daftar transaksi yang ada
inspect(apriori(transaksi, parameter = list(support=.1, minlen=2, target='frequent itemsets')))

### menampilkan data transaksi dari data besar
#Menggunakan library arules
library(arules)

#Membaca transaksi dari file data_transaksi2.txt
transaksi <- read.transactions(file="https://academy.dqlab.id/dataset/data_transaksi2.txt", format="single", sep="\t", cols=c(1,2), skip=1)

#Menampilkan jumlah kombinasi dari produk yang terdapat pada daftar transaksi yang ada
inspect(apriori(transaksi, parameter = list(support=.03, minlen=2, target='frequent itemsets')))

####
#Membaca file yang berlokasi di https://academy.dqlab.id/dataset/data_transaksi.txt dengan fungsi read.csv, dan kemudian disimpan pada variable transaksi_tabular
transaksi_tabular <- read.csv("https://academy.dqlab.id/dataset/data_transaksi.txt", sep="\t")

#Menampilkan variable transaksi_tabular dengan fungsi print
print(transaksi_tabular)
#membaca data sebagai transaksi
library(arules)
read.transactions(file="https://academy.dqlab.id/dataset/data_transaksi.txt", format="single", sep="\t", cols=c(1,2), skip=1)
#Menampilkan Daftar Item Transaksi
transaksi <- read.transactions(file="https://academy.dqlab.id/dataset/data_transaksi.txt", format="single", sep="\t", cols=c(1,2), skip=1)
#Menggunakan inspect terhadap transaksi
inspect(transaksi)
transaksi@itemInfo
#Menampilkan Daftar Kode Transaksi
transaksi@itemsetInfo
#Tampilan Transaksi dalam bentuk Matrix
transaksi@data
#Tampilan item frequency plot
itemFrequencyPlot(transaksi)
#kemunculan item
itemFrequency(transaksi, type="absolute")

#TOP 3
data_item <- itemFrequency(transaksi, type="absolute")
#Melakukan sorting pada data_item
data_item <- sort(data_item, decreasing = TRUE)
#Mengambil 3 item pertama
data_item <- data_item[1:3]
#Konversi data_item menjadi data frame dengan kolom Nama_Produk dan Jumlah
data_item <- data.frame("Nama Produk"=names(data_item), "Jumlah"=data_item, row.names=NULL)
print(data_item)

#Menghasilkan association rules dan disimpan sebagai variable mba
mba <- apriori(transaksi)
#RULES dgn support dan confidence
mba <- apriori(transaksi,parameter = list(supp = 0.1, confidence = 0.5))
inspect(mba)
#
#Filter rhs dengan item "Sirup" dan tampilkan
inspect(subset(mba, rhs %in% "Sirup"))
#Filter LHS
inspect(subset(mba, lhs %in% "Gula"))
inspect(subset(mba, lhs %in% "Pet Food" & rhs %in% "Sirup"))
inspect(subset(mba, lhs %in% "Teh Celup" | rhs %in% "Teh Celup"))
inspect(subset(mba, (lhs %in% "Teh Celup" | rhs %in% "Teh Celup") & lift>1))
inspect(subset(mba, (lhs %ain% c("Pet Food", "Gula" ))))
#Hanya terdapat 1 rule sebagai hasil filter, dan dengan lift di atas 1 dan support 0.1 (10%) rule ini bisa dianggap layak untuk meghasilkan rekomendasi item, yaitu Sirup.
#Jadi orang yang membeli Gula dan Pet Food, bisa ditawarin Sirup.
plot(subset(mba, lift>1.1), method="graph")
#Besarnya bulatan menunjukkan popularitas (support), dan intensitas warna menunjukkan lift.
