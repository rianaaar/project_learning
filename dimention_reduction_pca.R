# cor() dari library stats untuk menghitung korelasi matrik.
# eigen() dari library base untuk menghitung nilai eigen dan vektor eigen
# prcomp() dari library stats untuk analisa Principal Component
# princomp() dari library stats untuk analisa Principal Component
# screeplot() dari library stats untuk membuat screeplot
# biplot() dari library stats untuk membuat biplot.
# autoplot() dari package ggfortify untuk membuat biplot.
# fviz_pca_ind() dari package factoextra untuk membuat biplot.
# Langkah-langkah reduksi dimensi dengan PCA(variabilitas data trbsr) yang sudah dilakukan adalah:
# Melakukan standarisasi data
# Menghitung matrik korelasi
# Menghitung nilai eigen dan vektor eigen
# Memilih banyaknya principal component dengan Screeplot dan kriterion Kaiser
# Visualisasi output
# Langkah 1 sampai dengan 3 dapat secara otomatis dilakukan oleh R.

library(openxlsx)
df <- read.xlsx("https://academy.dqlab.id/dataset/dqlab_pcadata.xlsx", sheet="3varb")

#standarisasi variabel (centering dan scaling)
df <- scale(df, center = TRUE, scale = TRUE)
head(df, 3)
#matrix korelasi
cormat <- cor(df)
cormat
#menghitung nilai eigen dan vektor eigen
eig <- eigen(cormat)
eig
#kontribusi PC1, PC2 dan PC3 terhadap variabilitas data
round(eig$values/ncol(df),3)
round(cumsum(eig$values/ncol(df)),3)
#Dengan menggunakan batasan 80%, maka hanya 2 principal component yang diperlukan 
#untuk menjelaskan 99% variabilitas data, yaitu PC1 dan PC2
pr.out <- prcomp(df, scale. = TRUE, center = TRUE)
pr.out
summary(pr.out)
#Banyaknya PC(p) yang dipilih adalah pada saat terjadi penurunan varians yang tajam sebelum penurunan varians melandai.
#Pada contoh ini banyaknya PC yang dipilih adalah 2.
library(factoextra)
fviz_eig(pr.out, addlabels = TRUE)
screeplot(pr.out, type = "line")
abline(h = 1, lty = 3, col = "red")
#Biplot
pr.out$rotation
biplot(pr.out, scale = 0)
#Skor variabel lama akan ditransformasi menjadi skor variabel baru
df_new <- df %*% pr.out$rotation
df_new[1:6,1:2]

#########
#Panggil library openxlsx untuk membaca file data Excel
library(openxlsx)
#Baca data pada sheet "3varb" dalam file https://academy.dqlab.id/dataset/dqlab_pcadata.xlsx
#dan simpan data dengan nama df_raw
df_raw <- read.xlsx("https://academy.dqlab.id/dataset/dqlab_pcadata.xlsx", sheet="3varb")
#Tampilkan struktur data 
str(df_raw)
#Tampilkan beberapa baris observasi dengan fungsi head()
head(df_raw)
#Lakukan analisa PCA dengan fungsi prcomp() dan
#simpan output ke dalam obyek dengan nama pr.out
pr.out <- prcomp(df_raw, center = TRUE, scale = TRUE,  retx = TRUE)
#Tampilkan komponen output fungsi prcomp()
names(pr.out)
#Tampilkan output PCA dengan memanggil obyek pr.out
pr.out
#Tampilkan summary dari output PCA
summary(pr.out)
#Gambarkan Screeplot dengan menggunakan fungsi screeplot()
screeplot(pr.out, type = "line")

#Tambahkan garis horizontal sebagai panduan untuk menggunakan kriteria Kaiser
abline(h = 1,  col = "red", lty = 3)
#Gambarkan biplot dengan menggunakan fungsi biplot()
#Gunakan opsi scale = 0 agar panjang vektor sesuai dengan kontribusi variabel.
biplot(pr.out, scale = 0)

###PCA 4 Variable
#Teknik sampling yang akan digunakan adalah stratified sampling dengan menggunakan risk rating sebagai stratum. Alasan menggunakan stratified sampling adalah agar risk rating yang berbeda-beda bisa terwakili dalam dataset training dan mencegah kemungkinan memperoleh PCA yang dihasilkan dari training dataset yang hanya mengandung risk rating tertentu.
#Proporsi untuk setiap risk rating pada training set akan diterapkan untuk testing set. PCA akan diterapkan pada training dataset. 
#Panggil library openxlsx untuk membaca file data Excel
library(openxlsx)
csdat_raw <- read.xlsx("https://academy.dqlab.id/dataset/dqlab_pcadata.xlsx", sheet = "csdata")
str(csdat_raw)
head(csdat_raw)
summary(csdat_raw)
library(ggplot2)
ggplot(csdat_raw, aes(as.factor(dependents), income)) +
  geom_boxplot() + xlab("Dependents") + ggtitle("Boxplot Income Berdasarkan Dependents")
#Untuk mencari index baris yang memiliki risk rating 1, 2 dan seterusnya
index1 <- which(csdat_raw$riskrating == 1)
index2 <- which(csdat_raw$riskrating == 2)
index3 <- which(csdat_raw$riskrating == 3)
index4 <- which(csdat_raw$riskrating == 4)
index5 <- which(csdat_raw$riskrating == 5)
#Banyaknya data untuk tiap-tiap risk rating yang dialokasikan untuk training set
ntrain1 <- round(0.8 * length(index1))
ntrain2 <- round(0.8 * length(index2))
ntrain3 <- round(0.8 * length(index3))
ntrain4 <- round(0.8 * length(index4))
ntrain5 <- round(0.8 * length(index5))
#sampel data tiap-tiap risk rating untuk dialokasikan ke dalam training set
set.seed(100)
train1_index <- sample(index1, ntrain1)
train2_index <- sample(index2, ntrain2)
train3_index <- sample(index3, ntrain3)
train4_index <- sample(index4, ntrain4)
train5_index <- sample(index5, ntrain5)
#untuk mengalokasikan data untuk testing set. Indeks baris yang dipilih adalah indeks baris yang tidak diambil dalam proses sampling untuk training set
test1_index <- setdiff(index1, train1_index)
test2_index <- setdiff(index2, train2_index)
test3_index <- setdiff(index3, train3_index)
test4_index <- setdiff(index4, train4_index)
test5_index <- setdiff(index5, train5_index)
#Menggabungkan hasil sampling masing-masing rating ke dalam training set
csdattrain <- do.call("rbind", list(csdat_raw[train1_index,],
                                    csdat_raw[train2_index,], csdat_raw[train3_index,],
                                    csdat_raw[train4_index,], csdat_raw[train5_index,]))
cstrain <- subset(csdattrain, select =
                    -c(contractcode,riskrating))
csdattest <- do.call("rbind", list(csdat_raw[test1_index,],
                                   csdat_raw[test2_index,], csdat_raw[test3_index,],
                                   csdat_raw[test4_index,], csdat_raw[test5_index,]))
cstest <- subset(csdattest,
                 select = -c(contractcode,riskrating))
#korelasi
cor(cstrain)
#pca
pr.out <- prcomp(cstrain, scale = TRUE, center = TRUE)
pr.out
summary(pr.out)
#visualisasi
screeplot(pr.out, type = "line", ylim = c(0,2))
abline(h = 1, lty = 3, col = "red")
biplot(pr.out, scale = 0)
predboject <- predict(pcaobject, newdata = newdataset)
#Data pada predobject$x dapat disimpan untuk digunakan sebagai input algoritma lain.
#Keterbatasan PCA adalah proses pemilihan PC hanya dilakukan dengan variabel-variabel prediktor. Oleh karena itu PCA sebaiknya bukan digunakan sebagai model namun digunakan sebagai teknik preprocessing data untuk kemudian menjadi input metode lain. Alternatif lain adalah menggunakan metode Partial Least Squares yang melibatkan variabel respons dalam reduksi dimensi.