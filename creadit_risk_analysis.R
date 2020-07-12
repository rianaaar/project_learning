library("openxlsx")
library("C50")
#Mempersiapkan data
dataCreditRating <- read.xlsx(xlsxFile = "https://academy.dqlab.id/dataset/credit_scoring_dqlab.xlsx")
dataCreditRating$risk_rating <- as.factor(dataCreditRating$risk_rating) 
#Menggunakan C5.0
drop_columns <- c("kpr_aktif", "pendapatan_setahun_juta", "risk_rating", "rata_rata_overdue")
datafeed <- dataCreditRating[ , !(names(dataCreditRating) %in% drop_columns)]
modelKu <- C5.0(datafeed, as.factor(dataCreditRating$risk_rating))
summary(modelKu)

library("openxlsx")
library("C50")

#Mempersiapkan data
dataCreditRating <- read.xlsx(xlsxFile = "https://academy.dqlab.id/dataset/credit_scoring_dqlab.xlsx")
#Mempersiapkan class dan input variables 
dataCreditRating$risk_rating <- as.factor(dataCreditRating$risk_rating) 
input_columns <- c("durasi_pinjaman_bulan", "jumlah_tanggungan")
datafeed <- dataCreditRating[ , input_columns ]
#Mempersiapkan training dan testing set
set.seed(100) #untuk menyeragamkan hasil random antar tiap komputer
indeks_training_set <- sample(900, 800)
#Membuat dan menampilkan training set dan testing set
input_training_set <- datafeed[indeks_training_set,]
class_training_set <- dataCreditRating[indeks_training_set,]$risk_rating
input_testing_set <- datafeed[-indeks_training_set,]
input_testing_set$risk_rating <- dataCreditRating[-indeks_training_set,]$risk_rating
#menghasilkan model
risk_rating_model <- C5.0(input_training_set, class_training_set, control = C5.0Control(label="Risk Rating"))
summary(risk_rating_model)
plot(risk_rating_model)
#menyimpan hasil prediksi testing set ke dalam kolom hasil_prediksi
input_testing_set$hasil_prediksi <- predict(risk_rating_model, input_testing_set)
print(input_testing_set)
#membuat confusion matrix
library("reshape2")
dcast(hasil_prediksi ~ risk_rating, data=input_testing_set)
nrow(input_testing_set[input_testing_set$risk_rating==input_testing_set$hasil_prediksi,])

#Membuat data frame aplikasi baru
aplikasi_baru <- data.frame(jumlah_tanggungan = 6, durasi_pinjaman_bulan = 12)
print(aplikasi_baru)
