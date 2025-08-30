library(readxl)
library(forecast)
library(TTR)
library(imputeTS)
library(tseries)
library(ggplot2)
library(dplyr)
library(graphics)
library(TSA)
library(tidyverse)
library(lubridate)
library(gridExtra)
library(ggfortify)
library(cowplot)
library(lmtest)
library(stats)
library(MASS)
library(fpp2)
library(FinTS)
library(car)
library(nortest)
library(fGarch)
library(rugarch)
library(aTSA)
library(dynlm)
library(psych)
library(moments)

#Deklarasi Data
data <- read_excel("C:/Users/ASUS/Downloads/weather_data_nyc_centralpark_2016(1).xlsx")
data <- data[, c("date", "maximum temperature")]
ts.data <- ts(data)
kableExtra::kable(head(data) ,caption = 'Subset Data Weather 2016')

#statistika deskriptif
summary(data)
describe(data)
z <- data$`maximum temperature`
boxplot(z, xlab="Suhu Cuaca Maksimal")

#Time Series Plot
DATES = seq(as.POSIXlt("2016-01-01 00:00:00", tz="UTC"), 
            as.POSIXlt("2016-12-31 00:00:00", tz="UTC"), length.out=40000)
dataset = data.frame(
  Outcome = c(rpois(20,1000),rpois(20,2000)),
  Week_End_Date = rep(DATES,2),
  Group = rep(c(FALSE,TRUE),each=20)
)
dataset$Condition = ifelse(
  dataset$Week_End_Date < as.POSIXlt("2016-03-31 00:00:00", tz="UTC"),
  "Musim Panas dan Musim Gugur",
  "Musim Dingin dan Musim Semi"
)
FRAME = dataset %>% group_by(Condition) %>% 
  summarize(xmin=min(Week_End_Date),xmax=max(Week_End_Date))

p <- ggplot(data, aes(x=date, y=`maximum temperature`)) +
  geom_line(lwd=1.2,col="#E26A2C")
p +labs( x="Month",y = "Suhu Cuaca (celcius)",
         title="Time Series Plot Suhu Cuaca Newyork 2016 ",
         subtitle = "Periode Juli 2012 - Juli 2022")+
  theme_bw()+
  theme(
    plot.title = element_text(size = 14L,
                              face = "bold",
                              hjust = 0.5),
    plot.subtitle = element_text(size = 11L,
                                 face = "plain",
                                 hjust = 0.5)
  )+
  geom_rect(data=FRAME,inherit.aes=FALSE,
            aes(xmin=xmin,xmax=xmax,ymax=+Inf,ymin=-Inf,fill=Condition),
            alpha=0.1)+
  scale_fill_manual(values=c("red","blue"))


# Plot ini mengalami penurunan ketika kondisi perekonomian mulai membaik. 
# Pola data harga penutupan emas yang terbentuk dari data tidak stasioner yang cenderung siklis dan tren positif.

#splitting data
Suhu.maksimal <- data$`maximum temperature`
training <- data[1:235, 2]   
testing <- data[236:336, 2]
training.ts<-ts(training)
testing.ts<-ts(testing,start=236)
plot(training.ts)
plot(testing.ts)

#Ubah Format Data Date
weatherdate<-data.frame(x=seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by="day"))
data <- data[,-1]
data <- cbind(weatherdate,data)
colnames(data) <- c("Date","Maximum Temperature")
str(data)


#plot acf
acf(training)
#Berdasarkan plot ACF pada data harga penutupan emas, nilai korelasi setiap lag sebagaimana terlihat pada plot di atas 
#menurun menuju nilai nol secara perlahan (tails off slowly). 
#Artinya, data harga penutupan emas tidak stasioner

#plot pacf
pacf(training)

#uji adf untuk melihat stasioneritas terhdap rataan
adf.test(training.ts)
#p-value = 0.99
#Berdasarkan hasil augmented dickey-Fuller test (ADF test) didapatkan p-value > α, 
#maka tak tolak H0. 
#Artinya, tidak cukup bukti untuk menyatakan bahwa data stasioner pada taraf nyata 5%, 
#sehingga perlu dilakukan differencing.

#diff 1x
data.train.d1 = diff(training.ts, differences=1)
plot(data.train.d1)
plot(training.ts)
#Setelah dilakukan differencing satu kali d=1, 
#pola data harga penutupan emas sudah stasioner dilihat dari times series plot di atas.

adf.test(data.train.d1)
#Diperoleh p-value = 0.01 < α = 0.05, maka tolak H0. 
#Artinya, cukup bukti untuk mengatakan bahwa 
#data stasioner dalam rataan setelah dilakukan differencing sebanyak satu kali pada taraf nyata 5%.

#misal jika datanya belum stasioner hd rataan di diff 1x
#DIFF 2X
#data.train.d2 = diff(training.ts, differences=2)
#plot(data.train.d2)
#adf.test(data.train.d2)
#Cek Stasioneritas Setelah Differencing
#Stasioner dalam Rataan

#plot acf
ggAcf(data.train.d1,col="black",lwd=2)+labs( x="Lag",y = "ACF",
                                             title="Plot ACF Maximum Temperature",
                                             subtitle = "(Januari 2016 - Desember 2016)")+
  theme_bw()+
  theme(
    plot.title = element_text(size = 14L,
                              face = "bold",
                              hjust = 0.5),
    plot.subtitle = element_text(size = 11L,
                                 face = "plain",
                                 hjust = 0.5)
  )
#atau
acf(data.train.d1)
#terpotong di lag ke 0, 1, 2, 13

#plot pacf
ggPacf(data.train.d1,col="black",lwd=2)+labs( x="Lag",y = "PACF",
                                              title="Plot ACF Maximum Temperature",
                                              subtitle = "(Januari 2016 - Desember 2016)",col="white")+
  theme_bw()+
  theme(
    plot.title = element_text(size = 14L,
                              face = "bold",
                              hjust = 0.5),
    plot.subtitle = element_text(size = 11L,
                                 face = "plain",
                                 hjust = 0.5)
  )
#atau
pacf(data.train.d1)
#terpotong pada lag ke 0,1,2
#kandidat model 
# p= 0, 1, 2
# q= 0, 1, 2
#1. (0,1,0)
#2. (0,1,1)
#3. (0,1,2)
#4. (1,1,0)
#5. (1,1,1)
#6. (1,1,2)
#7. (2,1,0)
#8. (2,1,1)
#9. (2,1,2)
#Berdasarkan plot ACF dan PACF di atas,terlihat bahwa nilai korelasi antara data dengan lag seperti gambar di atas tidak turun secara perlahan, 
#dimana diperoleh cuts off pada lag ke-2. Berdasarkan hasil eksplorasi di atas, 
#eacf(data.train.d1)

#Signifikansi Model
model1 <- Arima(data.train.d1,order = c(0,1,1),method ="ML")
model2 <- Arima(data.train.d1,order = c(0,1,2),method ="ML")
model3 <- Arima(data.train.d1,order = c(1,1,0),method ="ML")
model4 <- Arima(data.train.d1,order = c(1,1,1),method ="ML")
model5 <- Arima(data.train.d1,order = c(1,1,2),method ="ML")
model6 <- Arima(data.train.d1,order = c(2,1,0),method ="ML")
model7 <- Arima(data.train.d1,order = c(2,1,1),method ="ML")
model8 <- Arima(data.train.d1,order = c(2,1,2),method ="ML")

model1
#aic = 1636.86
coeftest(model1)
# PARAMETER yang signifikan adalah ma(1)

model2
#aic = 1628.99
coeftest(model2)
#PARAMETER yang signifikan adalah ma(1) dan ma(2)

model3
#aic = 1766.16
coeftest(model3)
# PARAMETER yang signifikan adalah ar(1)

model4
#aic = 1633.83
coeftest(model4)
#PARAMETER yang signifikan adalah ar(1) dan ma(1)

model5
#aic = 1604.12
coeftest(model5)
#PARAMETER yang signifikan adalah ar(1) 

model6
#aic = 1724.34
coeftest(model6)
#PARAMETER yang signifikan adalah ar(1) dan ar(2)

model7
#aic = 1622.87
coeftest(model7)
#parameter ar (1) dan ar(2) dan ma (1) signifikan

model8
#aic = 1605.59
coeftest(model8)
#parameter ar (1) dan ma(1) dan ma (2) signifikan


Model <- c("ARIMA (0,1,1)","ARIMA (0,1,2)","ARIMA(1,1,0)","ARIMA(1,1,1)","ARIMA(1,1,2)","ARIMA(2,1,0)","ARIMA(2,1,1)","ARIMA(2,1,2)")
AIC <- c(model1$aic,model2$aic,model3$aic,model4$aic,model5$aic,model6$aic,model7$aic,model8$aic)
BIC <- c(model1$bic,model2$bic,model3$bic,model4$bic,model5$bic,model6$bic,model7$bic,model8$bic)
Akurasi <- data.frame(Model,AIC,BIC)
kableExtra::kable(Akurasi)
paste("Model yang terbaik adalah model",Akurasi$Model[which.min(Akurasi[,"AIC"])])

#Uji Diagnostik
#Diagnostik Model: Eksploratif
#Analisis sisaan dapat dilihat secara eksploratif menggunakan Q-Q plot, residuals plot, ACF dan PACF plot.

sisaan <- model5$residuals
par(mfrow=c(2,2))
qqnorm(sisaan)
qqline(sisaan, col = "red", lwd =1)
plot(c(1:length(sisaan)),sisaan)
acf(sisaan)
pacf(sisaan)

#Berdasarkan hasil eksplorasi menggunakan Q-Q plot, 
#terlihat bahwa sisaan berdistribusi mengikuti garis normal, 
#sehingga dapat dikatakan bahwa sisaan menyebar normal. 
#Kemudian, plot sisaan yang diperoleh menunjukkan bahwa sisaan memiliki pola acak dan tersebar di sekitar nilai nol. 
#Sedangkan pada plot ACF dan PACF, nilai awal amatan tidak melewati garis signifikan, 
#atau dapat dikatakan bahwa sisaan saling bebas

#Diagnostik Model: Uji Formal
#1. Sisaan Menyebar Normal
jarque.bera.test(sisaan)
shapiro.test(sisaan)
#Berdasarkan Jarque-Bera test, diperoleh p-value (1.565e-10) < α (0.05). 
#Selain itu, hasil Shapiro-Wilk test, diperoleh p-value (0.000156) < α (0.05) maka tolak H0. 
#Artinya, tidak cukup bukti untuk menyatakan bahwa sisaan menyebar normal pada taraf nyata 5%. 
#Namun, asumsi normalitas tidak wajib terpenuhi.

#2. Sisaan Saling Bebas
#Uji formal ini dilakukan dengan LJung-Box test.
Box.test(sisaan, type = "Ljung")
#Berdasarkan LJung-Box test, diperoleh p-value (0.9711) > α (0.05), maka tak tolak H0. Artinya, cukup bukti untuk menyatakan bahwa sisaan antara lag saling bebas atau dapat dikatakan tidak ada autokorelasi antara sisaan lag pada taraf nyata 5%.

#3. Nilai Tengah Sisaan Sama dengan Nol
t.test(sisaan, mu = 0, conf.level = 0.95)

#autoarima
model.auto <- auto.arima(training.ts,trace = T,d = 1)
summary(model.auto)

#Forecasting ARIMA
ramalan <- forecast::forecast(Arima(training.ts, order=c(2,1,1),method="ML",include.drift = TRUE), h=20) 

data.ramalan <- ramalan$mean
plot(ramalan,lwd=2)


model <- Arima(training.ts, order=c(2,1,1), method="ML", include.drift=TRUE)
ramalan <- forecast::forecast(model, h=20) 
data.ramalan <- ramalan$mean
actual.ts <- ts(data$`Maximum Temperature`, start = start(training.ts), frequency = 365)
plot(actual.ts, col="black", lwd=2, main="Perbandingan Peramalan dan Aktual", ylim=c(min(actual.ts, na.rm = TRUE), max(actual.ts, na.rm = TRUE)))
lines(data.ramalan, col="red", lwd=2)
legend("topright", legend=c("Aktual", "Ramalan"), col=c("black", "red"), lty=1)

perbandingan.temp<-matrix(data=c(testing.ts[1:5], data.ramalan[1:5]), nrow = 5, ncol = 2)
colnames(perbandingan.temp)<-c("Aktual","Hasil Forecast")
head(perbandingan.temp)

accuracy(testing.ts,data.ramalan)













