library(readxl)
library(TSA)
library(tseries)

data<-read_excel('Stock Price PT Aneka Tambang.xlsx')
data$Periode <- as.Date(data$Periode)

# Plotting Data
ggplot(data,aes(Periode,`Stock Price`))+labs(y="Stock Price", title="Stock Price PT Aneka Tambang")+geom_line()+theme_minimal()

# Tanggal yang akan digunakan sebagai batas pemisah
tanggal_batas <- as.Date("2020-09-1")

# Bagian sebelum tanggal batas
data_sebelum_intervensi <- subset(data, Periode <= tanggal_batas)

# Bagian setelah tanggal batas
data_setelah_intervensi <- subset(data, Periode > tanggal_batas)

# Akan dibentuk suatu model (ar,i,ma)
# Tes stationeritas untuk menentukan i
adf.test(data_sebelum_intervensi$`Stock Price`) # Belum signifikan
adf.test(diff(data_sebelum_intervensi$`Stock Price`)) # Telah signifikan di i = 1

# Model_Preintervensi
acf(data_sebelum_intervensi$`Stock Price`)
pacf(data_sebelum_intervensi$`Stock Price`)
eacf(data_sebelum_intervensi$`Stock Price`)

# Didapat model (0,1,2),(1,1,1),(1,1,2)
model1 <- Arima(data_sebelum_intervensi$`Stock Price`,order = c(0,1,2))
model2 <- Arima(data_sebelum_intervensi$`Stock Price`,order = c(1,1,1))
model3 <- Arima(data_sebelum_intervensi$`Stock Price`,order = c(1,1,2))
summary(model1)
summary(model2)
summary(model3)

# Signifikansi
coeftest(model1)$p.value
coeftest(model2)$p.value
coeftest(model3)$p.value

# Perbandingan AIC
model1$aic
model2$aic
model3$aic

# Perbandingan BIC
model1$bic
model2$bic
model3$bic

# Uji normalitas
shapiro.test(model1$residuals)$p.value
shapiro.test(model2$residuals)$p.value
shapiro.test(model3$residuals)$p.value

# Ljung Box test
checkresiduals(model1)$p.value
checkresiduals(model2)$p.value
checkresiduals(model3)$p.value

df <- data.frame(col_1_res = c(model1$aic, model2$aic, model3$aic),
                 col_2_res = c(model1$bic, model2$bic, model3$bic),
                 col_3_res = c(shapiro.test(model1$residuals)$p.value, shapiro.test(model2$residuals)$p.value, shapiro.test(model3$residuals)$p.value),
                 col_4_res = c(checkresiduals(model1)$p.value,checkresiduals(model2)$p.value,checkresiduals(model3)$p.value))

colnames(df) <- c("AIC", "BIC", "Normality test (p.value)","Ljung Box Test (p.value)")
rownames(df) <- c("ARIMA(0,1,2)",
                  "ARIMA(1,1,1)",
                  "ARIMA(1,1,2)")
write.csv(df,"Uji diagnostik.csv")

# Terpilih model 3 yaitu (1,1,2)
# Forecasting dengan model terbaik
h_fut <- 20

Forecasted_data=forecast(model3, h = h_fut)
Forecasted_data

plot(Forecasted_data,main="ARIMA Forecast")
Acc <- accuracy(Forecasted_data)
write.csv(Acc,"Accuracy Forecast.csv")
write.csv(Forecasted_data,"Hasil Forecast.csv")

