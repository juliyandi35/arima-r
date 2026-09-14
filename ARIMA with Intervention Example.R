suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(forecast))
suppressPackageStartupMessages(library(astsa))
suppressPackageStartupMessages(library(lmtest))
suppressPackageStartupMessages(library(fUnitRoots))
suppressPackageStartupMessages(library(FitARMA))
suppressPackageStartupMessages(library(strucchange))
suppressPackageStartupMessages(library(reshape))
suppressPackageStartupMessages(library(Rmisc))
suppressPackageStartupMessages(library(fBasics))

url <- "https://www.openintro.org/stat/data/arbuthnot.csv"
abhutondot <- read.csv(url, header=TRUE)
nrow(abhutondot)

head(abhutondot)

abhutondot_rs <- melt(abhutondot, id = c("year"))
head(abhutondot_rs)

tail(abhutondot_rs)

ggplot(data = abhutondot_rs, aes(x = year)) + geom_line(aes(y = value, colour = variable)) +
  scale_colour_manual(values = c("blue", "red"))

t.test(value ~ variable, data = abhutondot_rs)

basicStats(abhutondot[-1])

p1 <- ggplot(data = abhutondot_rs, aes(x = variable, y = value)) + geom_boxplot()
p2 <- ggplot(data = abhutondot, aes(boys)) + geom_density()
p3 <- ggplot(data = abhutondot, aes(girls)) + geom_density()
multiplot(p1, p2, p3, cols = 3)

excess_frac <- (abhutondot$boys - abhutondot$girls)/abhutondot$girls
excess_ts <- ts(excess_frac, frequency = 1, start = abhutondot$year[1])
autoplot(excess_ts)

basicStats(excess_frac)

urdftest_lag = floor(12*(length(excess_ts)/100)^0.25)
urdfTest(excess_ts, type = "nc", lags = urdftest_lag, doplot = FALSE)

par(mfrow=c(1,2))
acf(excess_ts)
pacf(excess_ts)

summary(lm(excess_ts ~ 1))

(break_point <- breakpoints(excess_ts ~ 1))

plot(break_point)

summary(break_point)

plot(excess_ts)
lines(fitted(break_point, breaks = 1), col = 4)
lines(confint(break_point, breaks = 1))

fitted(break_point)[1]

fitted(break_point)[length(excess_ts)]

break_date <- breakdates(break_point)
win_1 <- window(excess_ts, end = break_date)
win_2 <- window(excess_ts, start = break_date + 1)
t.test(win_1, win_2)

# 1. non seasonal (1,1,1), as determined by auto.arima() within forecast package
# 2. seasonal (1,0,0)(0,0,1)[10]
# 3. seasonal (1,0,0)(1,0,0)[10]
# 4. seasonal (0,0,0)(0,0,1)[10] with level shift regressor as intervention variable
# 5. seasonal (1,0,0)(0,0,1)[10] with level shift regressor as intervention variable
# 6. non seasonal (1,0,0) with level shift regressor as intervention variable

model_1 <- auto.arima(excess_ts, stepwise = FALSE, trace = TRUE)

summary(model_1)

coeftest(model_1)

model_2 <- Arima(excess_ts, order = c(1,0,0),
                 seasonal = list(order = c(0,0,1), period = 10),
                 include.mean = TRUE)
summary(model_2)

coeftest(model_2)

model_3 <- Arima(excess_ts, order = c(1,0,0),
                 seasonal = list(order = c(1,0,0), period = 10),
                 include.mean = TRUE)
summary(model_3)

coeftest(model_3)

level <- c(rep(0, break_point$breakpoints),
           rep(1, length(excess_ts) - break_point$breakpoints))

model_4 <- Arima(excess_ts, order = c(0,0,0),
                 seasonal = list(order = c(0,0,1), period = 10),
                 xreg = level, include.mean = TRUE)
summary(model_4)

coeftest(model_4)

model_5 <- Arima(excess_ts, order = c(1,0,0),
                 seasonal = list(order = c(0,0,1), period=10),
                 xreg = level, include.mean = TRUE)
summary(model_5)

coeftest(model_5)

model_6 <- Arima(excess_ts, order = c(1,0,0), xreg = level, include.mean = TRUE)
summary(model_6)

coeftest(model_6)

checkresiduals(model_1)

Box.test(residuals(model_1), lag = 20,type = "Ljung")

sarima(excess_ts, p = 1, d = 1, q = 1)

checkresiduals(model_2)

Box.test(residuals(model_2), lag = 20,type = "Ljung")

sarima(excess_ts, p = 1, d = 0, q = 0, P = 0, D = 0, Q = 1, S = 10)

checkresiduals(model_3)

Box.test(residuals(model_3),lag = 20,type = "Ljung")
sarima(excess_ts, p = 1, d = 0, q = 0, P = 1, D = 0, Q = 0, S = 10)

checkresiduals(model_4)

Box.test(residuals(model_4), lag = 20,type = "Ljung")

sarima(excess_ts, p = 0, d = 0, q = 0, P = 0, D = 0, Q = 1, S = 10, xreg = level)

checkresiduals(model_6)

Box.test(residuals(model_6), lag = 20,type = "Ljung")

sarima(excess_ts, p = 1, d = 0, q = 0, xreg = level)

df <- data.frame(col_1_res = c(model_1$aic, model_2$aic, model_3$aic, model_4$aic, model_6$aic),
                 col_2_res = c(model_1$aicc, model_2$aicc, model_3$aicc, model_4$aicc, model_6$aicc),
                 col_3_res = c(model_1$bic, model_2$bic, model_3$bic, model_4$bic, model_6$bic))

colnames(df) <- c("AIC", "AICc", "BIC")
rownames(df) <- c("ARIMA(1,1,1)",
                  "ARIMA(1,0,0)(0,0,1)[10]",
                  "ARIMA(1,0,0)(1,0,0)[10]",
                  "ARIMA(0,0,0)(0,0,1)[10] with level shift",
                  "ARIMA(1,0,0) with level shift")
df

h_fut <- 20
plot(forecast(model_4, h = h_fut, xreg = rep(1, h_fut)))

abhutondot.ts <- ts(abhutondot$boys + abhutondot$girls, frequency = 1 ,
                    start = abhutondot$year[1])
autoplot(abhutondot.ts)

summary(lm(abhutondot.ts ~ 1))

(break_point <- breakpoints(abhutondot.ts ~ 1))
par(mfrow=c(1,1))
plot(break_point)

summary(break_point)
plot(abhutondot.ts)
fitted.ts <- fitted(break_point, breaks = 3)
lines(fitted.ts, col = 4)
lines(confint(break_point, breaks = 3))

unique(as.integer(fitted.ts))

breakdates(break_point, breaks = 3)

fitted.ts <- fitted(break_point, breaks = 3)
autoplot(fitted.ts)

abhutondot_xreg <- Arima(abhutondot.ts, order = c(0,1,1), xreg = fitted.ts, include.mean = TRUE)
summary(abhutondot_xreg)

checkresiduals(abhutondot_xreg)

Box.test(residuals(abhutondot_xreg), lag=20,type = "Ljung")

sarima(abhutondot.ts, p=0, d=1, q=1, xreg = fitted.ts)

