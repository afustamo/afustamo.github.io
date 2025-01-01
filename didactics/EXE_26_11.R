load("Agrimonia_Dataset_v_3_0_0.Rdata")
# vediamo le stazioni
unique(AgrImOnIA_Dataset_v_3_0_0$IDStations)
# selezioniamo la stazione 1264
sub <- subset(AgrImOnIA_Dataset_v_3_0_0, IDStations == "1264")
# nomi delle variabili
names(sub)
# classi delle variabili
sapply(sub, class)

# Analisi esplorativa 
summary(sub)

y <- sub$AQ_pm10

plot(y,type="l")

library(ggplot2)
ggplot(sub)+
  geom_line(aes(y=AQ_pm10,x=Time))

# matrice di correlazione tra le variabili
# l'autocorrelazione dei dati
acf(y,na.action = na.pass) #autocorrelazione
pacf(y,na.action = na.pass) #autocorr parz
hist(y) #sia sulla variabile risposta, che sulle covariate
# test di normalità, di autocorrelazione, stazionarietà
# ed eventuali ....

# Regressione lineare con metodo OLS
lm0 <- lm(AQ_pm10 ~ WE_temp_2m + WE_wind_speed_10m_mean,
          data = sub)

lm1 <- lm(AQ_pm10 ~ WE_temp_2m + WE_wind_speed_10m_mean+
            LI_bovine,
          data = sub)

summary(lm0)
summary(lm1)

# diagnostica dei residui
e = y[!is.na(y)] - lm0$fitted.values #Y - E[Y]
# media zero (+ cons)
mean(e)

# omoschedastici
var(e)
#varianza per stagione
Time<-sub$Time[!is.na(sub$AQ_pm10)]
e_df <- data.frame(e=e,Time=Time)
e_df <- merge(sub[,4],e_df,all.x=T)

aggregate(e_df$e,by=list(months(e_df$Time)),"var", na.rm=T)

# autocorrelazione (o correlazione seriale)

mean(e_df$e[-nrow(e_df)] * e_df$e[-1], na.rm=T)
e_df$e_std <- (e_df$e - mean(e_df$e,na.rm=T))/sd(e_df$e,na.rm=T)
mean(e_df$e_std[-nrow(e_df)] * e_df$e_std[-1], na.rm=T)
acf(e_df$e,na.action = na.pass)
#
z <- rnorm(1000,5000,300)
plot(z,type="l")

#STANDARDIZZIAMO Z
x <- (z - mean(z))/sd(z)
plot(x,type="l")

cov(z[-1000],z[-1])
cov(x[-1000],x[-1])

# Processi autoregressivi 
summary(y)
ar1 <- lm(y[-1] ~ y[-2192]-1)
phi <- ar1$coefficients
y_ar1 <- phi*y[-2192]

res <- y_ar1 - y[-1]
summary(res)

summary(lm0)
summary(ar1)


# lm( temp ~ nuvole)

lm(e_df$e[-1] ~ e_df$e[-2192])

X <- cbind(sub$WE_temp_2m,sub$WE_wind_speed_10m_mean)

ar1 <- arima(y,order = c(1,0,0),xreg = X)

lm0 <- lm(AQ_pm10 ~ WE_temp_2m + WE_wind_speed_10m_mean,
          data = sub)











