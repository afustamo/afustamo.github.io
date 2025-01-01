load("Agrimonia_Dataset_v_3_0_0.Rdata")

ger <- "DEHB002"
ita <- "583"

sub <- AgrImOnIA_Dataset_v_3_0_0[AgrImOnIA_Dataset_v_3_0_0$IDStations==ita,]

plot(sub$AQ_pm25,type="l")

lm(AQ_pm25~. -Latitude - Longitude -IDStations -Time -LA_land_use -LA_soil_use -Altitude, data=sub)
variables <- names(AgrImOnIA_Dataset_v_3_0_0)
f <- as.formula(paste("AQ_pm25",paste(variables[13:14],collapse = "+"),sep = "~"))

summary(lm(f, data=sub))

X <- sub[,13]
X <- cbind(rep(1,2192),X)
names(X)[1]<-"Intercept"
y <- sub$AQ_pm25
X[,2]<-scale(X[,2])
NA_idx <- which(is.na(y))
arima(y,order = c(1,0,0),xreg = X,
      method = "CSS")
