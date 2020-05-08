
library(forecast)
tst<-data.frame(list(num_data=100*runif(100),
                     period = now()+days(seq(1:100))))
tst_xts<-xts(x = tst$num_data, order.by = tst$period)
plot(tst_xts)
y_tst_xts<-msts(tst_xts, seasonal.periods=c(7,30)) %>% mstl()
fcast_tst_xts<-forecast(y_tst_xts, method = 'ets', h = 30)
plot(fcast_tst_xts)

pred_tst_xts <- as.data.frame(fcast_tst_xts$mean) %>%
  mutate(period = now()+100+row_number())

pred_tst_xts_x <- xts(x = pred_tst_xts$x, order.by = pred_tst_xts$period)

ema_tst_xts <- EMA(rbind(tst_xts,pred_tst_xts_x), n = 7)
index(ema_tst_xts)<-as.Date(index(ema_tst_xts))

fin<-rbind(tst_xts,pred_tst_xts_x)
index(fin)<-as.Date(index(fin))
colnames(fin)<-'fin'
fin$ema<-ema_tst_xts$EMA
plot(fin)
