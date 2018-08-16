library(usdm)
library(car)
ESG<- read.csv(file = "~/R/ESG_data.csv")
View(ESG)
str(ESG)
pairs(ESG[,c(9:13)], lower.panel = panel.smooth)
ESG$RETURN_COM_EQY <- as.numeric(as.factor(ESG$RETURN_COM_EQY))
ESG$RETURN_ON_ASSET <- as.numeric(as.factor(ESG$RETURN_ON_ASSET))

set.seed(1234)
WACC.lm<- lm(WACC ~ SUSTAINALYTICS_ENVIRONMENT_PCT + 
               SUSTAINALYTICS_SOCIAL_PERCENTILE +
               SUSTAINALYTICS_GOVERNANCE_PCT +
               TOTAL.ASSET, data=ESG)
summary(WACC.lm)


set.seed(1234)
TQR.lm<- lm(TOBIN_Q_RATIO ~ SUSTAINALYTICS_ENVIRONMENT_PCT + 
                SUSTAINALYTICS_SOCIAL_PERCENTILE +
                SUSTAINALYTICS_GOVERNANCE_PCT +
                TOTAL.ASSET, data=ESG)
summary(TQR.lm)



set.seed(1234)
ROE.lm<- lm(RETURN_COM_EQY ~ SUSTAINALYTICS_ENVIRONMENT_PCT + 
              SUSTAINALYTICS_SOCIAL_PERCENTILE +
              SUSTAINALYTICS_GOVERNANCE_PCT +
              TOTAL.ASSET, data=ESG)
summary(ROE.lm)

set.seed(1234)
ROA.lm<- lm(RETURN_ON_ASSET ~ SUSTAINALYTICS_ENVIRONMENT_PCT + 
              SUSTAINALYTICS_SOCIAL_PERCENTILE +
              SUSTAINALYTICS_GOVERNANCE_PCT +
              TOTAL.ASSET, data=ESG)
summary(ROA.lm)

lm1<- lm(TOTAL_ASSET ~ SUSTAINALYTICS_ENVIRONMENT_PCT + 
              SUSTAINALYTICS_SOCIAL_PERCENTILE +
              SUSTAINALYTICS_GOVERNANCE_PCT +
              ROA + ROE + WACC + TOBIN_Q_RATIO, data=ESG)
vif(lm1, digits = 3)

par(mfrow=c(2,2))
plot(crime.lm1)
par(mfrow=c(1,1))