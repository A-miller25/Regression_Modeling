MentalHealth <- read.csv("Mental Health & Music.csv")
data("MentalHealth")
View(MentalHealth)
str(MentalHealth)

lm.fit=lm(Depression~.,data=MentalHealth)
summary(lm.fit)

par(mfrow=c(2,2))
plot(lm.fit)
par(mfrow=c(1,1))
#Interaction 1 
lmi.fit=lm(Depression~Anxiety*Insomnia,data=MentalHealth)
summary(lmi.fit)
#Interaction 2 
lmi2.fit=lm(Depression~Frequency..Rap.*Frequency..EDM., data=MentalHealth)
summary(lmi2.fit)
#Revised Model
lmrevised.fit=lm(Depression~Frequency..Country.+Frequency..EDM.+Frequency..Rap.+Anxiety+Insomnia,data=MentalHealth)
summary(lmrevised.fit)
#Confidence Interval

confint(lmrevised.fit)
#Intercept:0.03 to 1.09
#Frequency.Country.Rarely: -0.58 to 0.26
#Frequency.Country.Sometimes: -1.1 to -0.03
#Frequency.Country.Very Frequently: -0.72 to 0.77
#Frequency.EDM.Rarely: 0.04 to 0.96
#Frequency.EDM.Sometimes: -0.19 to 0.81
#Frequency.EDM.Very Frequently: -0.38 to 0.81
#Frequency.Rap.Rarely:0.19 to 1.16
#Frequency.Rap.Sometimes: 0.30 to 1.31
#Frequency.Rap.Very Frequently: 0.15 to 1.29
#Anxiety: 0.40 to 0.54
#Insomnia: 0.17 to 0.30
predict(lmrevised.fit)
par(mfrow = c(2, 2))  
plot(lmrevised.fit)



