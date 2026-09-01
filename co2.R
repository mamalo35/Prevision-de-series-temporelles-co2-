
#Chargement des librairies

install.packages("forecast")
install.packages("tseries")
library(forecast)
library(tseries)

rmse <- function(y, yhat) {
  sqrt(mean((y - yhat)^2))
}

mape <- function(y, yhat) {
  mean(abs((y - yhat) / y)) * 100
}

data(co2)
co2
# On garde la dernière année (1997) pour test
train <- window(co2, end=c(1996,12))
test  <- window(co2, start=c(1997,1))


# Visualisation
plot(train, main="Train vs Test", col="blue", lwd=2)
lines(test, col="red", lwd=2)
legend("topleft", legend=c("Train","Test"),
       col=c("blue","red"), lwd=2)







# variable temps
t <- 1:length(train)

plot(train,
     main = "CO2",
     xlab = "Temps",
     ylab = "ppm",
     col = "darkgreen")

# Modèle 1 : régression linéaire simple

modele1 <- lm(train ~ t + I(t^2))

# Résumé du modèle
cat("\n===== Modèle 1 : CO2 ~ temps =====\n")
print(summary(modele1))

# Ajouter la droite de régression
lines(ts(fitted(modele1), start = start(train), frequency = frequency(train)),
      col = "blue", lwd = 2)


# Modèle 2
# avec saisonnalité
mois <- factor(cycle(train))

modele2 <- lm(train ~ t + mois)

# Résumé du modèle
cat("\n===== Modèle 2 : CO2 ~ temps + mois =====\n")
print(summary(modele2))


# Visualisation du modèle 2

plot(train,
     main = "Modèle avec tendance + saisonnalité",
     ylab = "ppm",
     xlab = "Temps",
     col = "darkgreen")


lines(ts(fitted(modele2), start = start(train), frequency = frequency(train)),
      col = "blue", lwd = 2)



# Modèle 3
# Période = 12 mois
modele3 <- lm(train ~ t +
                sin(2 * pi * t/12) + cos(2 * pi * t/12))



# Résumé
cat("\n===== Modèle 3 : CO2 ~ temps + sin/cos =====\n")
print(summary(modele3))

plot(train,
     main = "Modèle 3 : tendance + saisonnalité sinusoïdale",
     ylab = "ppm",
     xlab = "Temps",
     col = "darkgreen")

lines(ts(fitted(modele3),
         start = start(train),
         frequency = frequency(train)),
      col = "purple", lwd = 2)

legend("topleft",
       legend = c("Données observées", "Modèle"),
       col = c("darkgreen", "purple"),
       lwd = c(1, 2),
       bty = "n")

modele4 <- lm(train ~ t +
                sin(2*pi*t/12) + cos(2*pi*t/12) +
                sin(4*pi*t/12) + cos(4*pi*t/12))

# Résumé
cat("\n===== Modèle 4 : CO2 ~ temps + sin/cos =====\n")
print(summary(modele4))

plot(train,
     main = "Modèle 4 : tendance + saisonnalité harmonique",
     ylab = "ppm",
     xlab = "Temps",
     col = "darkgreen")

lines(ts(fitted(modele4),
         start = start(train),
         frequency = frequency(train)),
      col = "purple", lwd = 2)

legend("topleft",
       legend = c("Données observées", "Modèle"),
       col = c("darkgreen", "purple"),
       lwd = c(1, 2),
       bty = "n")



X <- data.frame(t = t)

for(i in 1:6){
  X[[paste0("sin", i)]] <- sin(2*pi*i*t/12)
  X[[paste0("cos", i)]] <- cos(2*pi*i*t/12)
}

modele5 <- lm(train ~ ., data = X)

# Résumé
cat("\n===== Modèle 5 : CO2 ~ temps + sin/cos =====\n")
print(summary(modele5))

plot(train,
     main = "Modèle 5 : tendance + saisonnalité harmonique complète",
     ylab = "ppm",
     xlab = "Temps",
     col = "darkgreen")

lines(ts(fitted(modele5),
         start = start(train),
         frequency = frequency(train)),
      col = "purple", lwd = 2)

legend("topleft",
       legend = c("Données observées", "Modèle"),
       col = c("darkgreen", "purple"),
       lwd = c(1, 2),
       bty = "n")

modele6 <- lm(train ~ t + I(t^2) + sin(2 * pi * t/12) + cos(2 * pi * t/12))

# Résumé
cat("\n===== Modèle 6 : CO2 ~ quadra + sin/cos =====\n")
print(summary(modele6))

plot(train,
     main = "Modèle 4 : tendance quadratique + saisonnalité sinusoïdale",
     ylab = "ppm",
     xlab = "Temps",
     col = "darkgreen")

lines(ts(fitted(modele6),
         start = start(train),
         frequency = frequency(train)),
      col = "purple", lwd = 2)

legend("topleft",
       legend = c("Données observées", "Modèle"),
       col = c("darkgreen", "purple"),
       lwd = c(1, 2),
       bty = "n")


modele7 <- lm(train ~ t + I(t^2) +
                sin(2*pi*t/12) + cos(2*pi*t/12) +
                sin(4*pi*t/12) + cos(4*pi*t/12))

X <- model.matrix(modele7)[, -1]


modele7_ar1 <- arima(train,
                     order = c(1,0,0),
                     xreg = X)


# Résumé
cat("\n===== Modèle 7 : CO2 ~ quadra + sin/cos =====\n")
print(summary(modele7_ar1))

plot(train,
     main = "Modèle 5 : tendance quadratique+ saisonnalité harmonique",
     ylab = "ppm",
     xlab = "Temps",
     col = "darkgreen")

lines(ts(fitted(modele7_ar1),
         start = start(train),
         frequency = frequency(train)),
      col = "purple", lwd = 2)

legend("topleft",
       legend = c("Données observées", "Modèle"),
       col = c("darkgreen", "purple"),
       lwd = c(1, 2),
       bty = "n")


plot(train,
     main = "Modèle 5 : tendance quadratique+ saisonnalité harmonique",
     ylab = "ppm",
     xlab = "Temps",
     col = "darkgreen")

lines(ts(fitted(modele7),
         start = start(train),
         frequency = frequency(train)),
      col = "purple", lwd = 2)

legend("topleft",
       legend = c("Données observées", "Modèle"),
       col = c("darkgreen", "purple"),
       lwd = c(1, 2),
       bty = "n")




cat("\n===== Comparaison AIC =====\n")
print(AIC(modele1, modele2, modele3, modele4, modele5, modele6, modele7, modele7_ar1))

# temps pour le test
t_test <- (length(train)+1):(length(train)+length(test))

# prédictions modèle 4
pred4 <- predict(modele4,
                 newdata = data.frame(
                   t = t_test
                 ))

plot(test, col="red", lwd=2, main="Prédiction vs Réalité (1997)", ylim = c(359,369))
lines(ts(pred4, start=start(test), frequency=12),
      col="blue", lwd=2)

legend("topleft",
       legend=c("Réel","Prédit (modèle 4)"),
       col=c("red","blue"),
       lwd=2)


#pred model 6
pred6 <- predict(modele6,
                 newdata = data.frame(
                   t = t_test
                 ))

plot(test, col="red", lwd=2, main="Prédiction vs Réalité (1997)", ylim = c(359,369))
lines(ts(pred6, start=start(test), frequency=12),
      col="blue", lwd=2)

legend("topright",
       legend=c("Réel","Prédit (modèle 6)"),
       col=c("red","blue"),
       lwd=2)

#pred model 7
pred7 <- predict(modele7,
                 newdata = data.frame(
                   t = t_test
                 ))

plot(test, col="red", lwd=2, main="Prédiction vs Réalité (1997)", ylim = c(359,369))
lines(ts(pred7, start=start(test), frequency=12),
      col="blue", lwd=2)

legend("topright",
       legend=c("Réel","Prédiction "),
       col=c("red","blue"),
       lwd=2)


rmse7 <- sqrt(mean((test - pred7)^2))

# Erreur train
rmse_train7 <- sqrt(mean(residuals(modele7)^2))

# Erreur test déjà calculée
rmse_train7
rmse7

rmse4 <- sqrt(mean((test - pred4)^2))
rmse4

rmse6 <- sqrt(mean((test - pred6)^2))
rmse6 

mape4 <- mean(abs((test - pred4) / test)) * 100
mape4

# prédictions modèle 2
mois_test <- factor(cycle(test))
pred2 <- predict(modele2,
                 newdata = data.frame(
                   t = t_test,
                   mois = mois_test
                 ))

plot(test, col="red", lwd=2, main="Prédiction vs Réalité (1997)", ylim = c(359,367))
lines(ts(pred2, start=start(test), frequency=12),
      col="blue", lwd=2)

legend("topleft",
       legend=c("Réel","Prédit (modèle 2)"),
       col=c("red","blue"),
       lwd=2)

rmse2 <- sqrt(mean((test - pred2)^2))
rmse2

mape2 <- mean(abs((test - pred2) / test)) * 100
mape2


res7 <- residuals(modele7)

# Résidus
res6 <- residuals(modele6)

# Plot des résidus
plot(res6, type="l", main="Résidus du modèle 6")

# Histogramme
hist(res6, main="Histogramme des résidus")

# ACF des résidus
acf(res6, main="ACF des résidus")

# QQ-plot (normalité)
qqnorm(res6)
qqline(res6, col="red")

library(lmtest)

dwtest(modele7)





f3 <- fitted(modele3)
f4 <- fitted(modele4)
f5 <- fitted(modele5)

f6 <- fitted(modele6)
f7 <- fitted(modele7)
f7b <- fitted(modele7_ar1)

# Modèle 3
rmse3 <- rmse(train, f3)
mape3 <- mape(train, f3)

# Modèle 4
rmse4 <- rmse(train, f4)
mape4 <- mape(train, f4)

# Modèle 5
rmse5 <- rmse(train, f5)
mape5 <- mape(train, f5)

# Modèle 6
rmse6 <- rmse(train, f6)
mape6 <- mape(train, f6)

# Modèle 7
rmse7 <- rmse(train, f7)
mape7 <- mape(train, f7)

# Modèle 7
rmse7b <- rmse(train, f7b)
mape7b <- mape(train, f7b)

resultats <- data.frame(
  Modele = c("Modele 3", "Modele 4", "Modele 5", "Modele 6", "Modele 7","Modèle7 AR 1"),
  RMSE = c(rmse3, rmse4, rmse5, rmse6, rmse7, rmse7b),
  MAPE = c(mape3, mape4, mape5, mape6, mape7, mape7b)
)

print(resultats)



plot(residuals(modele7_ar1))
acf(residuals(modele7_ar1))
hist(residuals(modele7_ar1))
qqnorm(residuals(modele7_ar1))
qqline(residuals(modele7_ar1))
