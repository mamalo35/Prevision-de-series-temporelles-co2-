# Prévision de la concentration de CO2 (série co2, Mauna Loa) : régression et SARIMA
# Packages nécessaires : install.packages(c("forecast", "tseries"))

# Chargement des librairies
library(forecast)
library(tseries)

data(co2)

# On garde la dernière année (1997) pour test
train <- window(co2, end=c(1996,12))
test  <- window(co2, start=c(1997,1))

# Visualisation
plot(train, main="Train vs Test", col="blue", lwd=2)
lines(test, col="red", lwd=2)
legend("topleft", legend=c("Train","Test"),
       col=c("blue","red"), lwd=2)

plot(train,
     main = "Série temporelle CO2",
     ylab = "ppm",
     xlab = "Temps",
     col = "darkgreen")

# variable temps
t <- 1:length(train)

# Modèle 1 : régression linéaire simple

modele1 <- lm(train ~ t)

# Résumé du modèle
cat("\n===== Modèle 1 : CO2 ~ temps =====\n")
print(summary(modele1))

# Ajouter la droite de régression
abline(modele1, col = "red", lwd = 2)

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
                sin(2*pi*t/12) + cos(2*pi*t/12))

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

modele4 <- lm(train ~ t +
                sin(2*pi*t/12) + cos(2*pi*t/12) +
                sin(4*pi*t/12) + cos(4*pi*t/12))

# Résumé
cat("\n===== Modèle 4 : CO2 ~ temps + 2 harmoniques =====\n")
print(summary(modele4))

plot(train,
     main = "Modèle 4 : tendance + saisonnalité sinusoïdale",
     ylab = "ppm",
     xlab = "Temps",
     col = "darkgreen")

lines(ts(fitted(modele4),
         start = start(train),
         frequency = frequency(train)),
      col = "purple", lwd = 2)

cat("\n===== Comparaison AIC =====\n")
print(AIC(modele1, modele2, modele3, modele4))

# temps pour le test
t_test <- (length(train) + 1):(length(train) + length(test))

# prédictions modèle 4
pred4 <- predict(modele4,
                 newdata = data.frame(
                   t = t_test
                 ))

plot(test, col="red", lwd=2, main="Prédiction vs Réalité (1997)", ylim = c(359,367))
lines(ts(pred4, start=start(test), frequency=12),
      col="blue", lwd=2)

legend("topleft",
       legend=c("Réel","Prédit (modèle 4)"),
       col=c("red","blue"),
       lwd=2)

rmse4 <- sqrt(mean((test - pred4)^2))
rmse4

mape4 <- mean(abs((test - pred4) / test)) * 100
mape4

# prédictions modèle 2
mois_test <- factor(cycle(test), levels = levels(mois))

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

# MODÈLE SARIMA
# (même découpage train / test que pour les régressions)

# 2. Transformation logarithmique

# Le logarithme est optionnel pour CO2.

log_train <- log(train)
log_test  <- log(test)

plot(log_train,
     main = "Logarithme de la série CO2 - données d'entraînement",
     ylab = "log(CO2)",
     xlab = "Temps",
     col = "darkgreen",
     lwd = 2)

# 3. Analyse ACF / PACF de la série brute

Acf(log_train, lag.max = 50,
    main = "ACF de log(CO2)")

Pacf(log_train, lag.max = 50,
     main = "PACF de log(CO2)")

# ACF à décroissance lente : la série n'est pas stationnaire (tendance + saisonnalité).

# 4. Différenciation ordinaire

d1_log_train <- diff(log_train, differences = 1)

plot(d1_log_train,
     main = "Différenciation ordinaire de log(CO2)",
     ylab = "Différence première",
     xlab = "Temps",
     col = "blue",
     lwd = 2)

Acf(d1_log_train, lag.max = 50,
    main = "ACF après différenciation ordinaire")

Pacf(d1_log_train, lag.max = 50,
     main = "PACF après différenciation ordinaire")

# La différenciation ordinaire enlève principalement la tendance.
# Mais il reste encore souvent une saisonnalité annuelle.

# 5. Différenciation saisonnière

d2_log_train <- diff(d1_log_train, lag = 12)

plot(d2_log_train,
     main = "Différenciation ordinaire + saisonnière",
     ylab = "Série différenciée",
     xlab = "Temps",
     col = "purple",
     lwd = 2)

Acf(d2_log_train, lag.max = 50,
    main = "ACF après double différenciation")

Pacf(d2_log_train, lag.max = 50,
     main = "PACF après double différenciation")

# Ici on a appliqué :
# - une différenciation ordinaire : d = 1 ;
# - une différenciation saisonnière de période 12 : D = 1.

# Donc on cherchera un modèle de type :
# SARIMA(p,1,q)(P,1,Q)[12]

# 6. Test ADF

cat("\n===== Test ADF sur log(CO2) brut =====\n")
print(adf.test(log_train))

cat("\n===== Test ADF après différenciation =====\n")
print(adf.test(d2_log_train))

# Attention :
# Le test ADF peut être trompeur sur des séries fortement saisonnières.
# On l'utilise comme indication, pas comme preuve absolue.

# 7. Ajustement de plusieurs modèles SARIMA candidats

mod_sarima_1 <- Arima(log_train,
                      order = c(0, 1, 1),
                      seasonal = list(order = c(0, 1, 1),
                                      period = 12))

mod_sarima_2 <- Arima(log_train,
                      order = c(1, 1, 1),
                      seasonal = list(order = c(0, 1, 1),
                                      period = 12))

mod_sarima_3 <- Arima(log_train,
                      order = c(0, 1, 1),
                      seasonal = list(order = c(1, 1, 1),
                                      period = 12))

mod_sarima_4 <- Arima(log_train,
                      order = c(1, 1, 0),
                      seasonal = list(order = c(0, 1, 1),
                                      period = 12))

mod_sarima_auto <- auto.arima(log_train,
                              seasonal = TRUE,
                              stepwise = FALSE,
                              approximation = FALSE)

# 8. Comparaison AIC

cat("\n===== Comparaison AIC des modèles SARIMA =====\n")

print(AIC(mod_sarima_1,
          mod_sarima_2,
          mod_sarima_3,
          mod_sarima_4,
          mod_sarima_auto))

# Modèle retenu : le modèle automatique ARIMA(0,1,1)(2,1,2)[12].
# Son AIC est quasiment égal au minimum du tableau (SARIMA(1,1,1)(0,1,1)[12]),
# ses résidus sont proches d'un bruit blanc et sa prévision de 1997 est très précise.

meilleur_modele <- mod_sarima_auto

cat("\n===== Résumé du meilleur modèle choisi =====\n")
print(summary(meilleur_modele))

# 9. Diagnostic des résidus

checkresiduals(meilleur_modele)

# Si les résidus ressemblent à un bruit blanc,
# alors le modèle a bien capturé la structure temporelle.

# 10. Prévision sur 1997

pred_log <- forecast(meilleur_modele, h = length(test))

plot(pred_log,
     main = "Prévision SARIMA sur log(CO2)",
     ylab = "log(CO2)",
     xlab = "Temps")

lines(log_test, col = "red", lwd = 2)

legend("topleft",
       legend = c("Prévision", "Valeurs réelles"),
       col = c("blue", "red"),
       lwd = 2)

# 11. Retour à l'échelle originale

pred_co2 <- exp(pred_log$mean)

plot(test,
     main = "Prévision SARIMA sur CO2 - année 1997",
     ylab = "CO2 en ppm",
     xlab = "Temps",
     col = "red",
     lwd = 2,
     ylim = range(c(test, pred_co2)))

lines(ts(pred_co2,
         start = start(test),
         frequency = frequency(test)),
      col = "blue",
      lwd = 2)

legend("topleft",
       legend = c("Valeurs réelles", "Prévision SARIMA"),
       col = c("red", "blue"),
       lwd = 2)

# 12. Erreurs de prévision sur 1997

val_obs <- as.numeric(test)
val_pred <- as.numeric(pred_co2)

rmse_sarima <- sqrt(mean((val_pred - val_obs)^2))
mae_sarima  <- mean(abs(val_pred - val_obs))
mape_sarima <- mean(abs((val_obs - val_pred) / val_obs)) * 100

cat("\n===== Erreurs de prévision sur 1997 =====\n")
cat("RMSE =", rmse_sarima, "\n")
cat("MAE  =", mae_sarima, "\n")
cat("MAPE =", mape_sarima, "%\n")