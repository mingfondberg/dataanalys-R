file_path <- "C:/Users/Admin/Downloads/bilardata1 (1).xlsx"
car_data <- read_excel(file_path)
#översikt på datan
View(car_data)
dim(car_data)
head(car_data)
summary(car_data)
#missing
sum(is.na(car_data))
# Beräkna genomsnittspriset per märke
# Visa unika bilmarken
unique_brands <- unique(car_data$Brand)

print(unique_brands)
#ta bort lyxbilar som är dyrare än 1000000
car_data_ny <- car_data[car_data$Price <= 1000000, ]
# Filtrera data frame för att endast inkludera bilar från år 2010 till 2024
car_data_ny_filtered <- car_data_ny[car_data_ny$Year >= 2010 & car_data_ny$Year <= 2024, ]
dim(car_data_ny_filtered)
View(car_data_ny_filtered)
summary(car_data_ny_filtered)
str(car_data_ny_filtered)
sum(is.na(car_data_ny_filtered))
#se pris
library(ggplot2)   #ming1
ggplot(car_data_ny, aes(x = Price)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Histogram of Car Prices", x = "Price") +
  theme_minimal()



car_numeric <- car_data_ny_filtered[, sapply(car_data_ny_filtered, is.numeric)]

options(repr.plot.width = 8, repr.plot.height = 6)
pairs(car_numeric, pch = 16)     #ming2
#outliers
z_scores <- scale(car_numeric)
threshold <- 2
outlier_rows <- apply(abs(z_scores) > threshold, 1, any)
car_data_filtered <- car_data_ny_filtered[!outlier_rows, ]
pairs(car_data_filtered[, sapply(car_data_filtered, is.numeric)], pch = 16)    # ming 3
library(fastDummies)
categorical_features <- c("Brand", "Year", "Fuel", "Mileage", "Gearbox")
car_dummies <- dummy_cols(car_data_filtered, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
str(car_dummies)
summary(car_dummies)
correlation_matrix <- cor(car_dummies)
high_correlation_threshold <- 0.7
high_correlation_pairs <- which(abs(correlation_matrix) > high_correlation_threshold & correlation_matrix != 1, arr.ind = TRUE)
if (length(high_correlation_pairs) > 0) {
  print("Highly correlated variables:")
  print(high_correlation_pairs)
}


cor_mm <- cor(car_dummies$Year, car_dummies$Mileage)
print(paste("Correlation between Mileage and Year:", cor_mm))
par(mfrow = c(1, 1))
plot(car_dummies$Mileage, car_dummies$Year, 
     xlab = "Mileage", ylab = "Year", 
     main = "Scatter Plot of Mileage vs. Year",
     col = c("blue", "red"))    #ming4
#dela data
set.seed(123)
train_indices <- sample(nrow(car_dummies), size = round(0.8 * nrow(car_dummies)), replace = FALSE)
train_data <- car_dummies[train_indices, ]
test_data <- car_dummies[-train_indices, ]

X_train <- train_data[, !names(test_data) %in% "Price"]
y_train <- train_data$Price

X_test <- test_data[, !names(test_data) %in% "Price"]
y_test <- test_data$Price

lm_1 <- lm(Price ~., data = train_data)

summary(lm_1)

par(mfrow = c(2, 2))
plot(lm_1)    #ming5
#information on multicollinearity


#Korsvalidering
train_control <- trainControl(method = "cv", number = 10)
cv_model <- train(Price ~ ., data = train_data, method = "lm", trControl = train_control)

print(summary(cv_model))



#logaritmisk modell --------------------------------------------------------
train_data_log <- train_data
train_data_log$Price <- log(train_data_log$Price)

# Träna en linjär regressionsmodell på den log-transformerade priset
log_lm_model <- lm(Price ~ ., data = train_data_log)

# Sammanfattning och diagnostiska plots för den log-transformerade modellen
print(summary(log_lm_model))
par(mfrow = c(2, 2))
plot(log_lm_model)    #ming6

# Beräkna leverage-värden
leverage_values <- hatvalues(log_lm_model)
high_leverage_indices <- which(leverage == 1)
# Visa leverage-värden
head(leverage_values)
# Antal prediktorer och observationer
n <- nrow(train_data_log)
k <- length(coefficients(log_lm_model))  # Inkluderar intercept

# Beräkna tröskel för hög leverage
leverage_threshold <- 2 * (k + 1) / n

# Identifiera observationspunkter med hög leverage
high_leverage_points <- which(leverage_values > leverage_threshold)


# Visa index för hög leverage observationspunkter
print(high_leverage_points)
# Visa observationspunkter med hög leverage
train_data_log[high_leverage_points, ]
## Ta bort observationspunkter med hög leverage från datamängden
train_data_filtered <- train_data_log[-high_leverage_points, ]
# Träna en linjär regressionsmodell på den filtrerade datamängden
log_lm_model_filtered <- lm(Price ~ ., data = train_data_filtered)

# Visa en sammanfattning av den nya modellen
summary(log_lm_model_filtered)
par(mfrow = c(2, 2))
plot(log_lm_model_filtered)    #ming7

# Antag att train_data_log är din ursprungliga datamängd och log_lm_model din ursprungliga modell

# Ta bort observationspunkt 3080 från datamängden
train_data_filtered <- train_data_log[-3080, ]

# Träna en ny linjär regressionsmodell på den filtrerade datamängden
log_lm_model_filtered <- lm(Price ~ ., data = train_data_filtered)

# Visa en sammanfattning av den nya modellen
summary(log_lm_model_filtered)

# Visualisera diagnostiska plottar för att utvärdera den nya modellen
par(mfrow = c(2, 2))
plot(log_lm_model_filtered)    #ming8

# Shapiro-Wilk test for normality
shapiro.test(residuals(log_lm_model_filtered))

# Visualize residuals with a histogram and a Q-Q plot
par(mfrow = c(1, 2))  # Layout för två bilder i en rad
hist(residuals(log_lm_model_filtered), breaks = 30, main = "Histogram of Residuals")
qqnorm(residuals(log_lm_model_filtered))
qqline(residuals(log_lm_model_filtered), col = "red")    #ming9

plot(residuals(log_lm_model_filtered), type = 'l',
     xlab = "Observation Index",
     ylab = "Residuals",
     main = "Time Plot of Residuals")     #ming10

# Gör förutsägelser med den log-transformerade modellen
predicted_log_prices <- predict(log_lm_model_filtered, newdata = test_data)
# Omvandla log-priser till faktiska priser
predicted_prices <- exp(predicted_log_prices)
# Beräkna residualer
residuals_test <- y_test - predicted_prices  # Anta att y_test innehåller de faktiska priserna

# Beräkna RMSE och MAE
rmse_test <- sqrt(mean(residuals_test^2, na.rm = TRUE))
mae_test <- mean(abs(residuals_test), na.rm = TRUE)

# Sammanställ och skriv ut resultat
results_test <- data.frame(
  RMSE = rmse_test,
  MAE = mae_test
)
print(results_test)
# Skapa en data frame för att visa resultat
results_test <- data.frame(
  RMSE = rmse_test,
  MAE = mae_test,
  Adjusted_R_Squared = adjusted_r_squared_test,
  BIC = bic_test
)

# Skriv ut resultaten
print(results_test)

# För Mean of Price in Test
mean_of_price_test <- mean(test_data$Price, na.rm = TRUE)  # `na.rm = TRUE` tar bort NA-värden från beräkningen

# För MAE
predictions_test <- predict(log_lm_model_filtered, newdata = test_data)  # 'your_model' är modellen du använder
actuals_test <- test_data$Price
mae_test <- mean(abs(predictions_test - actuals_test), na.rm = TRUE)  # Beräknar medelvärdet av de absoluta felen

print(mae_test)



#visa resultaten grafiskt
ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs. Predicted Prices", x = "Actual Price", y = "Predicted Price") +
  theme_minimal()    #ming 11


