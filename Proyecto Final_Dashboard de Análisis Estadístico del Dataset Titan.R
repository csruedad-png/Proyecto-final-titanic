# =========================================
# PROYECTO TITANIC
# CONTRASTE DE HIPÓTESIS Y REGRESIÓN
# =========================================

# ==============================
# 1. INSTALAR LIBRERÍAS
# (solo ejecutar la primera vez)
# ==============================

install.packages("tidyverse")
install.packages("ggplot2")

# ==============================
# 2. CARGAR LIBRERÍAS
# ==============================

library(tidyverse)
library(ggplot2)

# ==============================
# 3. CARGAR DATASET TITANIC
# ==============================

df <- read.csv("https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv")

# revisar datos
str(df)
head(df)

# ==============================
# 4. LIMPIEZA DE DATOS
# ==============================

df <- df %>%
  drop_na(Age)

# ==============================
# 5. CONTRASTE DE HIPÓTESIS
# ==============================

edad_sobrevivieron <- df$Age[df$Survived == 1]
edad_no_sobrevivieron <- df$Age[df$Survived == 0]

prueba_t <- t.test(edad_sobrevivieron, edad_no_sobrevivieron)

print(prueba_t)

# ==============================
# 6. REGRESIÓN LINEAL
# ==============================

modelo_lineal <- lm(Fare ~ Age + SibSp + Parch, data = df)

summary(modelo_lineal)

# ==============================
# 7. REGRESIÓN LOGÍSTICA
# ==============================

modelo_logistico <- glm(Survived ~ Age + SibSp + Parch + Fare,
                        data = df,
                        family = binomial)

summary(modelo_logistico)
# ==============================
# 8. GRÁFICA 1
# SOBREVIVIENTES
# ==============================

grafica1 <- ggplot(df, aes(x = factor(Survived))) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Cantidad de sobrevivientes del Titanic",
    x = "Supervivencia",
    y = "Número de pasajeros"
  )

print(grafica1)
# ==============================
# 9. GRÁFICA 2
# EDAD Y SUPERVIVENCIA
# ==============================

grafica2 <- ggplot(df, aes(x = Age, fill = factor(Survived))) +
  geom_histogram(bins = 30, alpha = 0.7) +
  labs(
    title = "Distribución de edad según supervivencia",
    x = "Edad",
    y = "Frecuencia"
  )

print(grafica2)
# ==============================
# 10. GRÁFICA 3
# TARIFA Y SUPERVIVENCIA
# ==============================

grafica3 <- ggplot(df, aes(x = factor(Survived), y = Fare, fill = factor(Survived))) +
  geom_boxplot() +
  labs(
    title = "Tarifa pagada según supervivencia",
    x = "Supervivencia",
    y = "Tarifa"
  )

print(grafica3)

# ==============================
# 11. GRÁFICA 4
# EDAD VS TARIFA
# ==============================

grafica4 <- ggplot(df, aes(x = Age, y = Fare, color = factor(Survived))) +
  geom_point() +
  labs(
    title = "Relación entre edad y tarifa pagada",
    x = "Edad",
    y = "Tarifa"
  )

print(grafica4)
# ==============================
# 12. GRÁFICA 5
# FAMILIARES A BORDO
# ==============================

grafica5 <- ggplot(df, aes(x = SibSp, fill = factor(Survived))) +
  geom_bar(position = "dodge") +
  labs(
    title = "Número de hermanos o cónyuges a bordo",
    x = "SibSp",
    y = "Cantidad"
  )

print(grafica5)
# ==============================
# 13. GUARDAR GRÁFICAS
# ==============================

ggsave("grafica1_sobrevivientes.png", grafica1)
ggsave("grafica2_edad.png", grafica2)
ggsave("grafica3_tarifa.png", grafica3)
ggsave("grafica4_relacion.png", grafica4)
ggsave("grafica5_familia.png", grafica5)

# =========================================
# FIN DEL CÓDIGO
# =========================================