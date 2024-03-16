# 3+5
#n : taille d u n echantillon
# calcul de la moyenne empirique Xn|

#nobs : nombre d'echantillons crees 
# => c'est la taille de l'echantillon des moyennes

#dbinom() => densite
#pbinom() => proba ~ fct repartition
#qbinom() => quantiles ( P[X <  y] = x , on donne x on recupere y)
#si x suit binomiale(N,p)
#rbinom(size, N, p)
#rbinom() => random
install.packages("ggplot2")
library(ggplot2)
library(tidyr)

#1er echantillon
n <- 1000 #augmente la vitesse de conv
nobs <- 50
p <- 0.5
N <- 30 #essais 
lambda <- 1
#ech1 <- rbinom(n,N,p)
# ech1 = [ balbalallb] => visualiser il faut un histo
#hist(ech1)

#mean = moyenne empirique
#mean(ech1)

#on veut save uniquement la moyenne, pas l'echantillon
#echantillon_binomial <- replicate(nobs, mean(rbinom(n, N, p)))

#df "data frame"
#df <- data.frame(bin = echantillon_binomial)

#echantillon_cauchy <- replicate(nobs, mean(rcauchy(n, 0, scale = 1)))
#df <- data.frame(bin = echantillon_cauchy)

echantillon_expo <- replicate(nobs, mean(rexp(n, lambda + 2)))
df <- data.frame(lambda3
                 = echantillon_expo)
df$lambda10 <- replicate(nobs, mean(rexp(n, lambda + 9)))

df_tidy <- df %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value")

ggplot(df_tidy) +
  geom_histogram(aes(x = Value, fill = Variable), binwidth = 0.1, alpha = 0.3) +
  geom_density(aes(x = Value, color = Variable)) +
  facet_wrap(~ Variable, scales = "free") +
  labs(x = "Value", y = "Frequency") +
  scale_fill_manual(values = c("lambda_025" = "yellow", "lambda10" = "green")) +
  scale_color_manual(values = c("lambda_025" = "red", "lambda10" = "blue")) +
  theme_minimal()
# Histogramme basique
#ggplot(df, aes(x=bin)) + geom_histogram() + ggtitle("Histogramme d'echantillons d'exponentielles de paramètre(lambda = 1)") + xlab("Moyenne des tirages") + ylab("") + geom_density(color="darkred") + geom_vline(aes(xintercept=mean(echantillon_expo)), color="blue", linetype="dashed", size=1)


ggplot(df, aes(x = bin, y = norm)) + 
  geom_histogram()
  ggtitle("tat")

#Objectif : verif suit loi Normale
#hist(df$bin)

mean(df$bin)
var(df$bin)

#test hypothese
# W = statistique de test
# p value = proba H0 accepte
# ici H0 = etre une loi normale (param inconnus)
#p value acceptee si p > 0.05
shapiro.test(df$bin)

# check : 
test <- shapiro.test(df$bin)

#graphiquement
#qqplot() / qqnorm() (qqnorm + generique)
qqnorm(df$bin)
qqline(df$bin, col = 'blue')
qqPlot(df$bin, col = 'red')

install.packages("car")
library(car) #qq plot plus joli


#Todo : remplacer la loi binomiale
# ajouter densite sur la courbe
# et la densite theorique 
# proportion de p value < 0.05 
# varie selon n , nobs ??
# utiliser gg plots au lieu de qq plot

df$norm <- replicate(nobs, mean(rbinom(n, N+20, p+0.3)))
qqPlot(df$bin, col = 'red')

#redaction : shapiro W = H0 = blbbala donc accepte ou refuse
# proportion de tests failed , depend de n , nobs
# graph de refus loi normale en fct de n
# vitesse de convergeance


