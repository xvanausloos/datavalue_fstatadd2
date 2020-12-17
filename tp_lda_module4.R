# TP module 4 LDA 
# livre cornillon
# 17/12/20 XV

# importer les donnees
# ronfle <- read.csv("C:/xaviertemp/datavalue_fstatadd2/ronfle.csv")
donnees <- read.table("ronfle.txt", header=T)

# resumons le jeu de donnees
summary(donnees)

# 2 construire le modele
library(MASS)
modele <- lda(ronfle~., data = donnees)
modele

#supprimons poids et taille du modele
modele1 <- lda(ronfle~age+alcool+sexe+taba, data=donnees)
modele1

# utilisons PRIOR pour affecter au modele des proba a priori egales
modele2 <- lda(ronfle~., data=donnees, prior=c(0.5,0.5))
modele2

# 3. estimer le taux de mauvais classement
prev <- lda(ronfle~., data=donnees, CV=TRUE)$class
table(prev, donnees$ronfle)

# estimation du taux de mauvais classement
sum(prev!=donnees$ronfle) / nrow(donnees)

# taux d erreur des autres modeles etudies
prev1 <- lda(ronfle~age+alcool+sexe+taba, data=donnees, CV=TRUE)$class
table(prev1, donnees$ronfle)
sum(prev1!=donnees$ronfle)/nrow((donnees))

prev2 <- lda(ronfle~., data=donnees, prior=c(0.5, 0.5), CV=TRUE)$class
table(prev2, donnees$ronfle)

sum(prev2!=donnees$ronfle)/nrow(donnees)

prev3 <- lda(ronfle~age+alcool+sexe+taba, prior=c(0.5, 0.5), data=donnees, CV=TRUE)$class
table(prev3, donnees$ronfle)

sum(prev3!=donnees$ronfle)/nrow(donnees)

#4. Faire de la prevision
n_don1 <- matrix(c(42,55,169,0,58,94,185,4,35,70,180,6,67,63,166,3), ncol=4, byrow=T)
n_don2 <- matrix(c("F","N","H","O","H","O","F","N"), ncol=2, byrow=T)
n_donnees<-cbind.data.frame(n_don1, n_don2)

# on passe le nom des colonnes dans le nouvel objet dataframe et on ne prend que les 6 derniers 
names(donnees)

names(n_donnees) <- names(donnees)[-6]

names(n_donnees)

predict(modele, newdata = n_donnees)

