####IMPUTATION DES DONNEES MANQUANTES SUR RSTUDIO



##Importation de la base
base_midm<- read.csv2("C:/Users/dell/Desktop/ENSAE/ISEP2/Semestre_2/Programmation R/Exposé/Donnees_expose.csv")
View(base_midm)

##Information de la base

summary(base_midm)

##voir les données manquantes avec sapply qui applique une fonction donnée à 
##chaque élément d'une liste et renvoie les résultats sous forme de vecteur. Dans ce cas, 
##nous appliquons la fonction sum(is.na(x)) à chaque variable de la base de ames_raw 

sapply(base_midm, function(x) sum(is.na(x)))

##########################

##Imputation par suppression

# Création d'un data.frame avec des valeurs manquantes
df <- data.frame(var1 = c(1, 2, NA, 4, 5), var2 = c(NA, 7, 8, NA, 10))

# Suppression des observations avec des données manquantes
df_complete <- df[complete.cases(df), ]

# Affichage du data.frame complet
df_complete



###########################


##Imputation par la moyenne sur la variable Lot_Frontange


##Sans aucune bibiliothèque

# copie de la base
library(data.table)
midm1<-copy(base_midm)
View(midm1)
# Calculer la moyenne de LotFrontage sans les valeurs manquantes
mean_Lot_Frontage <- mean(midm1$Lot_Frontage, na.rm = TRUE)

# Remplacer les valeurs manquantes par la moyenne
midm1$Lot_Frontage[is.na(midm1$Lot_Frontage)] <- mean_Lot_Frontage

#Verification des valeurs manquantes
sum(is.na(midm1$Lot_Frontage))
View(midm1)

##Avec la bibliothèque Hmisc

library(Hmisc)

midm2<-copy(base_midm)

# Imputation des valeurs manquantes dans la variable "LotFrontage"
midm2$Lot_Frontage <- impute(midm2$Lot_Frontage, mean)

# Vérification des valeurs manquantes après l'imputation
sapply(midm2, function(x) sum(is.na(x)))
View(midm2)



###########################





###Imputation par la médian sur la variable Lot_Frontage

##Avec la bibliothéque Hmisc


library(Hmisc)
midm_med<-copy(base_midm)

# Imputation des valeurs manquantes par la médiane pour la variable LotFrontage
midm_med$Lot_Frontage <- with(midm_med, impute(Lot_Frontage, median))


# Vérification des valeurs manquantes après imputation
sapply(midm_med, function(x) sum(is.na(x)))






###########################





## Imputation par la regression 


# Exemple pour la variable "LotFrontage"
midm_rl<-copy(base_midm)
model <- lm(Sale_Price ~ ., data = midm_rl[, -1])
midm_rl$Sale_Price <- ifelse(is.na(midm_rl$Sale_Price), predict(model, newdata = midm_rl), midm_rl$Sale_Price)
View(midm_rl)
sum(is.na(midm_rl))



###########################





##Imputation par la méthode de LOCF(Last Observation Carried Forward) 



library(zoo)
midm_locf<-copy(base_midm)
#Imputer les valeurs manquantes de la variable « Lot_Frontage »

midm_locf$Lot_Frontage <- na.locf(midm_locf$Lot_Frontage) 

#Imputer les valeurs manquantes de la variable « Garage_Yr_Blt »

midm_locf$Garage_Yr_Blt <- na.locf(midm_locf$Garage_Yr_Blt) 

#Vérifier que toutes les valeurs manquantes ont été remplacées

sum(is.na(midm_locf)) #Vérifier que toutes les valeurs manquantes ont été remplacées
summary(midm_locf)




###########################



##  Imputation par la méthode de Monte carlo

midm_mc<-copy(base_midm)
library(mice)

# Convertir les variables qualitatives en variables facteur

midm_mc <- data.frame(lapply(midm_mc, factor))

# Convertir les variables qualitatives en variables facteur
m <- 1 # nombre d'itérations

imp <- mice(midm_mc, m = m, method = "norm.predict")

summary(imp)





###########################



#Imputation par les K plus proche voisins


library(VIM)
midm_k<-copy(base_midm)

dat.kNN=kNN(midm_k, k=5, imp_var=FALSE)





###########################





# Imputation par l’analyse en composantes principales (ACP)

midm_acp<-copy(base_midm)

# Supprimer les observations contenant des valeurs manquantes
data_complete <- na.omit(midm_acp)

# Effectuer une ACP sur les données complètes
pca <- princomp(data_complete)

# Imputer les valeurs manquantes à l'aide des composantes principales
data_imputed <- predict(pca, newdata = midm_acp, na.action = na.pass)
View(data_complete)
View(data_imputed)





##########################




#Imputation par la Foret aleatoire


library(missForest)
midm_fa<-copy(base_midm)
mifa.imp <- missForest(midm_fa)
mifa.complete <- mifa.imp$ximp
View(mifa.complete)

   