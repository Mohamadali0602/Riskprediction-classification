data <- read.csv("projet.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = T)
data_new <- read.csv("projet_new.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = T)

str(data)
ls = data.frame(unclass(summary(data)), check.names = FALSE, stringsAsFactors = FALSE)

table(data_new$revenus)
#-------------------------#
# AFFICHAGE DES EFFECTIFS #
#-------------------------#
# Effectifs des valeurs de la variable adresse
table(data$adresse)
# Effectifs des valeurs de la variable age
table(data$age)
# Effectifs des valeurs de la variable emploi
table(data$emploi)
# Effectifs des valeurs de la variable categorie
table(data$categorie)


data <- subset(data,select= -client)
data <- subset(data,select= -categorie)
#-----------------------------------#
# Diagrammes circulaire en secteurs #
#-----------------------------------#

pie(table(data$age), main = "Repartition des ages",cex = 0.8,radius = 1.05)
pie(table(data$education), main = "Repartition des niveaux d'educations",cex = 0.8,radius = 1.05)

library(ggplot2)
qplot(debcred, data=data, fill=defaut, binwidth=2, main="Distibution de Revenus", xlab="Valeur de Revenus", ylab="Nombre d'instances")
qplot(age, data=data, fill=defaut, binwidth=2, main="Distibution de l'age ", xlab="age", ylab="Nombre d'instances")
qplot(adresse, data=data, fill=defaut, binwidth=2, main="Distibution adresse ", xlab="adresse", ylab="Nombre d'instances")


#----------------------------------------#
# HISTOGRAMMES D'EFFECTIFS DES VARIABLES #
#----------------------------------------#
data_agefilter <- data[data$age != 999,]
data_filter <- data_agefilter[data_agefilter$adresse != 999, ]
# suppresion 
hist(table(data$revenus),breaks = 100, main = "Repartition des revenus",xlab= "revenus en milliers de $",ylab = "nb de clients")
hist(data_filter$age,main = "Repartition des ages", xlab = "age",ylab="nb de clients")
hist(data_filter$emploi,main = "Nombre d'années avec l'employeur actuel ", xlab = "année",ylab="nb de clients")
hist(data_filter$adresse,main = "Nombre d'années à l'adresse actuelle", xlab = "année",ylab="nb de clients")
hist(data$revenus,breaks = 100,main = "les revenus", xlab = "millers $",ylab="nb de clients")
hist(data_filter$debcred,main = "Ratio Débit/Crédit (x100) ", xlab = "ratio",ylab="nb de clients")
hist(data_filter$debcarte,breaks = 100,main = "Débit carte de crédit en milliers de $", xlab = "debit",ylab="nb de clients")
hist(data_filter$autres[data_filter$autres<60],breaks = 100,main = "Autres dettes en milliers de $ ", xlab = "dette",ylab="nb de clients")
hist(as.numeric(data_filter$defaut),main = "Autres dettes en milliers de $ ", xlab = "defaut",ylab="nb de clients")
# remplacement avec la mediane
hist(data$revenus,breaks = 100, main = "Repartition des revenus",xlab= "revenus en milliers de $",ylab = "nb de clients")
hist(data$age,main = "Repartition des ages", xlab = "age",ylab="nb de clients")
hist(data$emploi,main = "Nombre d'années avec l'employeur actuel ", xlab = "année",ylab="nb de clients")
hist(data$adresse,main = "Nombre d'années à l'adresse actuelle", xlab = "année",ylab="nb de clients")
hist(data$revenus,breaks = 100,main = "les revenus", xlab = "millers $",ylab="nb de clients")
hist(data$debcred,main = "Ratio Débit/Crédit (x100) ", xlab = "ratio",ylab="nb de clients")
hist(data$debcarte,breaks = 100,main = "Débit carte de crédit en milliers de $", xlab = "debit",ylab="nb de clients")
hist(data$autres[data$autres<60],breaks = 100,main = "Autres dettes en milliers de $ ", xlab = "dette",ylab="nb de clients")
hist(as.numeric(data$defaut),main = "Autres dettes en milliers de $ ", xlab = "defaut",ylab="nb de clients")

table(data$defaut)
A = c(1669,4331)
B = c("Oui","Non")
barplot(A,names.arg = B,main = "Nb de default")
text(
  x = barplot(A, names.arg = B, col = "steelblue", ylim = c(0, max(A) * 1.2),main = "Nb de default"),
  y = A + 1, labels = A, pos = 3, cex = 1.2, col = "black"
)
table(data$autres[data$autres>20])
# la donnée 416 peut etre fausse. On la supprime.  

#-----------------------#
# TABLES DE CONTINGENCE #
#-----------------------#
options(digits=2)

prop.table(table(data$debcred, data$defaut, dnn=c("debcred","defaut")))*100
table(data$debcarte, data$defaut)

#------------------#
# NUAGES DE POINTS #
#------------------#


qplot(age,revenus, data=data_agefilter,
      main="Nuage de point de Revenus et Age", 
      xlab="Valeur de Age", ylab="Valeur de Revenus",
      color = age)
qplot(debcred,debcarte, data=data,
      main="Nuage de point de debit carte et le ratio", 
      xlab="Valeur de debcred", ylab="Valeur de debcarte",
      color = debcarte) 

qplot(debcred,education, data=data,
      main="Nuage de point de debit carte et le ratio", 
      xlab="Valeur de debcred", ylab="Valeur de debcarte",
      color = debcarte) 


#-----------------------#
# BOXPLOT DES VARIABLES #
#-----------------------#
# boxplot pour l'age
c_age = cbind(data$age[data$defaut == "Oui"],data$age[data$defaut == "Non"])
boxplot(c_age,data=data, notch=T,main="Représentation de l'age en fonction du defaut",names = c("Oui","Non"))

# boxplot pour l'adresse
c_adresse = cbind(data$adresse[data$defaut == "Oui"],data$adresse[data$defaut == "Non"])
boxplot(c_adresse,data=data, notch=T,main="Représentation du nb d'années à une adresse en fonction du defaut",names = c("Oui","Non"))

# boxplot pour le revenus
c_revenus = cbind(data$revenus[data$defaut == "Oui"],data$revenus[data$defaut == "Non"])
boxplot(c_revenus,data=data, notch=T,main="Représentation des revenus en fonction du defaut",names = c("Oui","Non"),outline = FALSE)

# boxplot pour le revenus
c_revenus = cbind(data$revenus[data$defaut == "Oui"],data$revenus[data$defaut == "Non"])
boxplot(c_revenus,data=data, notch=T,main="Représentation des revenus en fonction du defaut",names = c("Oui","Non"),outline = FALSE)

# boxplot pour l emploi
c_emploi = cbind(data$emploi[data$defaut == "Oui"],data$emploi[data$defaut == "Non"])
boxplot(c_emploi,data=data, notch=T,main="Représentation des années d'emploi en fonction du defaut",names = c("Oui","Non"),outline = FALSE)

# boxplot pour debcred
c_debcred = cbind(data$debcred[data$defaut == "Oui"],data$debcred[data$defaut == "Non"])
boxplot(c_debcred,data=data, notch=T,main="Représentation des ratios debit/credit en fonction du defaut",names = c("Oui","Non"),outline = FALSE)

# boxplot pour debcarte
c_debcarte = cbind(data$debcarte[data$defaut == "Oui"],data$debcarte[data$defaut == "Non"])
boxplot(c_debcarte,data=data, notch=T,main="Représentation des debit carte en fonction du defaut",names = c("Oui","Non"),outline = FALSE)

# boxplot pour des autres dettes
c_autres = cbind(data$autres[data$defaut == "Oui"],data$autres[data$defaut == "Non"])
boxplot(c_autres,data=data, notch=T,main="Représentation des autres dettes en fonction du defaut",names = c("Oui","Non"),outline = FALSE)

#----------------------------#
# Pré-Traitement des données #
#----------------------------#

t_test_age <- t.test(data$age[data$defaut == "Oui"],data$age[data$defaut == "Non"],var.equal = FALSE)
t_test_age


t_test <- t.test(data$adresse[data$defaut == "Oui"],data$adresse[data$defaut == "Non"],var.equal = FALSE)
t_test

# filtrage des données manquantes
median_age = median(data$age)
data$missing_age <- ifelse(data$age == 999, 1, 0)
data$age <- ifelse(data$age == 999,median_age,data$age)

median_adresse = median(data$adresse)
data$missing_adresse <- ifelse(data$adresse == 999, 1, 0)
data$adresse <- ifelse(data$adresse == 999, median_adresse,data$adresse)

# Summary des données filtrées
ls_filter = data.frame(unclass(summary(data)), check.names = FALSE, stringsAsFactors = FALSE)

# Voir les données apres filtrage 
table(data$age)

#je veux enlever les valeurs extremes qui sont pas normales.
table(data$revenus[data$revenus > 100]) 
# les valeurs plus grands que 600 ne sont pas normaux et ne sont pas essentiel pour notre modele . on les enlève:
data <- data[data$revenus < 600,]

#regardons debcred 
table(data$debcred[data$debcred >30]) 
# ca semble correct

table(data$debcarte[data$debcarte >10])
#data <- data[data$debcarte < 50,]
table(data$autres[data$autres > 10])

sum_data <-data.frame(unclass(summary(data)), check.names = FALSE, stringsAsFactors = FALSE)

#----------------------------------------------#
# CLUSTERING DES DONNÉES                       #
#----------------------------------------------#
library(cluster)
library(ggplot2)


data_cluster <- data[, c("age", "emploi", "revenus", "debcred", "debcarte", "autres")]

data_cluster_scaled <- scale(data_cluster)

# Calcul de la matrice de distance par la fonction daisy() pour variables hétérogènes
dmatrix <- daisy(data_cluster)

# Résumé de la matrice de distance
summary(dmatrix)

#---------------------------------#
# CLUSTERING PAR PARTITIONNEMENT  #
#---------------------------------#

set.seed(122353)

# K = 4
km <- kmeans(data_cluster_scaled, 4)

# Répartition des classes défaut=Oui/Non par cluster
table(km$cluster, data$defaut)

data$cluster <- as.factor(km$cluster)

# Histogramme des nombres de oui et non dans chaque clusters avec la classe en couleur
ggplot(data, aes(x = cluster, fill = defaut)) +
  geom_bar(position = "dodge") +
  labs(title = "Répartition des classes par cluster", x = "Cluster", y = "Nombre d'individus") +
  theme_minimal()

# Visualisation des clusters avec ggplot
ggplot(data, aes(x = age, y = debcred, color = cluster)) +
  geom_jitter(width = 0.3, height = 0.3, alpha = 0.6) +
  theme_minimal() +
  labs(title = "Clusters K-means", x = "Âge", y = "Debcred", color = "Cluster") +
  scale_color_brewer(palette = "Set1")

ggplot(data, aes(x = debcred, y = revenus, color = cluster)) +
  geom_jitter(width = 0.3, height = 0.3, alpha = 0.6) +
  theme_minimal() +
  labs(title = "Clusters K-means", x = "debcred", y = "Revenus", color = "Cluster") +
  scale_color_brewer(palette = "Set1")

ggplot(data, aes(x = revenus, y = debcarte, color = cluster)) +
  geom_jitter(width = 0.3, height = 0.3, alpha = 0.6) +
  theme_minimal() +
  labs(title = "Clusters K-means", x = "Revenus", y = "Debcarte", color = "Cluster") +
  scale_color_brewer(palette = "Set1")

ggplot(data, aes(x = age, y = debcred, color = cluster)) +
  geom_jitter(width = 0.3, height = 0.3, alpha = 0.6) +
  theme_minimal() +
  labs(title = "Clusters K-means", x = "age", y = "Debcred", color = "Cluster") +
  scale_color_brewer(palette = "Set1")

ggplot(data, aes(x = emploi, y = autres, color = cluster)) +
  geom_jitter(width = 0.3, height = 0.3, alpha = 0.6) +
  theme_minimal() +
  labs(title = "Clusters K-means", x = "Emploi", y = "Autres", color = "Cluster") +
  scale_color_brewer(palette = "Set1")

ggplot(data, aes(x = debcred, y = debcarte, color = cluster)) +
  geom_jitter(width = 0.3, height = 0.3, alpha = 0.6) +
  theme_minimal() +
  labs(title = "Clusters K-means", x = "Debcred", y = "Debcarte", color = "Cluster") +
  scale_color_brewer(palette = "Set1")

# je definis la plage des valeurs de K à tester
k_values <- 1:10  

# Vecteur pour stocker la somme totale des carrés des distances intra-cluster (WSS)
wss <- numeric(length(k_values))

# Je calcule le WSS pour chaque K
for (k in k_values) {
  set.seed(123)  
  km_result <- kmeans(data_cluster_scaled, centers = k, nstart = 25)
  wss[k] <- km_result$tot.withinss
}

# Graphique du coude de la methode elbow
elbow_plot <- ggplot(data.frame(K = k_values, WSS = wss), aes(x = K, y = WSS)) +
  geom_line(size = 1) +
  geom_point(size = 2, color = "red") +
  labs(title = "Méthode du Coude pour le K optimal", x = "Nombre de Clusters (K)", y = "Somme Totale des Carrés des Distances Intra-Cluster (WSS)") +
  theme_minimal() +
  geom_vline(xintercept = 4, linetype = "dashed", color = "blue") +  
  annotate("text", x = 4, y = Inf, label = "Coude", vjust = 1.5, color = "blue")  

print(elbow_plot)

#----------------------------------#
# CLUSTERING DES DONNÉES avec tsne #
#----------------------------------#
#install.packages("Rtsne")
library(Rtsne)

set.seed(122353)
tsne_result <- Rtsne(data_cluster_scaled, perplexity = 30, max_iter = 1000)

data$tsne1 <- tsne_result$Y[, 1]
data$tsne2 <- tsne_result$Y[, 2]
km <- kmeans(data_cluster_scaled, centers = 4, nstart = 25)


data$cluster <- as.factor(km$cluster)

table(km$cluster, data$defaut)


ggplot(data, aes(x = cluster, fill = defaut)) +
  geom_bar(position = "dodge") +
  labs(title = "Répartition des classes par cluster", x = "Cluster", y = "Nombre d'individus") +
  theme_minimal()

# Visualisation des clusters avec t-SNE
ggplot(data, aes(x = tsne1, y = tsne2, color = cluster)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Clusters K-means avec t-SNE", x = "t-SNE Dim.1", y = "t-SNE Dim.2", color = "Cluster") +
  scale_color_brewer(palette = "Set1")



k_values <- 1:10  

wss <- numeric(length(k_values))

for (k in k_values) {
  set.seed(123)  
  km_result <- kmeans(data_cluster_scaled, centers = k, nstart = 25)
  wss[k] <- km_result$tot.withinss
}

# Graphique du coude 
elbow_plot <- ggplot(data.frame(K = k_values, WSS = wss), aes(x = K, y = WSS)) +
  geom_line(size = 1) +
  geom_point(size = 2, color = "red") +
  labs(title = "Méthode du Coude pour le K optimal", x = "Nombre de Clusters (K)", y = "Somme Totale des Carrés des Distances Intra-Cluster (WSS)") +
  theme_minimal() +
  geom_vline(xintercept = 4, linetype = "dashed", color = "blue") +  
  annotate("text", x = 4, y = Inf, label = "Coude", vjust = 1.5, color = "blue")  

print(elbow_plot)

#---------------------#
#Boxplot des clusters #
#---------------------#
for (k in 1:4) {
  cluster_data <- data %>% filter(cluster == k)
  print(nrow(cluster_data))
}

library(ggplot2)
library(dplyr)

variables <- c("age", "emploi", "revenus", "debcred", "debcarte", "autres")

for (var in variables) {
  ylim_min <- quantile(data[[var]], 0.05, na.rm = TRUE)
  ylim_max <- quantile(data[[var]], 0.95, na.rm = TRUE)
  
  p <- ggplot(data, aes_string(x = "cluster", y = var, fill = "cluster")) +
    geom_boxplot(outlier.colour = NA, outlier.shape = NA) +  # pour supprimer les outliers
    labs(title = paste("Boxplot de", var, "pour les Clusters"),
         x = "Cluster",
         y = var) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          plot.margin = unit(c(1, 1, 1, 1), "cm")) +  
    ylim(c(ylim_min, ylim_max))  
  
  print(p)
}

p_defaut <- ggplot(data, aes(x = cluster, fill = defaut)) +
  geom_bar(position = "dodge") +  
  labs(title = "Distribution de la variable 'defaut' pour les Clusters",
       x = "Cluster",
       y = "Nombre d'observations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.margin = unit(c(1, 1, 1, 1), "cm")) 

print(p_defaut)




#--------------------------------------#
#Définition de la méthode d'évaluation #
#--------------------------------------#
#install.packages("tidyverse")
#install.packages("randomForest")
#install.packages("rlang")
#install.packages("pROC") 
#install.packages("caret")
#install.packages("sp")
library(tidyverse)
library(splitstackshape)
library(sp)
library(caret)
library(randomForest)
library(pROC)
library(rpart)
library(C50)
library(tree)


set.seed(13505)
working_set <- data %>% stratified(group = "defaut", size = 0.8)
hold_out <- setdiff(data, working_set)
# je prend 80% des données pour entrainer notre modele et 20% restante pour tester
k <- 10  # Nombre de plis

# Configuration pour la validation croisée
#cv_control <- trainControl(
#  method = 'repeatedcv',
#  number = k,
#  summaryFunction = twoClassSummary,
#  classProbs = TRUE,
#  savePredictions = "all",
#  repeats = 5
#)
repeatedcvIndex <- createMultiFolds(factor(working_set$defaut),times =5,k)

cv_control <- trainControl(
  index = repeatedcvIndex,
  method = 'repeatedcv',
  number = k,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  savePredictions = "all"
)


model1 <- train(
  defaut ~ .,            
  data = working_set,            
  method = "glm",           # logistic regression
  trControl = cv_control   
)

# 2. Random Forest Model
model2 <- train(
  defaut ~ .,            
  data = working_set,             
  method = "rf",           # Random Forest
  trControl = cv_control, 
  importance = TRUE      
)

# random forest tuned
tune_grid_rf <- expand.grid(
  .mtry = c(4,5,6,7),
  .splitrule = 'gini',
  .min.node.size = c(5,10, 15)
)

model2.2 <- train(
  defaut ~ .,            
  data = working_set,             
  method = "ranger",           # Random Forest
  trControl =cv_control, 
  tuneGrid = tune_grid_rf
  )

# 3. Support Vector Machine Model
model3 <- train(
  defaut ~ .,             
  data = working_set,             
  method = "svmRadial",    # Support Vector Machine (Radial Kernel)
  trControl = cv_control, 
  preProcess = c("center", "scale")  
)

#SVM tuned
model3.2 <- train(
  defaut ~ .,            
  data = working_set,           
  method = "svmRadial",    
  trControl = cv_control,
  tuneLength = 10,
  preProcess = c("center", "scale")  
)

# SVM linear
model3.3 <- train(
  defaut ~ .,           
  data = working_set,            
  method = "svmLinear",  # Linear 
  trControl = cv_control,
  preProcess = c("center", "scale") 
)

# 4. Decision Tree Model
model4 <- train(
  defaut ~ .,            
  data = working_set,             
  method = "rpart",        
  trControl = cv_control   
)

# Model 4.1 (Gini split, minbucket = 10)
model4.1 <- train(
  defaut ~ ., 
  data = working_set, 
  method = "rpart", 
  trControl = cv_control, 
  tuneGrid = expand.grid(cp = c(0.001, 0.01, 0.1)),  
  parms = list(split = "gini"),
  control = rpart.control(minbucket = 10) 
)


# Model 4.2 (Gini split, minbucket = 9)
model4.2 <- train(
  defaut ~ .,           
  data = working_set,      
  method = "rpart",       
  trControl = cv_control,  
  tuneGrid = expand.grid(
    cp = 0.01               
  ),
  parms = list(split = "gini"),  
  control = rpart.control(minbucket = 9)  
)

# Model 4.3 (Information split, minbucket = 5)
model4.3 <- train(
  defaut ~ .,             
  data = working_set,   
  method = "rpart",        
  trControl = cv_control,  
  tuneGrid = expand.grid(
    cp = 0.01             
  ),
  parms = list(split = "information"),
  control = rpart.control(minbucket = 5)  
)

# Model 4.4 (Information split, minbucket = 9)
model4.4 <- train(
  defaut ~ .,            
  data = working_set,      
  method = "rpart",      
  trControl = cv_control, 
  tuneGrid = expand.grid(
    cp = 0.01               
  ),
  parms = list(split = "information"),  
  control = rpart.control(minbucket = 9)  
)

# 5. K-Nearest Neighbors Model
model5 <- train(
  defaut ~ .,             
  data = working_set,             
  method = "knn",          
  trControl = cv_control,  
  tuneLength = c(4,5,6,7,8,9,10)          
)

model5.1 <- train(
  defaut ~ .,         
  data = working_set,         
  method = "knn",          
  metric = "ROC",
  preProc = c("center","scale"),
  trControl = cv_control,  
  tuneLength = c(2,4,8,16,20,24,25,30,35,40)       
)


# Differentes valeurs lambda pour la regularisation de la methode LASSO 
lambda <- 10^seq(-1, -4, length = 10)  
grid <- expand.grid(alpha = 1, lambda = lambda)  

#  LASSO (Logit + LASSO)
set.seed(13505)
model6 <- train(
  defaut ~ .,                    
  data = working_set,         
  method = "glmnet",              # LASSO regression
  preProcess = c("center", "scale"),
  family = "binomial",           
  trControl = cv_control,         
  tuneGrid = grid                
)

# On sauvegarde le meilleure modele Lasso
best_lambda <- model6$bestTune$lambda
lasso_coeffs <- as.matrix(coef(model6$finalModel, s = best_lambda))  

# Model C50
# Model 7 (C5.0, minCases = 9, noGlobalPruning = FALSE)
model7 <- train(
  defaut ~ .,             
  data = working_set,     
  method = "C5.0",        
  trControl = cv_control, 
  tuneGrid = expand.grid(
    trials = 50,          
    model = "tree",      
    winnow = c(TRUE, FALSE) 
  ),
  control = C5.0Control(minCases = 9, noGlobalPruning = FALSE)  
)

# Model 7.1 (C5.0, minCases = 9, noGlobalPruning = TRUE)
model7.1 <- train(
  defaut ~ .,             
  data = working_set,     
  method = "C5.0",        
  trControl = cv_control, 
  tuneGrid = expand.grid(
    trials = 50,           
    model = "tree",       
    winnow = c(TRUE, FALSE) 
  ),
  control = C5.0Control(minCases = 9, noGlobalPruning = TRUE)  
)

# Model 7.2 (C5.0, minCases = 4, noGlobalPruning = FALSE)
model7.2 <- train(
  defaut ~ .,             
  data = working_set,    
  method = "C5.0",        
  trControl = cv_control,
  tuneGrid = expand.grid(
    trials = 50,           
    model = "tree",        
    winnow = c(TRUE, FALSE) 
  ),
  control = C5.0Control(minCases = 4, noGlobalPruning = FALSE)  
)

# Model 7.3 (C5.0, minCases = 4, noGlobalPruning = TRUE)
model7.3 <- train(
  defaut ~ .,             
  data = working_set,   
  method = "C5.0",       
  trControl = cv_control, 
  tuneGrid = expand.grid(
    trials = 50,          
    model = "tree",        
    winnow = c(TRUE, FALSE) 
  ),
  control = C5.0Control(minCases = 4, noGlobalPruning = TRUE)  
)


# Model 8 (Tree, split = "gini", mincut = 9)
model8 <- train(
  defaut ~ .,             
  data = working_set,    
  method = "rpart",       
  trControl = cv_control, 
  tuneGrid = expand.grid(
    cp = 0.01              
  ),
  control = rpart.control(
    minbucket = 9,         
    cp = 0.01,            
    split = "gini"         
  ) 
)


# Model 8.1 (Tree, split = "gini", mincut = 4)
model8.1 <- train(
  defaut ~ .,             
  data = working_set,     
  method = "rpart",       
  trControl = cv_control, 
  tuneGrid = expand.grid(
    cp = 0.01             
  ),
  control = rpart.control(minbucket = 4,         
                          cp = 0.01,             
                          split = "gini")  
)

models <- list(model1, 
               model2,model2.2, 
               model3,model3.2, 
               model4,model4.1,model4.2,model4.3,model4.4, 
               model5,model5.1,
               model6,
               model7,model7.1,model7.2,model7.3,
               model8,model8.1)

#--------------------#
# AUC ROC SENS SPEC
#--------------------#
models_auc <- vector("numeric", length = length(models))

for (i in 1:length(models)) {

  auc_values <- models[[i]]$resample$ROC  # AUC pour chaque plis 
  
  models_auc[i] <- mean(auc_values, na.rm = TRUE)
}

# resultat de chaque model
for (i in 1:length(models)) {
  print(models[[i]])
}
# Entraînement des modèles avec validation croisée
cv_results <- list()  

roc_curves <- list()


for (i in 1:length(models)) {
  
  predictions <- models[[i]]$pred
  
  positive_class <- levels(predictions$obs)[2] 
  
  roc_curves[[i]] <- roc(
    response = predictions$obs,                
    predictor = predictions[[positive_class]],  
    levels = rev(levels(predictions$obs))      
  )
}

# Traçons les courbes ROC 
plot(roc_curves[[1]], col = 1, main = "ROC Curves for All Models", lwd = 2)
for (i in 2:length(roc_curves)) {
  plot(roc_curves[[i]], col = i, add = TRUE, lwd = 2)
}

legend("bottomright", legend = paste0("Model ", 1:length(models)),
       col = 1:length(models), lwd = 2)




#---------------# 
# loss function #
#---------------#

FP <- 1    # Cout de faux positive
FN <- 3  # Cout de faux negatif 3x le coup de faux positive
cost <- FN / FP  


# Calculons la prevalence de la classe postive oui
prevalence <- sum(as.numeric(working_set$defaut) - 1) / length(working_set$defaut)

# on recalcule les prédiction en fonction de la fonction de perte 
adjusted_predictions <- list()
expected_loss <- list()


for (i in 1:length(models)) {
  positive_class <- levels(models[[i]]$pred$obs)[2]
  predicted_probs <- models[[i]]$pred[[positive_class]]
  
  true_labels <- models[[i]]$pred$obs
  
  adjusted_threshold <- cost / (1 + cost)
  
  # les prédictions ajustés
  adjusted_pred <- ifelse(predicted_probs >= adjusted_threshold, "Oui", "Non")
  adjusted_predictions[[i]] <- adjusted_pred
  
  # la matrice de confusion
  conf_matrix <- confusionMatrix(factor(adjusted_pred, levels = c("Non", "Oui")), true_labels)
  
  print(paste("Model", i, "Confusion Matrix:"))
  print(conf_matrix)
}

best_thresholds <- list()
expected_loss <- list()

for (i in 1:length(models)) {
  predictions <- models[[i]]$pred
  
  best_thresholds_cv <- list()
  expected_loss_cv <- list()
  
  # on itere sur chaque plis
  for (fold in 1:k) {
    # on prend les repetitions d un plis 
    for (rep in 1:cv_control$repeats) {
      fold_rep <- paste0("Fold", sprintf("%02d", fold), ".Rep", rep)
      
      cv_fold <- predictions %>% filter(Resample == fold_rep)
      
      # courbe ROC 
      roc_obj <- roc(
        response = cv_fold$obs,                
        predictor = cv_fold$Oui,  
        quiet = TRUE
      )
      
      # on trouve le seuil optimal 
      best_threshold <- pROC::coords(roc_obj, "best", ret = "all", transpose = FALSE,
                               best.method = "youden", best.weights = c(cost, prevalence))
      
      print(paste("Fold:", fold, "Rep:", rep))
      print(paste("FP:", best_threshold$fp))
      print(paste("FN:", best_threshold$fn))
      print(paste("Number of observations:", nrow(cv_fold)))
      
      # on garde le seuil optimal et les pertes attendus
      best_thresholds_cv[[paste0("Fold", fold, ".Rep", rep)]] <- best_threshold$threshold
      expected_loss_cv[[paste0("Fold", fold, ".Rep", rep)]] <- (best_threshold$fp * FP + best_threshold$fn * FN) / nrow(cv_fold)
    }
  }
  
  # lA MOYENNE des seuil et des pertes pour chaque modele
  best_thresholds[[paste0("Model ", i)]] <- mean(unlist(best_thresholds_cv))
  expected_loss[[paste0("Model ", i)]] <- mean(unlist(expected_loss_cv))
}


summary_models <- data.frame(
  'optimal threshold' = unlist(best_thresholds),
  'expected loss' = unlist(expected_loss),
  'ROC' = unlist(models_auc)
)

print(summary_models)


#---------------------------#
# Choix du meilleure modele #
#---------------------------#

# Step 1: ENtrainement du model 1 sur tout le working_set 
model1_full <- train(
  defaut ~ .,                     
  data = working_set,             
  method = "glm",              # Logistic Regression
)

# je predit sur le holdout avec le model choisi 
predicted_probabilities_holdout <- predict(model1_full, newdata = hold_out, type = 'prob')

# je prends la proba de tous les données oui
hold_out$best_pred <- predicted_probabilities_holdout[, 'Oui']

# ROC sur le holdout
roc_obj_holdout <- roc(hold_out$defaut, hold_out[,"best_pred",drop = TRUE], quiet = TRUE)
auc(roc_obj_holdout)
# je prends le seuil optimal trouvé precedemment
best_threshold <- best_thresholds[['Model 1']]  

holdout_threshold <- pROC::coords(roc_obj_holdout, x = best_threshold, input= 'threshold', 
                            ret='all', transpose = FALSE)

#je calcule les pertes attendus
expected_loss_holdout <- (holdout_threshold$fp * FP + holdout_threshold$fn * FN) / length(hold_out$defaut)
cat("Expected Loss on Holdout:", expected_loss_holdout, "\n")

# on transforme la proba en classe oui ou non selon le seui optimale
holdout_prediction <- ifelse(hold_out$best_pred < best_threshold, 'Non', 'Oui') %>%
  factor(levels = c('Non', 'Oui'))

# Matrice de confusion
cm_object_holdout <- confusionMatrix(holdout_prediction, hold_out$defaut)
cm_table <- cm_object_holdout$table

cat("Confusion Matrix:\n")
print(cm_table)

cm_pctg <- round(cm_table / sum(cm_table) * 100, 1)
cat("Confusion Matrix in Percentages:\n")
print(cm_pctg)

TP <- cm_table['Oui', 'Oui']   # True Positives
FN <- cm_table['Oui', 'Non'] # False Negatives
TN <- cm_table['Non', 'Non'] # True Negatives
FP <- cm_table['Non', 'Oui']   # False Positives

# Sensitivity 
sensitivity <- TP / (TP + FN)
cat("Sensitivity (True Positive Rate):", round(sensitivity, 3), "\n")

# Specificity 
specificity <- TN / (TN + FP)
cat("Specificity (True Negative Rate):", round(specificity, 3), "\n")

#--------------------------------#
# le projet avec knn imputation  #
#--------------------------------#
data <- read.csv("projet.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = T)
data_new <- read.csv("projet_new.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = T)

str(data)
ls = data.frame(unclass(summary(data)), check.names = FALSE, stringsAsFactors = FALSE)

# knn imputation
#install.packages("VIM")
library("VIM")

data <- read.csv("projet.csv",header = TRUE, sep = ",", dec = ".", stringsAsFactors = T, na.strings = "999")

original_mean <- colMeans(data[c("age", "revenus", "debcred", "debcarte", "autres")], na.rm = TRUE)
original_sd <- apply(data[c("age", "revenus", "debcred", "debcarte", "autres")], 2, sd, na.rm = TRUE)

data_scaled <- data
data_scaled[c("age", "revenus", "debcred", "debcarte", "autres")] <- scale(data[c("age", "revenus", "debcred", "debcarte", "autres")])
summary(data_scaled)
data_imputed <- kNN(data_scaled, variable = c("age", "adresse"), k = round(sqrt(nrow(data))))  
data_imputed[c("age", "revenus", "debcred", "debcarte", "autres")] <- sweep(
  sweep(data_imputed[c("age", "revenus", "debcred", "debcarte", "autres")], 2, original_sd, "*"), 
  2, original_mean, "+" )
summary(data_imputed)
ls1 = data.frame(unclass(summary(data_imputed)), check.names = FALSE, stringsAsFactors = FALSE)


data$defaut <- as.factor(data$defaut)
summary(data) 

data_imputed <- subset(data_imputed, select = c(-client,-categorie,-age_imp,-adresse_imp))
data_imputed$education <- as.numeric(as.factor(data_imputed$education))

table(data_imputed$revenus[data_imputed$revenus >200])
data_imputed <- data_imputed[data_imputed$revenus < 550,]

ls2 = data.frame(unclass(summary(data_imputed)), check.names = FALSE, stringsAsFactors = FALSE)

table(data_imputed$debcarte[data_imputed$debcarte>30])

data_imputed <- data_imputed[data_imputed$debcarte<50,]


#--------------------------#
#install.packages("themis")
library(tidyverse)
library(splitstackshape)
library(sp)
library(caret)
library(randomForest)
library(pROC)
library(rpart)
library(C50)
library(tree)


set.seed(13505)
working_set <- data_imputed %>% stratified(group = "defaut", size = 0.8)
hold_out <- setdiff(data_imputed, working_set)
# je prend 80% des données pour entrainer notre modele et 20% restante pour tester
k <- 10  # Nombre de plis

# Configuration pour la validation croisée
cv_control <- trainControl(
  method = 'repeatedcv',            
  number = k,             
  summaryFunction = twoClassSummary, 
  #sampling = "smote",
  classProbs = TRUE,        
  savePredictions = "all",   
  repeats = 5 # nb of repeats 
)

model1 <- train(
  defaut ~ .,            
  data = working_set,            
  method = "glm",           # logistic regression
  trControl = cv_control   
)

# 2. Random Forest Model
model2 <- train(
  defaut ~ .,            
  data = working_set,             
  method = "rf",           # Random Forest
  trControl = cv_control, 
  importance = TRUE      
)

# random forest tuned
tune_grid_rf <- expand.grid(
  .mtry = c(4,5,6,7),
  .splitrule = 'gini',
  .min.node.size = c(5,10, 15)
)

model2.2 <- train(
  defaut ~ .,            
  data = working_set,             
  method = "ranger",           # Random Forest
  trControl =cv_control, 
  tuneGrid = tune_grid_rf
)

# 3. Support Vector Machine Model
model3 <- train(
  defaut ~ .,             
  data = working_set,             
  method = "svmRadial",    # Support Vector Machine (Radial Kernel)
  trControl = cv_control, 
  preProcess = c("center", "scale")  
)

#SVM tuned
model3.2 <- train(
  defaut ~ .,            
  data = working_set,           
  method = "svmRadial",    
  trControl = cv_control,
  tuneLength = 10,
  preProcess = c("center", "scale")  
)

# SVM linear
model3.3 <- train(
  defaut ~ .,           
  data = working_set,            
  method = "svmLinear",  # Linear 
  trControl = cv_control,
  preProcess = c("center", "scale") 
)
print(model3.3)
# 4. Decision Tree Model
model4 <- train(
  defaut ~ .,            
  data = working_set,             
  method = "rpart",        
  trControl = cv_control   
)

# Model 4.1 (Gini split, minbucket = 10)
model4.1 <- train(
  defaut ~ ., 
  data = working_set, 
  method = "rpart", 
  trControl = cv_control, 
  tuneGrid = expand.grid(cp = c(0.001, 0.01, 0.1)),  
  parms = list(split = "gini"),
  control = rpart.control(minbucket = 10) 
)


# Model 4.2 (Gini split, minbucket = 9)
model4.2 <- train(
  defaut ~ .,           
  data = working_set,      
  method = "rpart",       
  trControl = cv_control,  
  tuneGrid = expand.grid(
    cp = 0.01               
  ),
  parms = list(split = "gini"),  
  control = rpart.control(minbucket = 9)  
)

# Model 4.3 (Information split, minbucket = 5)
model4.3 <- train(
  defaut ~ .,             
  data = working_set,   
  method = "rpart",        
  trControl = cv_control,  
  tuneGrid = expand.grid(
    cp = 0.01             
  ),
  parms = list(split = "information"),
  control = rpart.control(minbucket = 5)  
)

# Model 4.4 (Information split, minbucket = 9)
model4.4 <- train(
  defaut ~ .,            
  data = working_set,      
  method = "rpart",      
  trControl = cv_control, 
  tuneGrid = expand.grid(
    cp = 0.01               
  ),
  parms = list(split = "information"),  
  control = rpart.control(minbucket = 9)  
)

# 5. K-Nearest Neighbors Model
model5 <- train(
  defaut ~ .,             
  data = working_set,             
  method = "knn",          
  trControl = cv_control,  
  tuneLength = c(4,5,6,7,8,9,10)          
)

model5.1 <- train(
  defaut ~ .,         
  data = working_set,         
  method = "knn",          
  metric = "ROC",
  preProc = c("center","scale"),
  trControl = cv_control,  
  tuneLength = c(2,4,8,16,20,24,25,30,35,40)       
)


# Differentes valeurs lambda pour la regularisation de la methode LASSO 
lambda <- 10^seq(-1, -4, length = 10)  
grid <- expand.grid(alpha = 1, lambda = lambda)  

#  LASSO (Logit + LASSO)
set.seed(13505)
model6 <- train(
  defaut ~ .,                    
  data = working_set,         
  method = "glmnet",              # LASSO regression
  preProcess = c("center", "scale"),
  family = "binomial",           
  trControl = cv_control,         
  tuneGrid = grid                
)

# On sauvegarde le meilleure modele Lasso
best_lambda <- model6$bestTune$lambda
lasso_coeffs <- as.matrix(coef(model6$finalModel, s = best_lambda))  

# Model C50
# Model 7 (C5.0, minCases = 9, noGlobalPruning = FALSE)
model7 <- train(
  defaut ~ .,             
  data = working_set,     
  method = "C5.0",        
  trControl = cv_control, 
  tuneGrid = expand.grid(
    trials = 50,          
    model = "tree",      
    winnow = c(TRUE, FALSE) 
  ),
  control = C5.0Control(minCases = 9, noGlobalPruning = FALSE)  
)

# Model 7.1 (C5.0, minCases = 9, noGlobalPruning = TRUE)
model7.1 <- train(
  defaut ~ .,             
  data = working_set,     
  method = "C5.0",        
  trControl = cv_control, 
  tuneGrid = expand.grid(
    trials = 50,           
    model = "tree",       
    winnow = c(TRUE, FALSE) 
  ),
  control = C5.0Control(minCases = 9, noGlobalPruning = TRUE)  
)

# Model 7.2 (C5.0, minCases = 4, noGlobalPruning = FALSE)
model7.2 <- train(
  defaut ~ .,             
  data = working_set,    
  method = "C5.0",        
  trControl = cv_control,
  tuneGrid = expand.grid(
    trials = 50,           
    model = "tree",        
    winnow = c(TRUE, FALSE) 
  ),
  control = C5.0Control(minCases = 4, noGlobalPruning = FALSE)  
)

# Model 7.3 (C5.0, minCases = 4, noGlobalPruning = TRUE)
model7.3 <- train(
  defaut ~ .,             
  data = working_set,   
  method = "C5.0",       
  trControl = cv_control, 
  tuneGrid = expand.grid(
    trials = 50,          
    model = "tree",        
    winnow = c(TRUE, FALSE) 
  ),
  control = C5.0Control(minCases = 4, noGlobalPruning = TRUE)  
)


# Model 8 (Tree, split = "gini", mincut = 9)
model8 <- train(
  defaut ~ .,             
  data = working_set,    
  method = "rpart",       
  trControl = cv_control, 
  tuneGrid = expand.grid(
    cp = 0.01              
  ),
  control = rpart.control(
    minbucket = 9,         
    cp = 0.01,            
    split = "gini"         
  ) 
)


# Model 8.1 (Tree, split = "gini", mincut = 4)
model8.1 <- train(
  defaut ~ .,             
  data = working_set,     
  method = "rpart",       
  trControl = cv_control, 
  tuneGrid = expand.grid(
    cp = 0.01             
  ),
  control = rpart.control(minbucket = 4,         
                          cp = 0.01,             
                          split = "gini")  
)

models <- list(model1, 
               model2,model2.2, 
               model3,model3.2, 
               model4,model4.1,model4.2,model4.3,model4.4, 
               model5,model5.1,
               model6,
               model7,model7.1,model7.2,model7.3,
               model8,model8.1)

#--------------------#
# AUC ROC SENS SPEC
#--------------------#
models_auc <- vector("numeric", length = length(models))

# Calcule de l' AUC moyen
for (i in 1:length(models)) {
  auc_values <- models[[i]]$resample$ROC  # AUC pour chaque plis
  
  # la moyenne sur tous les plis et les répetitions
  models_auc[i] <- mean(auc_values, na.rm = TRUE)
}


for (i in 1:length(models)) {
  print(models[[i]])
}

cv_results <- list()  

roc_curves <- list()


for (i in 1:length(models)) {
  
  predictions <- models[[i]]$pred
  
  positive_class <- levels(predictions$obs)[2]  
  
  roc_curves[[i]] <- roc(
    response = predictions$obs,               
    predictor = predictions[[positive_class]], 
    levels = rev(levels(predictions$obs))       
  )
}

#plot(roc_curves[[17]], col = 1, main = "Courbe ROC pour tous les modèles", lwd = 2)
#plot(roc_curves[[2]], col = 1,add=TRUE, main = "Courbe ROC pour tous les modèles", lwd = 2)
plot(roc_curves[[1]], col = 1, main = "Courbe ROC pour tous les modèles", lwd = 2)
for (i in 2:length(roc_curves)) {
  plot(roc_curves[[i]], col = i, add = TRUE, lwd = 2)
}

legend("bottomright", legend = paste0("Modèle ", 1:length(models)),
       col = 1:length(models), lwd = 2)


#---------------# 
# loss function #
#---------------#

FP <- 1   
FN <- 3  
cost <- FN / FP  


prevalence <- sum(as.numeric(working_set$defaut) - 1) / length(working_set$defaut)

adjusted_predictions <- list()
expected_loss <- list()


for (i in 1:length(models)) {
  positive_class <- levels(models[[i]]$pred$obs)[2]
  predicted_probs <- models[[i]]$pred[[positive_class]]
  
  true_labels <- models[[i]]$pred$obs
  
  adjusted_threshold <- cost / (1 + cost)
  
  adjusted_pred <- ifelse(predicted_probs >= adjusted_threshold, "Oui", "Non")
  adjusted_predictions[[i]] <- adjusted_pred
  
  conf_matrix <- confusionMatrix(factor(adjusted_pred, levels = c("Non", "Oui")), true_labels)
  
  print(paste("Model", i, "Confusion Matrix:"))
  print(conf_matrix)
}

best_thresholds <- list()
expected_loss <- list()

for (i in 1:length(models)) {
  predictions <- models[[i]]$pred
  
  best_thresholds_cv <- list()
  expected_loss_cv <- list()
  
  for (fold in 1:k) {
    for (rep in 1:cv_control$repeats) {
      fold_rep <- paste0("Fold", sprintf("%02d", fold), ".Rep", rep)
      
      cv_fold <- predictions %>% filter(Resample == fold_rep)
      
      roc_obj <- roc(
        response = cv_fold$obs,                
        predictor = cv_fold$Oui,   
        quiet = TRUE
      )
      
      best_threshold <- pROC::coords(roc_obj, 'best', ret = 'all', transpose = FALSE,
                                     best.method = 'youden', best.weights = c(cost, prevalence))
      
      print(paste("Fold:", fold, "Rep:", rep))
      print(paste("FP:", best_threshold$fp))
      print(paste("FN:", best_threshold$fn))
      print(paste("Number of observations:", nrow(cv_fold)))
      
      best_thresholds_cv[[paste0("Fold", fold, ".Rep", rep)]] <- best_threshold$threshold
      expected_loss_cv[[paste0("Fold", fold, ".Rep", rep)]] <- (best_threshold$fp * FP + best_threshold$fn * FN) / nrow(cv_fold)
    }
  }
  
  best_thresholds[[paste0("Model ", i)]] <- mean(unlist(best_thresholds_cv))
  expected_loss[[paste0("Model ", i)]] <- mean(unlist(expected_loss_cv))
}


summary_models <- data.frame(
  'optimal threshold' = unlist(best_thresholds),
  'expected loss' = unlist(expected_loss),
  'ROC' = unlist(models_auc)
)


print(summary_models)

#-------------------------------#
# Choix du meilleure modele     #
#-------------------------------#
model1_full <- train(
  defaut ~ .,                     
  data = working_set,             
  method = "glm",              # Logistic Regression
)

#model_full <- train(
#  defaut ~ .,            
#  data = working_set,             
#  method = "rf",           # Random Forest
#  importance = TRUE      
#)


# je predit sur le holdout avec le model choisi 
predicted_probabilities_holdout <- predict(model1_full, newdata = hold_out, type = 'prob')

# je prends la proba de tous les données oui
hold_out$best_pred <- predicted_probabilities_holdout[, 'Oui']

# ROC sur le holdout
roc_obj_holdout <- roc(hold_out$defaut, hold_out[,"best_pred",drop = TRUE], quiet = TRUE)
auc(roc_obj_holdout)
# je prends le seuil optimal trouvé precedemment
best_threshold <- best_thresholds[['Model 1']]  

holdout_threshold <- pROC::coords(roc_obj_holdout, x = best_threshold, input= 'threshold', 
                                  ret='all', transpose = FALSE)

#je calcule les pertes attendus
expected_loss_holdout <- (holdout_threshold$fp * FP + holdout_threshold$fn * FN) / length(hold_out$defaut)
cat("Expected Loss on Holdout:", expected_loss_holdout, "\n")

# on transforme la proba en classe oui ou non selon le seui optimale
holdout_prediction <- ifelse(hold_out$best_pred < best_threshold, 'Non', 'Oui') %>%
  factor(levels = c('Non', 'Oui'))

# Matrice de confusion
cm_object_holdout <- confusionMatrix(holdout_prediction, hold_out$defaut)
cm_table <- cm_object_holdout$table

cat("Confusion Matrix:\n")
print(cm_table)

cm_pctg <- round(cm_table / sum(cm_table) * 100, 1)
cat("Confusion Matrix in Percentages:\n")
print(cm_pctg)

TP <- cm_table['Oui', 'Oui']   # True Positives
FN <- cm_table['Oui', 'Non'] # False Negatives
TN <- cm_table['Non', 'Non'] # True Negatives
FP <- cm_table['Non', 'Oui']   # False Positives

# Sensitivity 
sensitivity <- TP / (TP + FN)
cat("Sensitivity (True Positive Rate):", round(sensitivity, 3), "\n")

# Specificity 
specificity <- TN / (TN + FP)
cat("Specificity (True Negative Rate):", round(specificity, 3), "\n")

#--------------------------#
# Projet avec knn et smote #
#--------------------------#

data <- read.csv("projet.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = T)
data_new <- read.csv("projet_new.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = T)

str(data)
ls = data.frame(unclass(summary(data)), check.names = FALSE, stringsAsFactors = FALSE)

# knn imputation
#install.packages("VIM")
library("VIM")

data <- read.csv("projet.csv",header = TRUE, sep = ",", dec = ".", stringsAsFactors = T, na.strings = "999")

original_mean <- colMeans(data[c("age", "revenus", "debcred", "debcarte", "autres")], na.rm = TRUE)
original_sd <- apply(data[c("age", "revenus", "debcred", "debcarte", "autres")], 2, sd, na.rm = TRUE)

data_scaled <- data
data_scaled[c("age", "revenus", "debcred", "debcarte", "autres")] <- scale(data[c("age", "revenus", "debcred", "debcarte", "autres")])
summary(data_scaled)
data_imputed <- kNN(data_scaled, variable = c("age", "adresse"), k = round(sqrt(nrow(data))))  
data_imputed[c("age", "revenus", "debcred", "debcarte", "autres")] <- sweep(
  sweep(data_imputed[c("age", "revenus", "debcred", "debcarte", "autres")], 2, original_sd, "*"), 
  2, original_mean, "+" )
summary(data_imputed)
ls1 = data.frame(unclass(summary(data_imputed)), check.names = FALSE, stringsAsFactors = FALSE)


data$defaut <- as.factor(data$defaut)
summary(data) 

data_imputed <- subset(data_imputed, select = c(-client,-categorie,-age_imp,-adresse_imp))
data_imputed$education <- as.numeric(as.factor(data_imputed$education))

table(data_imputed$revenus[data_imputed$revenus >200])
data_imputed <- data_imputed[data_imputed$revenus < 550,]

ls2 = data.frame(unclass(summary(data_imputed)), check.names = FALSE, stringsAsFactors = FALSE)

table(data_imputed$debcarte[data_imputed$debcarte>30])

data_imputed <- data_imputed[data_imputed$debcarte<50,]


#--------------------------#
#install.packages("themis")
library(tidyverse)
library(splitstackshape)
library(sp)
library(caret)
library(randomForest)
library(pROC)
library(rpart)
library(C50)
library(tree)
library(themis)

set.seed(13505)
working_set <- data_imputed %>% stratified(group = "defaut", size = 0.8)
hold_out <- setdiff(data_imputed, working_set)
# je prend 80% des données pour entrainer notre modele et 20% restante pour tester
k <- 10  # Nombre de plis

# Configuration pour la validation croisée
cv_control <- trainControl(
  method = 'repeatedcv',            
  number = k,             
  summaryFunction = twoClassSummary, 
  sampling = "smote",
  classProbs = TRUE,        
  savePredictions = "all",   
  repeats = 5 # nb of repeats 
)
model1 <- train(
  defaut ~ .,            
  data = working_set,            
  method = "glm",           # logistic regression
  trControl = cv_control   
)

# 2. Random Forest Model
model2 <- train(
  defaut ~ .,            
  data = working_set,             
  method = "rf",           # Random Forest
  trControl = cv_control, 
  importance = TRUE      
)

# random forest tuned
tune_grid_rf <- expand.grid(
  .mtry = c(4,5,6,7),
  .splitrule = 'gini',
  .min.node.size = c(5,10, 15)
)

model2.2 <- train(
  defaut ~ .,            
  data = working_set,             
  method = "ranger",           # Random Forest
  trControl =cv_control, 
  tuneGrid = tune_grid_rf
)

# 3. Support Vector Machine Model
model3 <- train(
  defaut ~ .,             
  data = working_set,             
  method = "svmRadial",    # Support Vector Machine (Radial Kernel)
  trControl = cv_control, 
  preProcess = c("center", "scale")  
)

#SVM tuned
model3.2 <- train(
  defaut ~ .,            
  data = working_set,           
  method = "svmRadial",    
  trControl = cv_control,
  tuneLength = 10,
  preProcess = c("center", "scale")  
)

# SVM linear
model3.3 <- train(
  defaut ~ .,           
  data = working_set,            
  method = "svmLinear",  # Linear 
  trControl = cv_control,
  preProcess = c("center", "scale") 
)
print(model3.3)
# 4. Decision Tree Model
model4 <- train(
  defaut ~ .,            
  data = working_set,             
  method = "rpart",        
  trControl = cv_control   
)

# Model 4.1 (Gini split, minbucket = 10)
model4.1 <- train(
  defaut ~ ., 
  data = working_set, 
  method = "rpart", 
  trControl = cv_control, 
  tuneGrid = expand.grid(cp = c(0.001, 0.01, 0.1)),  
  parms = list(split = "gini"),
  control = rpart.control(minbucket = 10) 
)


# Model 4.2 (Gini split, minbucket = 9)
model4.2 <- train(
  defaut ~ .,           
  data = working_set,      
  method = "rpart",       
  trControl = cv_control,  
  tuneGrid = expand.grid(
    cp = 0.01               
  ),
  parms = list(split = "gini"),  
  control = rpart.control(minbucket = 9)  
)

# Model 4.3 (Information split, minbucket = 5)
model4.3 <- train(
  defaut ~ .,             
  data = working_set,   
  method = "rpart",        
  trControl = cv_control,  
  tuneGrid = expand.grid(
    cp = 0.01             
  ),
  parms = list(split = "information"),
  control = rpart.control(minbucket = 5)  
)

# Model 4.4 (Information split, minbucket = 9)
model4.4 <- train(
  defaut ~ .,            
  data = working_set,      
  method = "rpart",      
  trControl = cv_control, 
  tuneGrid = expand.grid(
    cp = 0.01               
  ),
  parms = list(split = "information"),  
  control = rpart.control(minbucket = 9)  
)

# 5. K-Nearest Neighbors Model
model5 <- train(
  defaut ~ .,             
  data = working_set,             
  method = "knn",          
  trControl = cv_control,  
  tuneLength = c(4,5,6,7,8,9,10)          
)

model5.1 <- train(
  defaut ~ .,         
  data = working_set,         
  method = "knn",          
  metric = "ROC",
  preProc = c("center","scale"),
  trControl = cv_control,  
  tuneLength = c(2,4,8,16,20,24,25,30,35,40)       
)


# Differentes valeurs lambda pour la regularisation de la methode LASSO 
lambda <- 10^seq(-1, -4, length = 10)  
grid <- expand.grid(alpha = 1, lambda = lambda)  

#  LASSO (Logit + LASSO)
set.seed(13505)
model6 <- train(
  defaut ~ .,                    
  data = working_set,         
  method = "glmnet",              # LASSO regression
  preProcess = c("center", "scale"),
  family = "binomial",           
  trControl = cv_control,         
  tuneGrid = grid                
)

# On sauvegarde le meilleure modele Lasso
best_lambda <- model6$bestTune$lambda
lasso_coeffs <- as.matrix(coef(model6$finalModel, s = best_lambda))  

# Model C50
# Model 7 (C5.0, minCases = 9, noGlobalPruning = FALSE)
model7 <- train(
  defaut ~ .,             
  data = working_set,     
  method = "C5.0",        
  trControl = cv_control, 
  tuneGrid = expand.grid(
    trials = 50,          
    model = "tree",      
    winnow = c(TRUE, FALSE) 
  ),
  control = C5.0Control(minCases = 9, noGlobalPruning = FALSE)  
)

# Model 7.1 (C5.0, minCases = 9, noGlobalPruning = TRUE)
model7.1 <- train(
  defaut ~ .,             
  data = working_set,     
  method = "C5.0",        
  trControl = cv_control, 
  tuneGrid = expand.grid(
    trials = 50,           
    model = "tree",       
    winnow = c(TRUE, FALSE) 
  ),
  control = C5.0Control(minCases = 9, noGlobalPruning = TRUE)  
)

# Model 7.2 (C5.0, minCases = 4, noGlobalPruning = FALSE)
model7.2 <- train(
  defaut ~ .,             
  data = working_set,    
  method = "C5.0",        
  trControl = cv_control,
  tuneGrid = expand.grid(
    trials = 50,           
    model = "tree",        
    winnow = c(TRUE, FALSE) 
  ),
  control = C5.0Control(minCases = 4, noGlobalPruning = FALSE)  
)

# Model 7.3 (C5.0, minCases = 4, noGlobalPruning = TRUE)
model7.3 <- train(
  defaut ~ .,             
  data = working_set,   
  method = "C5.0",       
  trControl = cv_control, 
  tuneGrid = expand.grid(
    trials = 50,          
    model = "tree",        
    winnow = c(TRUE, FALSE) 
  ),
  control = C5.0Control(minCases = 4, noGlobalPruning = TRUE)  
)


# Model 8 (Tree, split = "gini", mincut = 9)
model8 <- train(
  defaut ~ .,             
  data = working_set,    
  method = "rpart",       
  trControl = cv_control, 
  tuneGrid = expand.grid(
    cp = 0.01              
  ),
  control = rpart.control(
    minbucket = 9,         
    cp = 0.01,            
    split = "gini"         
  ) 
)


# Model 8.1 (Tree, split = "gini", mincut = 4)
model8.1 <- train(
  defaut ~ .,             
  data = working_set,     
  method = "rpart",       
  trControl = cv_control, 
  tuneGrid = expand.grid(
    cp = 0.01             
  ),
  control = rpart.control(minbucket = 4,         
                          cp = 0.01,             
                          split = "gini")  
)

models <- list(model1, 
               model2,model2.2, 
               model3,model3.2, 
               model4,model4.1,model4.2,model4.3,model4.4, 
               model5,model5.1,
               model6,
               model7,model7.1,model7.2,model7.3,
               model8,model8.1)

#--------------------#
# AUC ROC SENS SPEC
#--------------------#
models_auc <- vector("numeric", length = length(models))

# Calcule de l' AUC moyen
for (i in 1:length(models)) {
  auc_values <- models[[i]]$resample$ROC  # AUC pour chaque plis
  
  # la moyenne sur tous les plis et les répetitions
  models_auc[i] <- mean(auc_values, na.rm = TRUE)
}


for (i in 1:length(models)) {
  print(models[[i]])
}

cv_results <- list() 

roc_curves <- list()


for (i in 1:length(models)) {
  
  predictions <- models[[i]]$pred
  
  positive_class <- levels(predictions$obs)[2]  
  
  roc_curves[[i]] <- roc(
    response = predictions$obs,               
    predictor = predictions[[positive_class]], 
    levels = rev(levels(predictions$obs))       
  )
}

#plot(roc_curves[[17]], col = 1, main = "Courbe ROC pour tous les modèles", lwd = 2)
#plot(roc_curves[[2]], col = 1,add=TRUE, main = "Courbe ROC pour tous les modèles", lwd = 2)
plot(roc_curves[[13]])
for (i in 2:length(roc_curves)) {
  plot(roc_curves[[i]], col = i, add = TRUE, lwd = 2)
}

legend("bottomright", legend = paste0("Modèle ", 1:length(models)),
       col = 1:length(models), lwd = 2)


#---------------# 
# loss function #
#---------------#

FP <- 1   
FN <- 3  
cost <- FN / FP  


prevalence <- sum(as.numeric(working_set$defaut) - 1) / length(working_set$defaut)

adjusted_predictions <- list()
expected_loss <- list()


for (i in 1:length(models)) {
  positive_class <- levels(models[[i]]$pred$obs)[2]
  predicted_probs <- models[[i]]$pred[[positive_class]]
  
  true_labels <- models[[i]]$pred$obs
  
  adjusted_threshold <- cost / (1 + cost)
  
  adjusted_pred <- ifelse(predicted_probs >= adjusted_threshold, "Oui", "Non")
  adjusted_predictions[[i]] <- adjusted_pred
  
  conf_matrix <- confusionMatrix(factor(adjusted_pred, levels = c("Non", "Oui")), true_labels)
  
  print(paste("Model", i, "Confusion Matrix:"))
  print(conf_matrix)
}

best_thresholds <- list()
expected_loss <- list()

for (i in 1:length(models)) {
  predictions <- models[[i]]$pred
  
  best_thresholds_cv <- list()
  expected_loss_cv <- list()
  
  for (fold in 1:k) {
    for (rep in 1:cv_control$repeats) {
      fold_rep <- paste0("Fold", sprintf("%02d", fold), ".Rep", rep)
      
      cv_fold <- predictions %>% filter(Resample == fold_rep)
      
      roc_obj <- roc(
        response = cv_fold$obs,                
        predictor = cv_fold$Oui,   
        quiet = TRUE
      )
      
      best_threshold <- pROC::coords(roc_obj, 'best', ret = 'all', transpose = FALSE,
                                     best.method = 'youden', best.weights = c(cost, prevalence))
      
      print(paste("Fold:", fold, "Rep:", rep))
      print(paste("FP:", best_threshold$fp))
      print(paste("FN:", best_threshold$fn))
      print(paste("Number of observations:", nrow(cv_fold)))
      
      best_thresholds_cv[[paste0("Fold", fold, ".Rep", rep)]] <- best_threshold$threshold
      expected_loss_cv[[paste0("Fold", fold, ".Rep", rep)]] <- (best_threshold$fp * FP + best_threshold$fn * FN) / nrow(cv_fold)
    }
  }
  
  best_thresholds[[paste0("Model ", i)]] <- mean(unlist(best_thresholds_cv))
  expected_loss[[paste0("Model ", i)]] <- mean(unlist(expected_loss_cv))
}


summary_models <- data.frame(
  'optimal threshold' = unlist(best_thresholds),
  'expected loss' = unlist(expected_loss),
  'ROC' = unlist(models_auc)
)


print(summary_models)

#-------------------------------#
# Choix du meilleure modele     #
#-------------------------------#
model1_full <- train(
  defaut ~ .,                     
  data = working_set,             
  method = "glm",              # Logistic Regression
)

#model_full <- train(
#  defaut ~ .,            
#  data = working_set,             
#  method = "rf",           # Random Forest
#  importance = TRUE      
#)


# je predit sur le holdout avec le model choisi 
predicted_probabilities_holdout <- predict(model1_full, newdata = hold_out, type = 'prob')

# je prends la proba de tous les données oui
hold_out$best_pred <- predicted_probabilities_holdout[, 'Oui']

# ROC sur le holdout
roc_obj_holdout <- roc(hold_out$defaut, hold_out[,"best_pred",drop = TRUE], quiet = TRUE)

# je prends le seuil optimal trouvé precedemment
best_threshold <- best_thresholds[['Model 1']] 

holdout_threshold <- pROC::coords(roc_obj_holdout, x = best_threshold, input= 'threshold', 
                                  ret='all', transpose = FALSE)

#je calcule les pertes attendus
expected_loss_holdout <- (holdout_threshold$fp * FP + holdout_threshold$fn * FN) / length(hold_out$defaut)
cat("Expected Loss on Holdout:", expected_loss_holdout, "\n")

# on transforme la proba en classe oui ou non selon le seui optimale
holdout_prediction <- ifelse(hold_out$best_pred < best_threshold, 'Non', 'Oui') %>%
  factor(levels = c('Non', 'Oui'))

# Matrice de confusion
cm_object_holdout <- confusionMatrix(holdout_prediction, hold_out$defaut)
cm_table <- cm_object_holdout$table

cat("Confusion Matrix:\n")
print(cm_table)

cm_pctg <- round(cm_table / sum(cm_table) * 100, 1)
cat("Confusion Matrix in Percentages:\n")
print(cm_pctg)

TP <- cm_table['Oui', 'Oui']   # True Positives
FN <- cm_table['Oui', 'Non'] # False Negatives
TN <- cm_table['Non', 'Non'] # True Negatives
FP <- cm_table['Non', 'Oui']   # False Positives

# Sensitivity 
sensitivity <- TP / (TP + FN)
cat("Sensitivity (True Positive Rate):", round(sensitivity, 3), "\n")

# Specificity 
specificity <- TN / (TN + FP)
cat("Specificity (True Negative Rate):", round(specificity, 3), "\n")
# Calculer l'AUC
auc_value <- auc(roc_obj_holdout)

# Afficher l'AUC
cat("AUC:", auc_value, "\n")

#----------
# pour un seuil de 0.5

holdout_threshold <- pROC::coords(roc_obj_holdout, x = 0.5, input= 'threshold', 
                                  ret='all', transpose = FALSE)

#je calcule les pertes attendus
expected_loss_holdout <- (holdout_threshold$fp * FP + holdout_threshold$fn * FN) / length(hold_out$defaut)
cat("Expected Loss on Holdout:", expected_loss_holdout, "\n")

# on transforme la proba en classe oui ou non selon le seui optimale
holdout_prediction <- ifelse(hold_out$best_pred < 0.5, 'Non', 'Oui') %>%
  factor(levels = c('Non', 'Oui'))

# Matrice de confusion
cm_object_holdout <- confusionMatrix(holdout_prediction, hold_out$defaut)
cm_table <- cm_object_holdout$table

cat("Confusion Matrix:\n")
print(cm_table)

cm_pctg <- round(cm_table / sum(cm_table) * 100, 1)
cat("Confusion Matrix in Percentages:\n")
print(cm_pctg)

TP <- cm_table['Oui', 'Oui']   # True Positives
FN <- cm_table['Oui', 'Non'] # False Negatives
TN <- cm_table['Non', 'Non'] # True Negatives
FP <- cm_table['Non', 'Oui']   # False Positives

# Sensitivity 
sensitivity <- TP / (TP + FN)
cat("Sensitivity (True Positive Rate):", round(sensitivity, 3), "\n")

# Specificity 
specificity <- TN / (TN + FP)
cat("Specificity (True Negative Rate):", round(specificity, 3), "\n")

# Calculer l'AUC
auc_value <- auc(roc_obj_holdout)

# Afficher l'AUC
cat("AUC:", auc_value, "\n")

#---------------------------#
#Prédiction sur le data_new #
#---------------------------#
data_new <- read.csv("projet_new.csv",header = TRUE, sep = ",", dec = ".", stringsAsFactors = T, na.strings = "999")
data_new_client <- read.csv("projet_new.csv",header = TRUE, sep = ",", dec = ".", stringsAsFactors = T, na.strings = "999")
sum(is.na(data_new))
summary(data_new)
data_new <- subset(data_new,select = c(-client,-categorie))
data_new$education <- as.numeric(as.factor(data_new$education))

ls3 = data.frame(unclass(summary(data_new)), check.names = FALSE, stringsAsFactors = FALSE)

# imputation
original_mean_new <- colMeans(data_new[c("age", "adresse")], na.rm = TRUE)
original_sd_new <- apply(data_new[c("age", "adresse")], 2, sd, na.rm = TRUE)

data_scaled_new <- data_new
data_scaled_new[c("age", "adresse")] <- scale(data_new[c("age", "adresse")])
summary(data_scaled_new)
data_imputed_new <- kNN(data_scaled_new, variable = c("age", "adresse"), k = round(sqrt(nrow(data))))  
data_imputed_new[c("age", "adresse")] <- sweep(
  sweep(data_imputed_new[c("age", "adresse")], 2, original_sd_new, "*"), 
  2, original_mean_new, "+" )
summary(data_imputed_new)
nrow(data_imputed_new)
ls4 = data.frame(unclass(summary(data_imputed_new)), check.names = FALSE, stringsAsFactors = FALSE)

#entrainement du modele
training_data <- data_imputed

model <- train(
  defaut ~ .,                     
  data = training_data,             
  method = "glm",              # Logistic Regression
)

# prediction 
predicted_probabilities_new <- predict(model, newdata = data_imputed_new, type = 'prob') 

data_new$best_pred <- predicted_probabilities_new[, 'Oui']
best_threshold <- best_thresholds[['Model 1']]  
# on transforme la proba en classe oui ou non selon le seui optimale
data_new_prediction <- ifelse(data_new$best_pred < best_threshold, 'Non', 'Oui') %>%
  factor(levels = c('Non', 'Oui'))

# creation d un fichier csv 
output_data <- data.frame(
  client = data_new_client$client,          
  best_pred = data_new$best_pred,        
  prediction = data_new_prediction        
)

write.csv(output_data, "predictions.csv", row.names = FALSE)



