
# 1. Définition du répertoire de travail
#_______________________________________

  getwd()
  setwd("C:\\Users\\dell\\Downloads\\TP ADD - AFC")
  getwd()
  
# 2. Chargement des packages nécessaires
#_______________________________________
  
  # Chargement des librairies
  # Mais d'abord télécharger les packages s'ils ne sont pas encore installés
  library(readxl)       # Permet d'importer les données
  library(tidyverse)    # Permet de manipuler les matrices
  library(FactoMineR)   # Permet de faire l'analyse factorielle
  library(factoextra)   # Permet de les représentations de l'Analyse factorielle
  library(gplots)       # Permet une représentation graphique du tableau de contingence
  library(corrplot)
  
# 3. Exportation de la base de données
#_____________________________________
  
  Data_CA <- read_excel("Data_CA.xlsx",
              sheet = "Data",
              range = NULL,
              col_names = TRUE,
              col_types = NULL,
              na = "")
  Data_CA  <- Data_CA %>% remove_rownames %>% column_to_rownames(var="Région")
  View(Data_CA)
  # La base contient deux variables
  # La variable "Région" admet 14 modalités
  # La variable "Age x Sexe" admet 34 modalités
  
  # Une visualisation graphique du tableau de contingence
  data <- as.table(as.matrix (Data_CA))  #pour les besoins de la fonction balloonplot
  balloonplot(t (data), main = "Demography", xlab = "", ylab = "",
              label = FALSE, show.margins = FALSE)
  
# 3. Statistiques descriptives classiques
#________________________________________
  
  # Le mode de la variable "Région"
  mode1 <- Data_CA %>% rowwise %>% mutate(sum_reg=sum(c_across(1:34)))
  mode2 <- Data_CA %>% mutate(sum_cl=sum(c_across(1:13)))
  
  # Le mode de la variable "Classe d'âge x Sexe"
  
  # La statistique du Chi-2
  X <- chisq.test(Data_CA)
  X
  
  # Le tableau de contingence de référence
  View(X$expected)
  
# 4. L'analyse factorielle des coresspondances
#_____________________________________________
  
  # 4.1 Mise en oeuvre de l'AFC
  #----------------------------
  CA_result <- CA (Data_CA, graph = FALSE)
  
  # 4.2 Analyse des valeurs propres
  #--------------------------------
  eig.value <- CA_result$eig
  eig.value = as.data.frame(eig.value)
  eig.value$dimension = seq.int(from=1, to=13, by=1)
  View(eig.value)
  ggplot(eig.value, aes(x=dimension, y=`percentage of variance`)) +
        geom_bar(fill="#FFA500",stat = "identity") +
        geom_point() +
        geom_line() + 
        scale_x_discrete(breaks=seq.int(from=1, to=10, by=1)) +
        ggtitle("Diagramme des valeurs propres") +
        xlab("Dimensions") +
        ylab("Valeurs propres")
  
  # 4.3 Analyse du nuage de la variable "Région"
  #---------------------------------------------
  row <- get_ca_row(CA_result)
  
  # Nuage des modalités
  fviz_ca_row(CA_result,repel = TRUE,axes = c(1, 2))
  
  # Qualite de representation des modalités
  corrplot(row$cos2[,1:4], is.corr=FALSE)
  fviz_cos2(CA_result, choice = "row", axes = 1:2)
  fviz_ca_row(CA_result, col.row = "cos2", gradient.rows = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE) 
  
  #Contribution des modalités a la formation des axes
  corrplot(row$contrib[,1:4], is.corr=FALSE)
  fviz_contrib(CA_result, choice = "row", axes = 1:2)
  fviz_ca_row(CA_result, col.row = "contrib",
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)
  
  # 4.4 Analyse du nuage de la variable "Age x Sexe"
  #-------------------------------------------------
  col <- get_ca_col(CA_result)
  
  # Nuage des modalités
  fviz_ca_col(CA_result,repel = TRUE,axes = c(1, 2))
  
  # Qualite de representation des modalités
  corrplot(col$cos2[,1:4], is.corr=FALSE)
  fviz_cos2(CA_result, choice = "col", axes = 1:2)
  fviz_ca_col(CA_result, col.col = "cos2", gradient.rows = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE) 
  
  #Contribution des modalités a la formation des axes
  corrplot(col$contrib[,1:4], is.corr=FALSE)
  fviz_contrib(CA_result, choice = "col", axes = 1:2)
  fviz_ca_col(CA_result, col.col = "contrib",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)

  # 4.5 Analyse des deux nuages
  #----------------------------
  fviz_ca_biplot(CA_result, repel = TRUE)
         
# 5. Mise en élèment supplémentaire
#__________________________________
  # Modalité mis en supplémentaire : "Dakar"
  CA_result_supp <- CA (Data_CA, row.sup = 1, graph = FALSE)
  fviz_ca_biplot(CA_result_supp, repel = TRUE)
  
 