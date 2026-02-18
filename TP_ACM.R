
# 1. Définition du répertoire de travail
#_______________________________________

  getwd()
  setwd("C:\\Users\\dell\\Downloads\\TP ADD - ACM")
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
  
  Data_MCA <- read_excel("Data_MCA.xlsx",
              sheet = "Sheet1",
              range = NULL,
              col_names = TRUE,
              col_types = NULL,
              na = "")
  Data_MCA  <- Data_MCA %>% remove_rownames %>% column_to_rownames(var="caseid")
  View(Data_MCA)
  # La base contient 17 variables et 51 individu
  
# 3. L'analyse des correspondances multiples
#___________________________________________
  
  Data_MCA <- Data_MCA[1:50, 2:17]
  
  # 3.1 Mise en oeuvre de l'ACM
  #----------------------------
  MCA_result <- MCA(Data_MCA, graph = FALSE)
  
  # 3.2 Analyse des valeurs propres
  #--------------------------------
  eig.value <- MCA_result$eig
  eig.value = as.data.frame(eig.value)
  eig.value$dimension = seq.int(from=1, to=49, by=1)
  View(eig.value)
  ggplot(eig.value, aes(x=dimension, y=`percentage of variance`)) +
        geom_bar(fill="#FFA500",stat = "identity") +
        geom_point() +
        geom_line() + 
        scale_x_discrete(breaks=seq.int(from=1, to=10, by=1)) +
        ggtitle("Diagramme des valeurs propres") +
        xlab("Dimensions") +
        ylab("Valeurs propres")
  
  # 3.3 Analyse du nuage des individus
  #-----------------------------------
    ind <- get_mca_ind(MCA_result)

  # Nuage des individus
  fviz_mca_ind(MCA_result,repel = TRUE,axes = c(1, 2))
  
  #Contribution des individus a la formation des axes
  corrplot(ind$contrib[,1:4], is.corr=FALSE)
  fviz_contrib(MCA_result, choice = "ind", axes = 1:2)
  fviz_mca_ind(MCA_result, col.ind = "contrib",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)
  
  # Qualite de representation des individus
  corrplot(ind$cos2[,1:4], is.corr=FALSE)
  fviz_cos2(MCA_result, choice = "ind", axes = 1:2)
  fviz_mca_ind(MCA_result, col.ind = "cos2", gradient.rows = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE) 
  
  # Coloration des individus par groupe
  fviz_mca_ind (MCA_result,
                label = "none",
                habillage = "pid3", 
                addEllipses = TRUE, ellipse.type = "confidence")
  
  # Quelques profils
  fviz_ellipses (MCA_result, c(5,6,11,14), geom = "point")
  
  # 3.4 Analyse du nuage des variables
  #-----------------------------------
  var <- get_mca_var(MCA_result)
  
  # Nuage des modalités
  fviz_mca_var(MCA_result,repel = TRUE,axes = c(1, 2))
  
  #Contribution des modalités a la formation des axes
  corrplot(var$contrib[,1:2], is.corr=FALSE)
  fviz_contrib(MCA_result, choice = "var", axes = 1:2)
  fviz_mca_var(MCA_result, col.var = "contrib",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)
  
  # Qualité de representation des modalités
  corrplot(var$cos2[,1:2], is.corr=FALSE)
  fviz_cos2(MCA_result, choice = "var", axes = 1:2)
  fviz_mca_var(MCA_result, col.var = "cos2", gradient.rows = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE) 
  
  # 3.5 Analyse des deux nuages
  #----------------------------
  fviz_mca_biplot(MCA_result, repel = TRUE)
 