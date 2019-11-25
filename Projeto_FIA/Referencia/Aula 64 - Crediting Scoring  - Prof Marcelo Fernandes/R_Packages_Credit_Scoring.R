
# Instalar alguns pacotes selecionados para as aulas de Credit Scoring
R_packages <- c(
  "randomForest", "neuralnet","tidyverse","psych", "readxl","Information", "arules",
  "smbinning", "cluster", "flexclust","dgof", "descr", "pROC","ROCR","neuralnet",
  "scorecard","corrplot","ggplot2","HH", "Amelia","anchors", "sjPlot")
install.packages(R_packages)

# Carregar todos os pacotes na sequencia
lapply(R_packages, library, character.only = TRUE)
