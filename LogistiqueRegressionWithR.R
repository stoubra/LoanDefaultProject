
rm(list = ls())

#***********************************************************#***********************************#
# Exportaion des données

data <- read.table("C:\\Users\\saidt\\Desktop\\Regression_logistique\\regressionLogistiqueR\\Loan_default.csv",
                   sep = ",",stringsAsFactors = T,header = T)
# Visualisation des données
str(data)

# Il y'a 127917 individu pour 16 variable  
# 9 entier, 6 chaines de caractère dont trois binomiale (no ou yes) et la variable reponse qui prent 0 ou 1(default). 

# résumer des données
summary(data)
#Aucun constan 


#***********************************************************#***********************************#
#Construction des bases de données train, test
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.7,0.3))
data_train  <- data[sample, ]
data_test   <- data[!sample, ]
nrow(data_train)
nrow(data_test)
# Données d'entraînement 
str(data_train)
summary(data_train)

#Données teste
str(data_test)
summary(data_test)


#***********************************************************#***********************************#
#modèle témoin 
modele <- glm(Default ~ .,data = data_train, family = binomial)
summary(modele)

D <- modele$null.deviance - modele$deviance
D
ddl <- modele$df.null - modele$df.residual
ddl
p_value <- pchisq(D,ddl,lower.tail = FALSE)
p_value


#***********************************************************#***********************************#
# Sélection de variables #**********************************************************************#

#pour stepAIC
library(MASS)

# #selection backward
# modele.backward_AIC <- stepAIC(modele, data = data_train, direction="backward", trace = FALSE)
# summary(modele.backward_AIC)
# #modele.backward_AIC$anova
# 
# D <- modele.backward_AIC$deviance - modele$deviance
# D
# ddl <- modele.backward_AIC$df.residual - modele$df.residual
# ddl
# p_value <- pchisq(D,ddl,lower.tail = FALSE)
# p_value
#***********************************************************#***********************************#
#selection backward - BIC

modele.backward_BIC <- stepAIC(modele, data = data_train, direction="backward", k = log(nrow(data_train)), trace = FALSE)
summary(modele.backward_BIC)
modele.backward_BIC$anova

D <- modele.backward_BIC$deviance - modele$deviance
D
ddl <- modele.backward_BIC$df.residual - modele$df.residual
ddl
p_value <- pchisq(D,ddl,lower.tail = FALSE)
p_value

#***********************************************************#***********************************#
#sélection forward - BIC
# modele.initial <- glm(Default ~ 1, data = data_train, family = binomial)
# modele.forward_BIC <- stepAIC(modele.initial, data = data_train, scope = list(lower="~1",upper=as.formula(modele)),
#                               direction="forward",k=log(nrow(data_train)),trace = FALSE)
# summary(modele.forward_BIC)
# modele.forward_BIC$coefficients
# 
# D <- modele.forward_BIC$deviance - modele$deviance
# D
# ddl <- modele.forward_BIC$df.residual - modele$df.residual
# ddl
# p_value <- pchisq(D,ddl,lower.tail = FALSE)
# p_value

#***********************************************************#***********************************#
#sélection Both
# modele.initial <- glm(Default ~ 1, data = data_train, family = binomial)
# modele.Both <- stepAIC(modele.initial, data = data_train, scope = list(lower="~1",
#                 upper=as.formula(modele)),direction="both",trace = FALSE)
# summary(modele.Both)
# modele.Both$anova
# 
# D <- modele.Both$deviance - modele$deviance
# D
# ddl <- modele.Both$df.residual - modele$df.residual
# ddl
# p_value <- pchisq(D,ddl,lower.tail = FALSE)
# p_value

#***********************************************************#***********************************#
#diagramme de fiabilité

diarammeDeFiablite <- function(modele,data,varReponce){
  #probabilités d'affectation
  scores <- predict(modele, newdata = data, type = "response")
  print(summary(scores))
  #groupes de découpage
  decoupe <- cut(scores,breaks=seq(from=0.0, to=1.0, by=0.2),include.lowest=T)
  print(table(decoupe))
  #moyenne des probabilités par groupe
  mean.scores <- tapply(scores,decoupe,mean)
  print(mean.scores)
  #proportion des positifs par groupe
  prop.pos <- apply(table(decoupe,varReponce),1,function(x){x[2]/sum(x)})
  print(prop.pos)
  #diagramme de fiabilit?
  plot(mean.scores,prop.pos,main="Diagramme de fiabilité",type="b",xlim=c(0,1),ylim=c(0,1))
  abline(a=0,b=1)
}
diarammeDeFiablite(modele.backward_BIC,data_test,data_test$Default)

#*******************************************#**********************************************#
#Test de Hosmer & Lemeshow
testHosmerLemeshow <- function(modele,data,varReponce){
  library(ResourceSelection)
  scores <- predict(modele.backward_BIC, newdata = data_test, type = "response")
  #appel de la fonction
  ResourceSelection::hoslem.test(unclass(data_test$Default)-1,scores)
}
testHosmerLemeshow(modele.backward_BIC,data_test,data_test$Default)

#******************************************#************************************************#
# courbe ROC

courbROC <- function(modele,donnees,varReponce){
  #utilisation de la librairie ROCR
  library(ROCR)
  #probabilit?s d'affectation
  scores <- predict(modele, newdata = donnees, type = "response")
  #construire un objet prediction avec les scores et la var. cible
  pred <- ROCR::prediction(scores,varReponce)
  #objet perfromance mesur?e
  graphs_roc <- ROCR::performance(pred,measure="tpr",x.measure="fpr")
  #graphique courbe ROC
  ROCR::plot(graphs_roc,xlab="taux de faux positifs",ylab="Taux de vrais positifs "
             ,main = "Courbe ROC",col="darkblue")
  abline(a=0,b=1)
  
  #calcul de l'AUC avec la librairie ROCR - objet performance
  auc_roc <- ROCR::performance(pred,measure="auc")
  print(paste("AOC = ", auc_roc@y.values))
}
courbROC(modele.backward_BIC,data_test,data_test$Default)

#******************************************#************************************************#
#Analyse des variables du modèle.

# Variable entier

#Age
modele.Age <- summary(glm(Default ~ Age,data = data_train, family = binomial))
modele.Age

exp(modele.Age$coefficients["Age","Estimate"])
#Olde ratio
# lorsque l'age augmente de une unité on a 0.98 fois plus de chance de d'avoir une défaut de payement 

#qauntile de la loi normale
u <- qnorm(0.975)
#modele.Age$coefficients["Age","Estimate"] + u*modele.Age$coefficients["Age","Std. Error"]




#********************************************************************************************************#
#Income
# Nous allons crée 4 niveaux de salaire en se basant sur les quantiles du variable Income.


niv1 <- ifelse(data_train$Income >= 48892,1,0)
niv2 <- ifelse(data_train$Income >= 82524,1,0)
niv3 <- ifelse(data_train$Income >= 116053,1,0)

summary(glm(data_train$Default ~ niv1+niv2+niv3,data = data_train,family=binomial))



#********************************************************************************************************#
#LoanAmount

modele.LoanAmount <- summary(glm(data_train$Default ~ LoanAmount ,data = data_train,family=binomial))
modele.LoanAmount
oldRatio <- exp(modele.LoanAmount$coefficients["LoanAmount","Estimate"])
oldRatio


#********************************************************************************************************#
# Variables qualitatif binomial 

old.Ratio <- function(coefficients,variables){
  print(paste("Old ratio : ", exp(coefficients[variables,"Estimate"]) ))
  print(paste("intervalle de confiance : ", exp(coefficients[variables,"Estimate"] - 
                                                  u*coefficients[variables,"Std. Error"]),
              exp(coefficients[variables,"Estimate"] + 
                    u*coefficients[variables,"Std. Error"])) )
}

# HasMortgage
modele.HasMortgage <- summary(glm(Default ~ HasMortgage,data = data_train, family = binomial))
#modele.HasMortgage

old.Ratio(modele.HasMortgage$coefficients,"HasMortgageYes")

#********************************************************************************************************#
# HasDependents
modele.HasDependents <- summary(glm(Default ~ HasDependents,data = data_train, family = binomial))
#modele.HasDependents

old.Ratio(modele.HasDependents$coefficients,"HasDependentsYes")

#********************************************************************************************************#
# HasDependents & HasMortgage
modele.HasDependentsHasMortgage <- summary(glm(Default ~ HasDependents*HasMortgage,data = data_train, family = binomial))
modele.HasDependentsHasMortgage

#summary(glm(Default ~ HasDependents*HasMortgage*HasCoSigner,data = data_train, family = binomial))

# pour HasMortgage

old.Ratio(modele.HasDependentsHasMortgage$coefficients,"HasMortgageYes")

# pour HasDependents
old.Ratio(modele.HasDependentsHasMortgage$coefficients,"HasDependentsYes")

# pour HasDependents et HasMortgage 
old.Ratio(modele.HasDependentsHasMortgage$coefficients,"HasDependentsYes:HasMortgageYes")

#********************************************************************************************************#

# HasCoSigner
modele.HasCoSigner <- summary(glm(Default ~ HasCoSigner,data = data_train, family = binomial))
#modele.HasCoSigner
old.Ratio(modele.HasCoSigner$coefficients,"HasCoSignerYes")

#********************************************************************************************************#

# HasDependents & HasCoSigner
modele.HasDependentsHasCoSigner <- summary(glm(Default ~ HasDependents*HasCoSigner,data = data_train, family = binomial))
modele.HasDependentsHasCoSigner


# pour HasCoSigner

old.Ratio(modele.HasDependentsHasCoSigner$coefficients,"HasCoSignerYes")

# pour HasDependents
old.Ratio(modele.HasDependentsHasCoSigner$coefficients,"HasDependentsYes")

#********************************************************************************************************#
# HasCoSigner & HasMortgage
modele.HasCoSignerHasMortgage <- summary(glm(Default ~ HasCoSigner*HasMortgage,data = data_train, family = binomial))
modele.HasCoSignerHasMortgage

# pour HasMortgage

old.Ratio(modele.HasCoSignerHasMortgage$coefficients,"HasMortgageYes")

# pour HasCoSigner
old.Ratio(modele.HasCoSignerHasMortgage$coefficients,"HasCoSignerYes")



#********************************************************************************************************#
# Variable qualitative non binomiale. 


modele.EmploymentType <- summary(glm(Default ~ EmploymentType, data = data_train, family=binomial))
apply(modele.EmploymentType$coefficients,1,function(x){exp(x["Estimate"])})



# MaritalStatus

modele.MaritalStatus <- summary(glm(Default ~ MaritalStatus, data = data_train, family=binomial))
apply(modele.MaritalStatus$coefficients,1,function(x){exp(x["Estimate"])})



#LoanPurpose

modele.LoanPurpose <- summary(glm(Default ~ LoanPurpose, data = data_train, family=binomial))
apply(modele.LoanPurpose$coefficients,1,function(x){exp(x["Estimate"])})















