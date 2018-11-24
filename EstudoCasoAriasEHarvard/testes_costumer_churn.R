#install.packages("ROCR")
#install.packages("caret")
#install.packages("ResourceSelection")
#install.packages("pROC")

#library(pROC)
#library(ResourceSelection)
#library(ROSE)
#library(ROCR)
#library(MASS)
#library(caret)

customerChurn <-
  read.csv2("./dados/estudo_caso_customer_churn.csv", stringsAsFactors = FALSE)


#Preparar dados
customerChurn$clientes_novos <- 0
customerChurn$clientes_novos[customerChurn$customer_age < 6] <- 1
customerChurn$clientes_risco <- 0
customerChurn$clientes_risco[customerChurn$customer_age > 6 &
                             customerChurn$customer_age <= 18] <- 1



glm(data = customerChurn,
    formula = churn ~ clientes_risco + chi_score_month_0 + support_cases_month_0 +  
      days_since_last_login_0_1 + support_cases_0_1   , family = binomial) -> 
        glmCustomer
summary(glmCustomer)

glmprobsCostumer <- predict(glmCustomer, type="response")

# head(sort(glmprobsCostumer, decreasing = TRUE), 100)



rocobj <- roc(customerChurn$churn, glmprobsCostumer)
coords(rocobj, x="best", input="threshold", best.method="youden")[1] -> pontoCorte
pontoCorte

nLinhasCostumer <- nrow(customerChurn)
glmpredCostumer <- rep(0, nLinhasCostumer)
glmpredCostumer[ glmprobsCostumer > pontoCorte ] <- 1


dadosReais <- customerChurn$churn
table(glmpredCostumer, dadosReais) -> tabelaCostumerChurn
tabelaCostumerChurn

(as.vector(tabelaCostumerChurn)[1] + as.vector(tabelaCostumerChurn)[4]) / nLinhasCostumer
mean(customerChurn$churn == glmpredCostumer)

sensitivity(tabelaCostumerChurn)
specificity(tabelaCostumerChurn)
?specificity


customerChurn$probs = glmprobsCostumer
head(order(customerChurn$probs, -probs), 100)


head(customerChurn[order(-customerChurn$probs),], 100) -> clientesMaisProvaveisChurn
cbind(clientesMaisProvaveis$id, clientesMaisProvaveis$probs) -> listaFinal


glmCustomer$coefficients

customerChurn
