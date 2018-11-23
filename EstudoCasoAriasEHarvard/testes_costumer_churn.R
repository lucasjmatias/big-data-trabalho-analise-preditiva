customerChurn <-
  read.csv2("./dados/estudo_caso_customer_churn.csv", stringsAsFactors = FALSE)


ggpairs(data = customerChurn, ggplot2::aes(color=as.factor(churn)))

table(customerChurn$costumer_age)

customerChurn$customer_age
#Preparar dados
customerChurn$clientes_novos <- 0
customerChurn$clientes_novos[customerChurn$customer_age < 6] <- 1
customerChurn$clientes_risco <- 0
customerChurn$clientes_risco[customerChurn$customer_age >= 6 & customerChurn$customer_age < 14] <- 1


str(customerChurn)
View(customerChurn)
#O que é um chi-score baixo?
boxplot(customerChurn$chi_score_month_1)
boxplot(customerChurn$chi_score_month_0)

summary(customerChurn$chi_score)
summary(customerChurn$chi_score_month_0)
summary(customerChurn$chi_score[customerChurn$churn == 1])

str(customerChurn)
glm(data = customerChurn,
    formula = churn ~ chi_score_month_0   +
      views_0_1 + days_since_last_login_0_1 +
      clientes_novos + clientes_risco, family = binomial) -> 
        glmCustomer

summary(glmCustomer)
