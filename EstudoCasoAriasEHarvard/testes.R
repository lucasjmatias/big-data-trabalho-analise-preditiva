#install.packages("tidyverse")
#install.packages("GGally")

pagamentoEmprestimo$id <- NULL
pagamentoEmprestimo$pagamento <- as.factor(pagamentoEmprestimo$pagamento)
pagamentoEmprestimo$sexo <- as.factor(pagamentoEmprestimo$sexo)
pagamentoEmprestimo$pagamento <- as.numeric(pagamentoEmprestimo$pagamento)
pagamentoEmprestimo$sexo <- as.numeric(pagamentoEmprestimo$sexo)


glm(data = pagamentoEmprestimo, formula = pagamento ~ estadocivil + idade + sexo, family = binomial) -> glmPagamento
summary(glmPagamento)
coef(glmPagamento)
glmprobs <- predict(glmPagamento, type="response")

nLinhas <- nrow(pagamentoEmprestimo)
glmpred <- rep(0, nLinhas)
glmpred[ glmprobs > 0.5 ] <- 1
View(glmpred)

View(pagamentoEmprestimo$pagamento)

table(glmpred, pagamentoEmprestimo$pagamento)

glmprobs
ggpairs(data = pagamentoEmprestimo, ggplot2::aes(color=as.factor(pagamento)))
