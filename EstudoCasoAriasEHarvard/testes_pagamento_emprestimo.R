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


table(glmpredPagamento, pagamentoEmprestimo$pagamento) -> tabela

str(tabela)
tabela$table

glm(data = pagamentoEmprestimo,
    formula = pagamento ~ estadocivil  , family = binomial) -> 
  glmPagamento
summary(glmPagamento)
glmprobsPagamento <- predict(glmPagamento, type="response")
nLinhasPagamento <- nrow(pagamentoEmprestimo)
glmpredPagamento <- rep(0, nLinhasPagamento)
glmpredPagamento[ glmprobsPagamento >= 0.5 ] <- 1
table(glmpredPagamento, pagamentoEmprestimo$pagamento) -> tabela
(as.vector(tabela)[1] + as.vector(tabela)[4]) / nLinhasPagamento
