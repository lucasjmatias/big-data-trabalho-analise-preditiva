#install.packages("tidyverse")
#install.packages("GGally")

pagamentoEmprestimo$id <- NULL
pagamentoEmprestimo$pagamento <- as.factor(pagamentoEmprestimo$pagamento)
pagamentoEmprestimo$sexo <- as.factor(pagamentoEmprestimo$sexo)
pagamentoEmprestimo$pagamento <- as.numeric(pagamentoEmprestimo$pagamento)
pagamentoEmprestimo$sexo <- as.numeric(pagamentoEmprestimo$sexo)


glm(data = pagamentoEmprestimo, formula = pagamento ~ estadocivil + idade + sexo, family = binomial) -> glmPagamento
summary(glmPagamento)


ggpairs(data = pagamentoEmprestimo, ggplot2::aes(color=as.factor(pagamento)))
