ggpairs(data = defaultCredito, ggplot2::aes(color=as.factor(default)))



glm(data = defaultCredito,
    formula = default ~ t_emprego + divida + divida_cc, family = binomial) -> 
  glmDefaultCredito

summary(glmDefaultCredito)

glmprobsDefaultCredito <- predict(glmDefaultCredito, type="response")
nLinhasDefaultCredito <- nrow(defaultCredito)
glmpredDefaultCredito <- rep(0, nLinhasDefaultCredito)
glmpredDefaultCredito[ glmprobsDefaultCredito > 0.5 ] <- 1
table(glmpredDefaultCredito, defaultCredito$default) -> tabela
(as.vector(tabela)[1] + as.vector(tabela)[4]) / nLinhasDefaultCredito


