espessuras <- as.data.frame(espessuras)
str(espessuras)
espessuras$...25 <- NULL
espessuras <- espessuras[-c(535:545), ]
str(espessuras)

library(tidyverse)
library(dplyr)
espessuras$Sexo <- recode(espessuras$Sexo, "M" = "Masculino", "F" = "Feminino")
espessuras$Idade <- as.factor(espessuras$Idade)
levels(espessuras$Idade) <- c("7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18")
espessuras$Grupo <- factor(espessuras$Idade, levels = c("7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18"),
                           labels = c("7 a 11 anos", "7 a 11 anos", "7 a 11 anos", "7 a 11 anos", "7 a 11 anos", "12 a 18 anos", 
                                      "12 a 18 anos", "12 a 18 anos", "12 a 18 anos", "12 a 18 anos", "12 a 18 anos", "12 a 18 anos"))
str(espessuras)
table(espessuras$Sexo)
table(espessuras$Grupo)
table(espessuras$Sexo, espessuras$Grupo)
#######################################################################################################################################
# Espessuras de tecido mole geral:
#######################################################################################################################################
library(dplyr)
medidas.resumo <- espessuras %>%
  dplyr::select(3:37) %>%
  summarise(across(
    everything(),  # Seleciona todas as colunas da 3ª à 37ª
    list(n = ~ sum(!is.na(.x)),
      Média = ~ mean(.x, na.rm = TRUE),
      Variância = ~ var(.x, na.rm = TRUE),
      Desvio.padrão = ~ sd(.x, na.rm = TRUE),
      Mediana = ~ median(.x, na.rm = TRUE),
      Mínimo = ~ min(.x, na.rm = TRUE),
      Máximo = ~ max(.x, na.rm = TRUE),
      Coef.variação = ~ (sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE)) * 100
    ),
    .names = "{.col}_{.fn}"  # Renomeia as colunas com o nome da função aplicada
  ))

resultado.geral <- medidas.resumo %>% 
  pivot_longer(cols = everything(), names_to = c("variavel", "medida"),  # Separando o nome da coluna original e a métrica
                                                   names_sep = "_",  # Separando pelo "_"
                                                   values_to = "valor") %>% pivot_wider(names_from = medida,  # Transformando a coluna 'medida' em novas colunas # Usando os valores calculados para preencher as novas colunas
                                                                                        values_from = valor)

resultado.geral <- as.data.frame(resultado.geral)
str(resultado.geral)
resultado.geral <- resultado.geral %>% mutate(across(where(is.numeric), ~ round(.x, 2)))
str(resultado.geral)
# Instalar e carregar o pacote xtable
install.packages("xtable")
library(xtable)

# Converter para LaTeX
print(xtable(resultado.geral), type = "latex", file = "tabela.geral.tex")

#Converter para excel
library(writexl)
write_xlsx(resultado.geral, "tabela2.xlsx")
#######################################################################################################################################
# Espessuras de tecido mole por grupo (sexo):
#######################################################################################################################################
library(dplyr)
medidas.sexo <- espessuras %>% dplyr::select(2:37) %>% group_by(Sexo) %>% summarise(across(everything(),
    list(n = ~ sum(!is.na(.x)),
         Média = ~ mean(.x, na.rm = TRUE),
         Variância = ~ var(.x, na.rm = TRUE),
         Desvio.padrão = ~ sd(.x, na.rm = TRUE),
         Mediana = ~ median(.x, na.rm = TRUE),
         Mínimo = ~ min(.x, na.rm = TRUE),
         Máximo = ~ max(.x, na.rm = TRUE),
         Coef.variação = ~ (sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE)) * 100
    ),
    .names = "{.col}_{.fn}"  # Renomeia as colunas com o nome da função aplicada
  ))

medidas.sexo

resultado.sexo <- medidas.sexo %>% pivot_longer(cols = -Sexo, names_to = c("variavel", "medida"),  # Separando o nome da coluna original e a métrica
                                                   names_sep = "_",  # Separando pelo "_"
                                                   values_to = "valor") %>% pivot_wider(names_from = medida,  # Transformando a coluna 'medida' em novas colunas
                                                                                        values_from = valor)  # Usando os valores calculados para preencher as novas colunas 


resultado.sexo
resultado.sexo <- as.data.frame(resultado.sexo)
str(resultado.sexo)
resultado.sexo <- resultado.sexo %>% mutate(across(where(is.numeric), ~ round(.x, 2)))

# Instalar e carregar o pacote xtable
install.packages("xtable")
library(xtable)

# Converter para LaTeX
print(xtable(resultado.sexo), type = "latex", file = "tabela.sexo.tex")

#Converter para excel
library(writexl)
write_xlsx(resultado.sexo, "tabela3.xlsx")

#######################################################################################################################################
# Espessuras por grupo (idade):
#######################################################################################################################################
library(dplyr)
medidas.idade <- espessuras %>% dplyr::select(3:38) %>% group_by(Idade) %>% summarise(across(everything(), list(n = ~ sum(!is.na(.x)),
                                     Média = ~ mean(.x, na.rm = TRUE),
                                     Variância = ~ var(.x, na.rm = TRUE),
                                     Desvio.padrão = ~ sd(.x, na.rm = TRUE),
                                     Mediana = ~ median(.x, na.rm = TRUE),
                                     Mínimo = ~ ifelse(all(is.na(.x)), NA, min(.x, na.rm = TRUE)),  
                                     Máximo = ~ ifelse(all(is.na(.x)), NA, max(.x, na.rm = TRUE)),
                                     Coef.variação = ~ (sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE)) * 100
    ),
    .names = "{.col}_{.fn}"  # Renomeia as colunas com o nome da função aplicada
  ))

                                                                        
medidas.idade

resultado.idade <- medidas.idade %>% pivot_longer(cols = -Idade, names_to = c("variavel", "medida"),  # Separando o nome da coluna original e a métrica
                              names_sep = "_",  # Separando pelo "_"
                              values_to = "valor") %>% pivot_wider(names_from = medida,  
                                                                   values_from = valor)  

resultado.idade
resultado.idade <- as.data.frame(resultado.idade)
str(resultado.idade)
resultado.idade <- resultado.idade %>% mutate(across(where(is.numeric), ~ round(.x, 2)))

# Instalar e carregar o pacote xtable
install.packages("xtable")
library(xtable)

# Converter para LaTeX
print(xtable(resultado.idade), type = "latex", file = "tabela.idade.tex")

#Converter para excel
library(writexl)
write_xlsx(resultado.idade, "anexos.xlsx")
#######################################################################################################################################
# Espessuras por grupo (idade): 2 grupos
#######################################################################################################################################
library(dplyr)
medidas.idade.grupo <- espessuras %>% dplyr::select(3:37, 39) %>% group_by(Grupo) %>% summarise(across(everything(), list(n = ~ sum(!is.na(.x)),
                                                                                                                Média = ~ mean(.x, na.rm = TRUE),
                                                                                                                Variância = ~ var(.x, na.rm = TRUE),
                                                                                                                Desvio.padrão = ~ sd(.x, na.rm = TRUE),
                                                                                                                Mediana = ~ median(.x, na.rm = TRUE),
                                                                                                                Mínimo = ~ ifelse(all(is.na(.x)), NA, min(.x, na.rm = TRUE)),  
                                                                                                                Máximo = ~ ifelse(all(is.na(.x)), NA, max(.x, na.rm = TRUE)),
                                                                                                                Coef.variação = ~ (sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE)) * 100
     ),
    .names = "{.col}_{.fn}"  # Renomeia as colunas com o nome da função aplicada
))


medidas.idade.grupo

resultado.idade.grupo <- medidas.idade.grupo %>% pivot_longer(cols = -Grupo, names_to = c("variavel", "medida"),  # Separando o nome da coluna original e a métrica
                                                  names_sep = "_",  # Separando pelo "_"
                                                  values_to = "valor") %>% pivot_wider(names_from = medida,  
                                                                                       values_from = valor)  
resultado.idade.grupo
resultado.idade.grupo <- as.data.frame(resultado.idade.grupo)
str(resultado.idade.grupo)
resultado.idade.grupo <- resultado.idade.grupo %>% mutate(across(where(is.numeric), ~ round(.x, 2)))

# Instalar e carregar o pacote xtable
install.packages("xtable")
library(xtable)

# Converter para LaTeX
print(xtable(resultado.idade.grupo), type = "latex", file = "tabela.idade.grupo.tex")

#Converter para excel
library(writexl)
write_xlsx(resultado.idade.grupo, "tabela4.xlsx")

#######################################################################################################################################
# Espessuras por grupo (idade) x sexo
#######################################################################################################################################
medidas.sexo.grupo <- espessuras %>% dplyr::select(2:37, 39) %>% group_by(Grupo, Sexo) %>% summarise(across(everything(), list(n = ~ sum(!is.na(.x)),
                                                                                                   Média = ~ mean(.x, na.rm = TRUE),
                                                                                                   Variância = ~ var(.x, na.rm = TRUE),
                                                                                                   Desvio.padrão = ~ sd(.x, na.rm = TRUE),
                                                                                                   Mediana = ~ median(.x, na.rm = TRUE),
                                                                                                   Mínimo = ~ ifelse(all(is.na(.x)), NA, min(.x, na.rm = TRUE)),  
                                                                                                   Máximo = ~ ifelse(all(is.na(.x)), NA, max(.x, na.rm = TRUE)),
                                                                                                   Coef.variação = ~ (sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE)) * 100),
  .names = "{.col}_{.fn}"  # Renomeia as colunas com o nome da função aplicada
))

resultado.sexo.grupo <- medidas.sexo.grupo %>% pivot_longer(cols = -c(Grupo, Sexo), names_to = c("variavel", "medida"),  # Separando o nome da coluna original e a métrica
                                                              names_sep = "_",  # Separando pelo "_"
                                                              values_to = "valor") %>% pivot_wider(names_from = medida,  
                                                                                                   values_from = valor)  
resultado.sexo.grupo
resultado.sexo.grupo <- as.data.frame(resultado.sexo.grupo)
resultado.sexo.grupo <- resultado.sexo.grupo %>% mutate(across(where(is.numeric), ~ round(.x, 2)))
resultado.sexo.grupo

#Converter para excel
library(writexl)
write_xlsx(resultado.sexo.grupo, "tabela11.xlsx")


#######################################################################################################################################
# Teste-t - Variável Sexo
#######################################################################################################################################
##############
# Descritiva
##############
resultado.sexo %>% ggplot(aes(variavel, Média, fill= Sexo, group = Sexo)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) + 
  geom_errorbar(aes(ymin = Média - Desvio.padrão, ymax = Média + Desvio.padrão), position = position_dodge(width = 0.8), width = 0.25) +
  labs(x = "Ponto craniométrico", y = "Espessura média (em mm)", title = "Espessura média (em mm) e desvio-padrão para cada ponto craniométrico, por sexo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  
##############
# Pressupostos
##############
#Independência
#Homogeneidade e Normalidade: qqplot
# Ordenando os dados
glabela.f <- espessuras %>% filter(Sexo=="Feminino") %>% select(Glabela) %>% na.omit()
glabela.m <- espessuras %>% filter(Sexo=="Masculino") %>% select(Glabela) %>% na.omit()
# Número total de observações para cada grupo
n_glabela.f <- length(glabela.f$Glabela)
n_glabela.m <- length(glabela.m$Glabela)
# Frequências cumulativas observadas para cada grupo (usando (j - 0.5) / n)
freq_feminino <- (1:n_glabela.f - 0.5) / n_glabela.f
freq_masculino <- (1:n_glabela.m - 0.5) / n_glabela.m
# Ordenando as observacoes para cada grupo
dados_masculino_ordenado <- sort(glabela.m$Glabela)
dados_feminino_ordenado <- sort(glabela.f$Glabela)
# Calculando os quantis teóricos para a distribuição normal
quantis_teoricos_masculino <- qnorm(freq_masculino)
quantis_teoricos_feminino <- qnorm(freq_feminino)
# Plotando o gráfico de probabilidade normal para ambos os sexos no mesmo gráfico:
plot(dados_masculino_ordenado, quantis_teoricos_masculino,
     col = "blue", pch = 16, xlab = "Observações Ordenadas", ylab = "Quantis teóricos", 
     main = "Normal Probability Plot para Masculino e Feminino",
     xlim = range(c(dados_masculino_ordenado, dados_feminino_ordenado)),
     ylim = range(c(quantis_teoricos_masculino, quantis_teoricos_feminino)))
# Adicionando os pontos do grupo feminino
points(dados_feminino_ordenado, quantis_teoricos_feminino, col = "red", pch = 16)
# Adicionando legenda
legend("topleft", legend = c("Masculino", "Feminino"), col = c("blue", "red"), pch = 16)


#ou GLABELA #######
qqnorm(glabela.m$Glabela, pch = 1, frame = FALSE, col = "blue", main = "Gráfico de Probabilidade Normal ")
qqline(glabela.m$Glabela, col = "blue", lwd = 2)  # Linha de referência para Homens
qqnorm(glabela.f$Glabela, pch = 1, col = "red", add = TRUE)
qqline(glabela.f$Glabela, col = "red", lwd = 2)  # Linha de referência para Mulheres

library(pastecs)
by(espessuras$Glabela, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))


# NASIO #######
nasio.f <- espessuras %>% filter(Sexo=="Feminino") %>% select(Nasio) %>% na.omit()
nasio.m <- espessuras %>% filter(Sexo=="Masculino") %>% select(Nasio) %>% na.omit()
qqnorm(nasio.m$Nasio, pch = 1, frame = FALSE, col = "blue", main = "Gráfico de Probabilidade Normal ")
qqline(nasio.m$Nasio, col = "blue", lwd = 2)  # Linha de referência para Homens
qqnorm(nasio.f$Nasio, pch = 1, col = "red", add = TRUE)
qqline(nasio.f$Nasio, col = "red", lwd = 2)  # Linha de referência para Mulheres

library(pastecs)
by(espessuras$Nasio, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
library(car)
leveneTest(espessuras$Nasio, espessuras$Sexo, center = median)

 # Rinio #######
library(pastecs)
by(espessuras$Rinio, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE)  %>% lapply(function(x) round(x, 3))
rinio.m <- espessuras %>% filter(Sexo=="Masculino") %>% select(Rinio) %>% na.omit()
qqnorm(rinio.m$Rinio, pch = 1, frame = FALSE, col = "blue", main = "Gráfico de Probabilidade Normal ")
qqline(rinio.m$Rinio, col = "blue", lwd = 2)  # Linha de referência para Homens

# Filtro medio #######
by(espessuras$Filtro.medio, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE)  %>% lapply(function(x) round(x, 3))

# Prostio #######
by(espessuras$Prostio, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE)  %>% lapply(function(x) round(x, 3))

# Infradental #######
by(espessuras$Infradental, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE)  %>% lapply(function(x) round(x, 3))

# Supramental #######
by(espessuras$Supramental, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE)  %>% lapply(function(x) round(x, 3))

# Pogonio #######
by(espessuras$Pogonio, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE)  %>% lapply(function(x) round(x, 3))

# Mento #######
by(espessuras$Mento, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE)  %>% lapply(function(x) round(x, 3))

# Midsupraorbial  #######
by(espessuras$Mid.supraorbital.E, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE)  %>% lapply(function(x) round(x, 3))
by(espessuras$Mid.supraorbital.D, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE)  %>% lapply(function(x) round(x, 3))
library(car)
leveneTest(espessuras$Mid.supraorbital.E, espessuras$Sexo, center = median)

# Midinfraaorbial  #######
by(espessuras$Mid.infraorbital.E, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE)  %>% lapply(function(x) round(x, 3))
by(espessuras$Mid.infraorbital.D, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE)  %>% lapply(function(x) round(x, 3))
# Malar  #######
by(espessuras$Malar.E, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE)  %>% lapply(function(x) round(x, 3))
by(espessuras$Malar.D, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE)  %>% lapply(function(x) round(x, 3))
library(car)
leveneTest(espessuras$Malar.D, espessuras$Sexo, center = median)
# Lateral orbita #######
by(espessuras$Lateral.orbita.E, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE)  %>% lapply(function(x) round(x, 3))
by(espessuras$Lateral.orbita.D, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE)  %>% lapply(function(x) round(x, 3))
library(car)
leveneTest(espessuras$Lateral.orbita.E, espessuras$Sexo, center = median)
# Zigio E #######
by(espessuras$Zigio.E, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE)  %>% lapply(function(x) round(x, 3))
by(espessuras$Zigio.D, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE)  %>% lapply(function(x) round(x, 3))
# Supraglenoide #######
by(espessuras$Supraglenoide.E, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE)  %>% lapply(function(x) round(x, 3))
by(espessuras$Supraglenoide.D, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE)  %>% lapply(function(x) round(x, 3))
library(car)
leveneTest(espessuras$Supraglenoide.E, espessuras$Sexo, center = median)
# Ectomolare1s #######
by(espessuras$Ectomolare1s.E, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE)  %>% lapply(function(x) round(x, 3))
by(espessuras$Ectomolare1s.D, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE)  %>% lapply(function(x) round(x, 3))
# Ectomolare2s  #######
by(espessuras$Ectomolare2s.E, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE)  %>% lapply(function(x) round(x, 3))
by(espessuras$Ectomolare2s.D, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE)  %>% lapply(function(x) round(x, 3))
# Ectomolare1I  #######
by(espessuras$Ectomolare1i.E, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE)  %>% lapply(function(x) round(x, 3))
by(espessuras$Ectomolare1i.D, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE)  %>% lapply(function(x) round(x, 3))
# Ectomolare2I  #######
by(espessuras$Ectomolare2i.E, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE)  %>% lapply(function(x) round(x, 3))
by(espessuras$Ectomolare2i.D, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE)  %>% lapply(function(x) round(x, 3))
# Gonio #######
by(espessuras$Gonio.E, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE)  %>% lapply(function(x) round(x, 3))
by(espessuras$Gonio.D, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE)  %>% lapply(function(x) round(x, 3))
# Linha oclusal  #######
by(espessuras$Linha.oclusal.E, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE)  %>% lapply(function(x) round(x, 3))
by(espessuras$Linha.oclusal.D, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE)  %>% lapply(function(x) round(x, 3))
# Mid ramus E #######
by(espessuras$Mid.ramus.E, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE)  %>% lapply(function(x) round(x, 3))
by(espessuras$Mid.ramus.D, espessuras$Sexo, stat.desc, basic = FALSE, norm = TRUE)  %>% lapply(function(x) round(x, 3))








##############
# Teste
##############
#Paramétrico: Teste-t
#H0: mu1 = mu2
#H1: mu1 != mu2
#alfa: 5%
#poder: 80%
t.test(Nasio ~ Sexo, data = espessuras, var.equal = TRUE, na.action = na.exclude, alternative = "two.sided")
t.test(Mid.supraorbital.E ~ Sexo, data = espessuras, var.equal = TRUE, na.action = na.exclude, alternative = "two.sided")
t.test(Malar.D ~ Sexo, data = espessuras, var.equal = TRUE, na.action = na.exclude, alternative = "two.sided")
t.test(Lateral.orbita.E ~ Sexo, data = espessuras, var.equal = FALSE, na.action = na.exclude, alternative = "two.sided")
t.test(Supraglenoide.E ~ Sexo, data = espessuras, var.equal = TRUE, na.action = na.exclude, alternative = "two.sided")
#Não paramétrico: Mann-Whitney (Wilcoxon rank-sum test)
#H0: os dois grupos masc e fem nao diferem quanto a espessura da glabela
#H1: diferem
wilcox.test(Rinio ~ Sexo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Filtro.medio ~ Sexo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Prostio ~ Sexo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Infradental ~ Sexo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Supramental ~ Sexo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Pogonio ~ Sexo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Mento ~ Sexo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Mid.supraorbital.D ~ Sexo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Mid.infraorbital.E ~ Sexo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Mid.infraorbital.D ~ Sexo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Malar.E ~ Sexo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Lateral.orbita.D ~ Sexo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Zigio.E ~ Sexo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Zigio.D ~ Sexo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Supraglenoide.D ~ Sexo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Ectomolare1s.E ~ Sexo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Ectomolare1s.D ~ Sexo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Ectomolare2s.E ~ Sexo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Ectomolare2s.D ~ Sexo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Supraglenoide.D ~ Sexo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Ectomolare1i.E ~ Sexo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Ectomolare1i.D ~ Sexo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Ectomolare2i.E ~ Sexo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Ectomolare2i.D ~ Sexo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Gonio.E ~ Sexo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Gonio.D ~ Sexo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Linha.oclusal.E ~ Sexo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Linha.oclusal.D ~ Sexo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Mid.ramus.E ~ Sexo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Mid.ramus.D ~ Sexo, data = espessuras, na.action = na.exclude, alternative = "two.sided")


##############
# Teste-t - Variável Idade
##############
resultado.idade.grupo %>% ggplot(aes(variavel, Média, fill= Grupo, group = Grupo)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) + 
  geom_errorbar(aes(ymin = Média - Desvio.padrão, ymax = Média + Desvio.padrão), position = position_dodge(width = 0.8), width = 0.25) +
  labs(x = "Ponto craniométrico", y = "Espessura média (em mm)", title = "Espessura média (em mm) e desvio-padrão para cada ponto craniométrico, por idade") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

##############
# Pressupostos
##############
#Independência
#Homogeneidade e Normalidade:
library(pastecs)
by(espessuras$Glabela, espessuras$Grupo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
library(car)
leveneTest(espessuras$Glabela, espessuras$Grupo, center = median)


by(espessuras$Nasio, espessuras$Grupo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
leveneTest(espessuras$Nasio, espessuras$Grupo, center = median)

by(espessuras$Rinio, espessuras$Grupo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
leveneTest(espessuras$Rinio, espessuras$Grupo, center = median)

by(espessuras$Filtro.medio, espessuras$Grupo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
leveneTest(espessuras$Filtro.medio, espessuras$Grupo, center = median)

by(espessuras$Prostio, espessuras$Grupo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
leveneTest(espessuras$Prostio, espessuras$Grupo, center = median)

by(espessuras$Infradental, espessuras$Grupo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
leveneTest(espessuras$Infradental, espessuras$Grupo, center = median)

by(espessuras$Supramental, espessuras$Grupo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
leveneTest(espessuras$Supramental, espessuras$Grupo, center = median)

by(espessuras$Pogonio, espessuras$Grupo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
leveneTest(espessuras$Pogonio, espessuras$Grupo, center = median)

by(espessuras$Mento, espessuras$Grupo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
leveneTest(espessuras$Mento, espessuras$Grupo, center = median)

by(espessuras$Mid.supraorbital.E, espessuras$Grupo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
leveneTest(espessuras$Mid.supraorbital.E, espessuras$Grupo, center = median)
by(espessuras$Mid.supraorbital.D, espessuras$Grupo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
leveneTest(espessuras$Mid.supraorbital.D, espessuras$Grupo, center = median)
by(espessuras$Mid.infraorbital.E, espessuras$Grupo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
leveneTest(espessuras$Mid.infraorbital.E, espessuras$Grupo, center = median)
by(espessuras$Mid.infraorbital.D, espessuras$Grupo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
leveneTest(espessuras$Mid.infraorbital.D, espessuras$Grupo, center = median)

by(espessuras$Malar.E, espessuras$Grupo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
leveneTest(espessuras$Malar.E, espessuras$Grupo, center = median)

by(espessuras$Malar.D, espessuras$Grupo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
leveneTest(espessuras$Malar.D, espessuras$Grupo, center = median)

by(espessuras$Lateral.orbita.D, espessuras$Grupo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
leveneTest(espessuras$Lateral.orbita.D, espessuras$Grupo, center = median)

by(espessuras$Zigio.E, espessuras$Grupo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
leveneTest(espessuras$Zigio.E, espessuras$Grupo, center = median)

by(espessuras$Zigio.D, espessuras$Grupo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
leveneTest(espessuras$Zigio.D, espessuras$Grupo, center = median)

by(espessuras$Supraglenoide.D, espessuras$Grupo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
leveneTest(espessuras$Supraglenoide.D, espessuras$Grupo, center = median)

by(espessuras$Supraglenoide.E, espessuras$Grupo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
leveneTest(espessuras$Supraglenoide.E, espessuras$Grupo, center = median)

by(espessuras$Ectomolare1s.E, espessuras$Grupo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
leveneTest(espessuras$Ectomolare1s.E, espessuras$Grupo, center = median)
by(espessuras$Ectomolare1s.D, espessuras$Grupo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
leveneTest(espessuras$Ectomolare1s.D, espessuras$Grupo, center = median)
by(espessuras$Ectomolare2s.E, espessuras$Grupo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
leveneTest(espessuras$Ectomolare2s.E, espessuras$Grupo, center = median)
by(espessuras$Ectomolare2s.D, espessuras$Grupo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
leveneTest(espessuras$Ectomolare2s.D, espessuras$Grupo, center = median)
by(espessuras$Ectomolare1i.E, espessuras$Grupo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
leveneTest(espessuras$Ectomolare1i.E, espessuras$Grupo, center = median)
by(espessuras$Ectomolare1i.D, espessuras$Grupo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
leveneTest(espessuras$Ectomolare1i.D, espessuras$Grupo, center = median)
by(espessuras$Ectomolare2i.E, espessuras$Grupo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
leveneTest(espessuras$Ectomolare2i.E, espessuras$Grupo, center = median)
by(espessuras$Ectomolare2i.D, espessuras$Grupo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
leveneTest(espessuras$Ectomolare2i.D, espessuras$Grupo, center = median)

by(espessuras$Gonio.E, espessuras$Grupo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
leveneTest(espessuras$Gonio.E, espessuras$Grupo, center = median)

by(espessuras$Gonio.D, espessuras$Grupo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
leveneTest(espessuras$Gonio.D, espessuras$Grupo, center = median)

by(espessuras$Linha.oclusal.E, espessuras$Grupo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
leveneTest(espessuras$Linha.oclusal.E, espessuras$Grupo, center = median)

by(espessuras$Mid.ramus.E, espessuras$Grupo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
leveneTest(espessuras$Mid.ramus.E, espessuras$Grupo, center = median)
by(espessuras$Mid.ramus.D, espessuras$Grupo, stat.desc, basic = FALSE, norm = TRUE) %>% lapply(function(x) round(x, 3))
leveneTest(espessuras$Mid.ramus.D, espessuras$Grupo, center = median)


espessuras %>% filter(Grupo=="7 a 11 anos") %>% select(Ectomolare2i.D) %>% na.omit() %>% stat.desc(norm = TRUE)
espessuras %>% filter(Sexo=="Masculino") %>% select(Glabela) %>% na.omit()

##############
# Teste
##############
#Paramétrico: Teste-t
#H0: mu1 = mu2
#H1: mu1 != mu2
#alfa: 5%

t.test(Nasio ~ Grupo, data = espessuras, var.equal = FALSE, na.action = na.exclude, alternative = "two.sided")
t.test(Filtro.medio ~ Grupo, data = espessuras, var.equal = FALSE, na.action = na.exclude, alternative = "two.sided")
t.test(Supramental ~ Grupo, data = espessuras, var.equal = FALSE, na.action = na.exclude, alternative = "two.sided")
t.test(Pogonio ~ Grupo, data = espessuras, var.equal = FALSE, na.action = na.exclude, alternative = "two.sided")
t.test(Mid.supraorbital.E ~ Grupo, data = espessuras, var.equal = FALSE, na.action = na.exclude, alternative = "two.sided")
t.test(Malar.E ~ Grupo, data = espessuras, var.equal = FALSE, na.action = na.exclude, alternative = "two.sided")
t.test(Malar.D ~ Grupo, data = espessuras, var.equal = TRUE, na.action = na.exclude, alternative = "two.sided")
t.test(Lateral.orbita.D ~ Grupo, data = espessuras, var.equal = FALSE, na.action = na.exclude, alternative = "two.sided")
t.test(Supraglenoide.E ~ Grupo, data = espessuras, var.equal = TRUE, na.action = na.exclude, alternative = "two.sided")
t.test(Supraglenoide.D ~ Grupo, data = espessuras, var.equal = FALSE, na.action = na.exclude, alternative = "two.sided")
t.test(Ectomolare1i.D ~ Grupo, data = espessuras, var.equal = FALSE, na.action = na.exclude, alternative = "two.sided")
#Não paramétrico: Mann-Whitney (Wilcoxon rank-sum test)
#H0: os dois grupos masc e fem nao diferem quanto a espessura da glabela
#H1: diferem
wilcox.test(Glabela ~ Grupo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Rinio ~ Grupo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Filtro.medio ~ Grupo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Prostio ~ Grupo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Infradental ~ Grupo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Mento ~ Grupo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Mid.supraorbital.D ~ Grupo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Mid.infraorbital.E ~ Grupo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Mid.infraorbital.D ~ Grupo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Lateral.orbita.E ~ Grupo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Zigio.E ~ Grupo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Zigio.D ~ Grupo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Supraglenoide.D ~ Grupo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Ectomolare1s.E ~ Grupo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Ectomolare1s.D ~ Grupo, data = espessuras, na.action = na.exclude, alternative = "two.sided")

wilcox.test(Ectomolare2s.E ~ Grupo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Ectomolare2s.D ~ Grupo, data = espessuras, na.action = na.exclude, alternative = "two.sided")

wilcox.test(Ectomolare1i.E ~ Grupo, data = espessuras, na.action = na.exclude, alternative = "two.sided")

wilcox.test(Ectomolare2i.E ~ Grupo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Ectomolare2i.D ~ Grupo, data = espessuras, na.action = na.exclude, alternative = "two.sided")

wilcox.test(Gonio.E ~ Grupo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Gonio.D ~ Grupo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Linha.oclusal.E ~ Grupo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Linha.oclusal.D ~ Grupo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Mid.ramus.E ~ Grupo, data = espessuras, na.action = na.exclude, alternative = "two.sided")
wilcox.test(Mid.ramus.D ~ Grupo, data = espessuras, na.action = na.exclude, alternative = "two.sided")




### tabelas
library(writexl)
tabelasexo <- resultado.sexo[ , c(1:4, 6, 7)]
tabelaidade <- resultado.idade.grupo[ , c(1:4, 6, 7)]

tabelasexo <- tabelasexo %>% arrange(variavel)
write_xlsx(tabelasexo, "tabelasexo.xlsx")

mediaf <- tabelasexo %>% filter(Sexo == "Feminino") %>% select(variavel, Média)
mediam <- tabelasexo %>% filter(Sexo == "Masculino") %>% select(variavel, Média)
df<- mediaf$Média - mediam$Média
var <- mediaf$variavel
dft<- data.frame(var, df)
dft

write_xlsx(dft, "dif.xlsx")

tabelaidade <- tabelaidade %>% arrange(variavel)
write_xlsx(tabelaidade, "tabelaidade.xlsx")

media1 <- tabelaidade %>% filter(Grupo == "7 a 11 anos") %>% select(variavel, Média)
media2 <- tabelaidade %>% filter(Grupo == "12 a 18 anos") %>% select(variavel, Média)
df<- media1$Média - media2$Média
var <- media1$variavel
dft<- data.frame(var, df)
dft
write_xlsx(dft, "dif.xlsx")



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### Comparação com Kuhnen
library(dplyr)
separado <- as.data.frame(kunen)
separado <- separado[,1:6]

kunen.sexo <- as.data.frame(kunen)
kunen.sexo <- kunen.sexo %>% na.exclude()
kunen.sexo <- kunen.sexo %>% rename(variavel = Variável)
str(kunen.sexo)
kunen.sexo$autor <- rep("Kuhnen", 38)


kondo.sexo <- data.frame(resultado.sexo$Sexo, resultado.sexo$variavel, resultado.sexo$Média, resultado.sexo$Desvio.padrão, resultado.sexo$n)
kondo.sexo <- kondo.sexo %>% rename(n = resultado.sexo.n, variavel = resultado.sexo.variavel)
str(kondo.sexo)
kondo.sexo <- kondo.sexo %>% rename(Sexo = resultado.sexo.Sexo, Media = resultado.sexo.Média, sd = resultado.sexo.Desvio.padrão)
kondo.sexo$autor <- rep("Kondo", 70)

kondo.sexo <- kondo.sexo[kondo.sexo$variavel %in% kunen.sexo$variavel, ]

comparacao <- rbind(kondo.sexo, kunen.sexo)



library(ggplot2)
comparacao %>% ggplot(aes(variavel, Media, fill= autor, group = autor)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) + 
  geom_errorbar(aes(ymin = Media - sd, ymax = Media + sd), position = position_dodge(width = 0.8), width = 0.25) +
  labs(x = "Ponto craniométrico", y = "Espessura", fill= "Autor", title = "Espessura média (em mm) e desvio-padrão para cada ponto craniométrico, por sexo e autor") +
  facet_wrap(~Sexo) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(size=0.2, colour="black"))











kunen.idade <- as.data.frame(kunen)
kunen.idade <- kunen.idade %>% na.exclude()
kunen.idade <- kunen.idade %>% rename(variavel = Variável)
str(kunen.idade)
kunen.idade$autor <- rep("Kuhnen", 38)


kondo.idade <- data.frame(resultado.idade.grupo$Grupo, resultado.idade.grupo$variavel, resultado.idade.grupo$Média, resultado.idade.grupo$Desvio.padrão, resultado.idade.grupo$n)
kondo.idade <- kondo.idade %>% rename(n = resultado.idade.grupo.n, variavel = resultado.idade.grupo.variavel, Idade = resultado.idade.grupo.Grupo, Media = resultado.idade.grupo.Média, sd = resultado.idade.grupo.Desvio.padrão)
str(kondo.idade)
kondo.idade$autor <- rep("Kondo", 70)
kondo.idade <- kondo.idade %>% na.exclude()



kondo.idade <- kondo.idade[kondo.idade$variavel %in% kunen.idade$variavel, ]
comparacao.idade <- rbind(kondo.idade, kunen.idade)


library(ggplot2)
comparacao.idade %>% ggplot(aes(variavel, Media, fill= autor, group = autor)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) + 
  geom_errorbar(aes(ymin = Media - sd, ymax = Media + sd), position = position_dodge(width = 0.8), width = 0.25) +
  labs(x = "Ponto craniométrico", y = "Espessura",fill= "Autor", title = "Espessura média (em mm) e desvio-padrão para cada ponto craniométrico, por idade e autor") +
  facet_wrap(~Idade) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(size=0.2, colour="black"))






teste<- kunen.sexo %>% arrange(Sexo)
diferenca.sexo <- data.frame(kondo.sexo$Media - teste$Media)
dif.sexo.autores <- data.frame(kondo.sexo$Sexo,kondo.sexo$variavel, kondo.sexo$Media, teste$Media, diferenca.sexo)
dif.sexo.autores <- dif.sexo.autores %>% rename(Sexo = kondo.sexo.Sexo, Variavel = kondo.sexo.variavel, Media.Kondo = kondo.sexo.Media, Media.Kuhnen = teste.Media, Diferenca = kondo.sexo.Media...teste.Media)
dif.sexo.autores <-  dif.sexo.autores %>% arrange(Variavel)
dif.sexo.autores <- dif.sexo.autores %>% select(Variavel, Sexo, Media.Kondo, Media.Kuhnen, Diferenca)


kunen.idade$Idade <- as.factor(kunen.idade$Idade)
kunen.idade$Idade <- factor(kunen.idade$Idade, levels = c("7 a 11 anos", "12 a 18 anos"))
str(kunen.idade)
teste <- kunen.idade %>% arrange(Idade)
teste <- teste %>% filter(variavel != c("Ectomolare2i.E", "Ectomolare2s.E"))
diferenca.idade <- data.frame(kondo.idade$Media - teste$Media)
dif.idade.autores <- data.frame(kondo.idade$variavel, kondo.idade$Idade, kondo.idade$Media, teste$Media, diferenca.idade)
dif.idade.autores <- dif.idade.autores %>% rename(Idade = kondo.idade.Idade, Variavel = kondo.idade.variavel, Media.Kondo = kondo.idade.Media, Media.Kuhnen = teste.Media, Diferenca = kondo.idade.Media...teste.Media)
dif.idade.autores <-  dif.idade.autores %>% arrange(Variavel)




#######################################################################################################################################
# ANOVA - Variável Sexo x Grupo
#######################################################################################################################################
#Descritiva - grafico média e sd
comparacao2way <- resultado.sexo.grupo %>% select(Grupo, Sexo, variavel, Média, Desvio.padrão) %>% rename(media = Média, sd = Desvio.padrão)
library(ggplot2)
comparacao2way %>% ggplot(aes(variavel, media, fill= Sexo)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) + 
  geom_errorbar(aes(ymin = media - sd, ymax = media + sd), position = position_dodge(width = 0.8), width = 0.25) +
  labs(x = "Ponto craniométrico", y = "Espessura", fill= "Sexo", title = "Espessura média (em mm) e desvio-padrão para cada ponto craniométrico, por sexo e faixa etária") +
  facet_wrap(~Grupo) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(size=0.2, colour="black"))



#####################
# Modelo 1 - Glabela
#####################
# ANOVA contexto da regressão
#criando dataframe
library(dplyr)
library(tidyverse)
dadosglabela <- espessuras %>% select(Glabela, Grupo, Sexo) %>% drop_na()

#modelo
modelo1 <- lm(Glabela ~ Grupo + Sexo + Grupo*Sexo, data = espessuras)
fit <- anova(modelo1)
fit
summary(modelo1) #para achar coefs do modelo. 
#Teste F testa:
#H0: beta0=beta1=beta2=0
#H1: peo menos1 é dif de zero
#alfa 5%
#Vemos que pelo menos 1 beta é diferente de zero. Verifica o teste t na tabela.


# Diagnóstico do modelo 
#qqplot - normalidade: teve dist normal
dadosglabela$residuos <- residuals(modelo1)
dadosglabela %>% ggplot(aes(sample = residuos)) + 
  stat_qq(color = "blue") + 
  stat_qq_line(color = "red") + 
  labs(x = "residuos amostra", y = "teoricos") +
  theme_bw()

shapiro.test(x = dadosglabela$residuos)

dadosglabela$res_padronizados <- dadosglabela$residuos/sqrt(fit$`Mean Sq`[4])
dadosglabela %>% ggplot(aes(sample = res_padronizados)) + 
  stat_qq(color = "blue") + 
  stat_qq_line(color = "red") + 
  labs(x = "residuos studentizados amostra", y = "teoricos")+
  theme_bw()

#residuos padronizados versus niveis fator Sexo : residuos aparentemente simetricos/variancia dos erros um pouco menor no masc
dadosglabela %>% ggplot(aes(x = res_padronizados, y = Sexo, color = Sexo)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Resíduos padronizados", y = "Sexo") +
  theme_bw()

#residuos padronizados versus niveis fator Grupo : residuos aparentemente simetricos/variancia dos erros um pouco menor no 7-14 anos
dadosglabela %>% ggplot(aes(x = res_padronizados, y = Grupo, color = Grupo)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Resíduos padronizados", y = "Sexo") +
  theme_bw()

#residuos padronizados versus niveis fator GrupoxSexo : 
dadosglabela %>% ggplot(aes(x = res_padronizados, y = Grupo, color = Sexo)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Resíduos padronizados", y = "Grupo") +
  facet_wrap(~Sexo) +
  theme_bw()


#residuos versus fitted (preditos)
dadosglabela$pred <- fitted(modelo1)
dadosglabela %>% ggplot(aes(x = pred, y = residuos, color = Sexo)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~Grupo) +
  labs(x = "Valores preditos para Glabela", y = "Resíduos") +
  theme_bw()

# Homocedasticidade: Breusch-Pagan
#H0:as variancias dos erros sao constantes (homocedastico)! Foi rejeitada a H0. Portanto, transformacao Box-Cox
#H1:as variancias dos erros Nao sao constantes
#alfa 5%
library(car)
ncvTest(modelo1)

# Relacao linear entre resposta e preditores é significativa?
summary(modelo1)$coef
#Rejeitamos H0 somente para beta0 e beta1. Ou seja, os outros betas sao iguais a zero

#Avaliando Falta de ajuste do modelo: tenho que ter replicacoes de preditores! Tenho varios do sexo fem no grupo 1, no grupo 2... mesmo para masculino
#H0:o modelo é razoavel
#H1:o modelo nao é razoavel
#alfa 5%
#library(EnvStats)
#anovaPE(modelo1)

# Transformação Box-Cox
library(MASS)
b1<-boxcox(modelo1)
lambda1 <- b1$x[which.max(b1$y)]
lambda1

#Na internet:
#((yi^lambda) - 1) / lambda) se lambda != 0
#log_e(y) se lambda = 0

# No livro:
#((yi^lambda) - 1)*k1) se lambda != 0
#(log_e(yi))*k2 se lambda = 0
#k1= 1/(lambda*(k2)^(lambda-1))
#k2=(produtorio yi)^1/n com i indo de 1 a n

dadosglabela$Glabela.boxcox <- (((dadosglabela$Glabela)^lambda1)-1)/lambda1

#Ajuste do novo modelo
modelo1.2 <- lm(Glabela.boxcox ~ Grupo + Sexo + Grupo*Sexo, data = dadosglabela)
anova(modelo1.2)
anova(modelo1)
summary(modelo1.2)
summary(modelo1)
fit1.2 <- anova(modelo1.2)
#normalidade
dadosglabela$residuos1.2 <- residuals(modelo1.2)
dadosglabela %>% ggplot(aes(sample = residuos1.2)) + 
  stat_qq(color = "blue") + 
  stat_qq_line(color = "red") + 
  labs(x = "residuos amostra", y = "teoricos") +
  theme_bw()

dadosglabela$res_padronizados1.2 <- dadosglabela$residuos1.2/sqrt(fit1.2$`Mean Sq`[4])
dadosglabela$pred1.2 <- fitted(modelo1.2)

dadosglabela %>% ggplot(aes(x = pred1.2, y = res_padronizados1.2, color = Sexo)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~Grupo) +
  labs(x = "Valores preditos para Glabela", y = "Resíduos") +
  theme_bw()

# Homocedasticidade: Breusch-Pagan
#H0:as variancias dos erros sao constantes (homocedastico)! Foi rejeitada a H0. Portanto, transformacao Box-Cox
#H1:as variancias dos erros Nao sao constantes
#alfa 5%
library(car)
ncvTest(modelo1.2)

#residuos padronizados versus niveis fator Sexo : residuos aparentemente simetricos/variancia dos erros um pouco menor no masc
dadosglabela %>% ggplot(aes(x = res_padronizados1.2, y = Sexo, color = Sexo)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Resíduos padronizados", y = "Sexo") +
  theme_bw()

#residuos padronizados versus niveis fator Grupo : residuos aparentemente simetricos/variancia dos erros um pouco menor no 7-14 anos
dadosglabela %>% ggplot(aes(x = res_padronizados1.2, y = Grupo, color = Grupo)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Resíduos padronizados", y = "Sexo") +
  theme_bw()

#residuos padronizados versus niveis fator GrupoxSexo : 
dadosglabela %>% ggplot(aes(x = res_padronizados1.2, y = Grupo, color = Sexo)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Resíduos padronizados", y = "Grupo") +
  facet_wrap(~Sexo) +
  theme_bw()

#Intervalo de confiança para a média:
xnew1.2 <- tibble(Grupo = c("7 a 11 anos", "7 a 11 anos", "12 a 18 anos", "12 a 18 anos"), Sexo = c("Feminino", "Masculino", "Feminino", "Masculino"))
xnew1.2
predict(modelo1, xnew1.2, interval = "confidence", level = 0.95)


#####################
# Modelo 2 - Nasio
#####################



#####################
# Modelo 3 - Rinio
#####################


#####################
# Modelo 4 - Filtro.medio
#####################



#####################
# Modelo 5 - Prostio
#####################



#####################
# Modelo 6 - Infradental
#####################




#####################
# Modelo 7 - Supramental
#####################


#####################
# Modelo 8 - Pogonio
#####################


#####################
# Modelo 9 - Mento
#####################


#####################
# Modelo 10 - Mid.Supraorbital.E
#####################


#####################
# Modelo 11 - Mid.Infraorbital.E
#####################



#####################
# Modelo 12 - Malar.E
#####################


#####################
# Modelo 13 - Lateral.orbita.E
#####################



#####################
# Modelo 14 - Zigio.E
#####################



#####################
# Modelo 15 - Supraglenoide.E
#####################


#####################
# Modelo 16 - Ectomolare1s.E
#####################


#####################
# Modelo 17 - Ectomolare2s.E
#####################


#####################
# Modelo 18 - Ectomolare1i.E
#####################


#####################
# Modelo 19 - Ectomolare2i.E
#####################


#####################
# Modelo 20 - Gonio.E
#####################

#####################
# Modelo 21 - Linha.oclusal.E
#####################

#####################
# Modelo 22 - Mid.ramus.E
#####################



#####################
# Modelo 23 - Gonio.D
#####################

#####################
# Modelo 24 - Linha.oclusal.D
#####################

#####################
# Modelo 25 - Mid.ramus.D
#####################


#####################
# Modelo 26 - Lateral.orbita.D
#####################



#####################
# Modelo 27 - Zigio.D
#####################



#####################
# Modelo 28 - Supraglenoide.D
#####################



#####################
# Modelo 29 - Ectomolare1s.D
#####################


#####################
# Modelo 30 - Ectomolare2s.D
#####################


#####################
# Modelo 31 - Ectomolare1i.D
#####################


#####################
# Modelo 32 - Ectomolare2i.D
#####################


#####################
# Modelo 33 - Mid.Supraorbital.D
#####################


#####################
# Modelo 34 - Mid.Infraorbital.D
#####################


#####################
# Modelo 35 - Malar.D
#####################





















# Anova tradicional
ok <- aov(Glabela ~ Grupo + Sexo + Grupo:Sexo, data = espessuras)
summary(ok)
# 1. homogeneidade
plot(ok, 1)
library(car)
leveneTest(Glabela ~ Grupo*Sexo, data = espessuras)
# 2. Normality
plot(ok, 2)
# 3. Extract the residuals
aov_residuals <- residuals(object = ok)
aov_residuals
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals)
## Unbalanced cases ***
library(car)
my_anova <- aov(Glabela ~ Grupo*Sexo, data = espessuras)
Anova(my_anova, type = "III")
summary(my_anova)











# ANOVA contexto da regressão
modelo1 <- lm(Glabela ~ Grupo + Sexo + Grupo*Sexo, data = espessuras)
fit <- anova(modelo1)
fit
summary(modelo1) #para achar coef do modelo

# Diagnóstico do modelo
#qqplot - normalidade
dadosglabela <- espessuras %>% select(Glabela, Grupo, Sexo) %>% drop_na()
dadosglabela$residuo <- residuals(modelo1)
dadosglabela %>% ggplot(aes(sample = residuo)) + 
  stat_qq(color = "blue") + 
  stat_qq_line(color = "red") + 
  theme_bw()

shapiro.test(x = dadosglabela$residuo)

dadosglabela$res_padronizados <- dadosglabela$residuo/sqrt(fit$`Mean Sq`[4])
dadosglabela %>% ggplot(aes(sample = res_padronizados)) + 
  stat_qq(color = "blue") + 
  stat_qq_line(color = "red") + 
  theme_bw()

#residuos padronizados versus niveis fator A
dadosglabela %>% ggplot(aes(x = Sexo, y = res_padronizados, color = Sexo)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Sexo", y = "Resíduos padronizados") +
  theme_bw()

#residuos padronizados versus niveis fator B
dadosglabela %>% ggplot(aes(x = Grupo, y = res_padronizados, color = Grupo)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Grupo", y = "Resíduos padronizados") +
  theme_bw()

#residuos versus fitted
dadosglabela$pred <- fitted(modelo1)
dadosglabela %>% ggplot(aes(x = pred, y = residuo, color = Sexo)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~Grupo) +
  labs(x = "Glabela", y = "Resíduos") +
  theme_bw()


# Homocedasticidade: Breusch-Pagan
#H0:as variancias dos erros sao constantes (homocedastico)
library(car)
ncvTest(modelo1)

# Transformação Box-Cox
library(MASS)
b <- boxcox(modelo1)
b
lambda <- b$x[which.max(b$y)]
lambda

#((y^lambda) - 1) / lambda) se lambda != 0
#log_e(y) se lambda = 0





#Novo modelo ajustado
dadosglabela$Glabela.boxcox <- dadosglabela$Glabela^lambda 
modelo2<-lm(Glabela.boxcox ~ Grupo + Sexo + Grupo*Sexo, data = dadosglabela)
anova(modelo2)
summary(modelo2)

#Normalidade
residuo2 <- residuals(modelo2)
shapiro.test(x = residuo2)
#Homocedast
library(car)
ncvTest(modelo2)

#Intervalo confianca dos estimadores betas
confint(modelo1)
library(broom)
library(knitr)
modelo1 %>% tidy(conf.int = TRUE) %>% kable(booktabs = TRUE)

#Intervalo confianca para a resposta média, considerando os dados
xnew <- data.frame(Glabela = c(4.87, 5.00, 5.09, 5.22),
                   Sexo = c("Feminino", "Masculino", "Feminino", "Masculino"),
                   Grupo = c("7 a 11 anos", "7 a 11 anos", "12 a 18 anos", "12 a 18 anos")) 
predict(modelo1, xnew, interval="confidence", level = 0.95)

# Teste para falta de ajuste
library(EnvStats)
anovaPE(modelo1)






# TRANSFORMAÇÃO log
#qqplot - normalidade
dados$tempo_log <-log(dados$tempo)

modelo.anova2 <- lm(tempo_log ~ papel*clips, data = dados)
fit2<-anova(modelo.anova2)

dados$residuos.2 <- residuals(modelo.anova2)
dados %>% ggplot(aes(sample = residuos.2)) + 
  stat_qq(color = "blue") + 
  stat_qq_line(color = "red") + 
  labs(x = "Quantis teóricos", y = "Quantis amostrais") +
  theme_bw()



# ANOVA NAO PARAMÉTRICA
# Transformação em postos

aov.rnk <- aov(rank(tempo) ~ papel*clips, data = dados, 
               contrasts = list(papel = 'contr.sum', clips = 'contr.sum'))

summary(aov.rnk, type = 'III')

# ou: (tanto faz)
aov.rnk.2 <- lm(rank(tempo) ~ papel*clips, data = dados, 
                contrasts = list(papel = 'contr.sum', clips = 'contr.sum'))

fit3<-anova(aov.rnk.2)
fit3

# Resíduos
#qqplot - normalidade
dados$res.np <- residuals(aov.rnk.2)
dados %>% ggplot(aes(sample = res.np)) + 
  stat_qq(color = "blue") + 
  stat_qq_line(color = "red") + 
  labs(x = "Quantis teóricos", y = "Quantis amostrais") +
  theme_bw()

dados$res_padronizados_np <- dados$res.np/sqrt(fit3$`Mean Sq`[4])
dados %>% ggplot(aes(sample = res_padronizados_np)) + 
  stat_qq(color = "blue") + 
  stat_qq_line(color = "red") + 
  theme_bw()

#residuos padronizados versus niveis fator A
dados %>% select(c(papel, res_padronizados_np)) %>% ggplot(aes(x = papel, y = res_padronizados_np, color = papel)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Tipo de papel", y = "Resíduos padronizados") +
  theme_bw()

#residuos padronizados versus niveis fator B
dados %>% select(c(clips, res_padronizados_np)) %>% ggplot(aes(x = clips, y = res_padronizados_np, color = clips)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Presença/ Ausência de clipe", y = "Resíduos padronizados") +
  theme_bw()


# POS-TESTE
TukeyHSD(aov.rnk)
plot(TukeyHSD(aov.rnk), las=1, col="red", cex.axis=0.2) 











