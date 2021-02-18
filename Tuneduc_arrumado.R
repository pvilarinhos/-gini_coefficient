# Colocar aqui o endereço que tiver com os dados da Prova Brasil
setwd("C:\\Users\\Pedro\\Documents\\Prova Brasil 2017\\DADOS") 


#Instalando pacotes
install.packages('reldist')
install.packages('scales')
install.packages('plotly')
install.packages('dplyr')
install.packages('tidyr')
install.packages('ggplot2')


#Carregando pacotes
library(plotly)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reldist)
library(scales)


#LENDO AS BASES
alunos5 <- read.csv("TS_ALUNO_5EF.csv", header = T, sep = ",") #BASE 5º ANO
alunos9 <- read.csv("TS_ALUNO_9EF.csv", header = T, sep = ",") #BASE 9º ANO
alunosEM <- read.csv("TS_ALUNO_3EM_ESC.csv", header = T, sep = ",") #BASE 3ª SÉRIE E.M.
#escolas <- read.csv("TS_ESCOLA.csv", header = T, sep = ",") 


#SELECIONANDO ALGUMAS VARIÁVEIS
alunos5 <- select(alunos5, c('ID_REGIAO','ID_UF','ID_MUNICIPIO','ID_ESCOLA','ID_DEPENDENCIA_ADM','ID_TURMA','ID_SERIE','ID_ALUNO','IN_PRESENCA_PROVA', 'PROFICIENCIA_LP', 'PROFICIENCIA_MT', 'PROFICIENCIA_LP_SAEB', 'PROFICIENCIA_MT_SAEB', 'ERRO_PADRAO_LP', 'ERRO_PADRAO_MT', 'ERRO_PADRAO_LP_SAEB', 'ERRO_PADRAO_MT_SAEB','ID_LOCALIZACAO'))
alunos9 <- select(alunos9, c('ID_REGIAO','ID_UF','ID_MUNICIPIO','ID_ESCOLA','ID_DEPENDENCIA_ADM','ID_TURMA','ID_SERIE','ID_ALUNO','IN_PRESENCA_PROVA', 'PROFICIENCIA_LP', 'PROFICIENCIA_MT', 'PROFICIENCIA_LP_SAEB', 'PROFICIENCIA_MT_SAEB', 'ERRO_PADRAO_LP', 'ERRO_PADRAO_MT', 'ERRO_PADRAO_LP_SAEB', 'ERRO_PADRAO_MT_SAEB','ID_LOCALIZACAO'))
alunosEM <- select(alunosEM, c('ID_REGIAO','ID_UF','ID_MUNICIPIO','ID_ESCOLA','ID_DEPENDENCIA_ADM','ID_TURMA','ID_SERIE','ID_ALUNO','IN_PRESENCA_PROVA', 'PROFICIENCIA_LP', 'PROFICIENCIA_MT', 'PROFICIENCIA_LP_SAEB', 'PROFICIENCIA_MT_SAEB', 'ERRO_PADRAO_LP', 'ERRO_PADRAO_MT', 'ERRO_PADRAO_LP_SAEB', 'ERRO_PADRAO_MT_SAEB','ID_LOCALIZACAO'))

#JUNTANDO TODOS OS ALUNOS
alunos <- rbind(alunos5, alunos9, alunosEM)

#RENOMEANDO AS VARIÁVEIS
alunos <- alunos%>%
  rename(regiao = ID_REGIAO,
         UF = ID_UF,
         municipio = ID_MUNICIPIO,
         rural = ID_LOCALIZACAO,
         escola = ID_ESCOLA,
         depadm = ID_DEPENDENCIA_ADM,
         turma = ID_TURMA,
         serie = ID_SERIE,
         aluno_id = ID_ALUNO,
         presenca = IN_PRESENCA_PROVA)

#SELECIONANDO APENAS OS QUE FIZERAM A PROVA E ALUNOS DA REDE PÚBLICA
alunos <- filter(alunos, presenca==1 & depadm!=4)

#EXCLUINDO OS MISSINGS
alunos <- na.omit(alunos)


# CRIANDO A BASE DE ESCOLAS (TODAS AS SÉRIES JUNTAS)
#Aqui, o gini é calculado para cada série já que os gráficos finais são separados pela série. 
#Ou seja, uma escola que tem turmas de 5º e 9º ano terá quatro ginis: Língua Portuguesa e Matemática (5º ano) e Língua Portuguesa e Matemática (9º ano)
escolas_gini <- alunos%>%
  group_by(UF, municipio,depadm, escola, serie)%>%
  summarise('alunos'=n(), 'turmas'= n_distinct(turma),'gini_lp'= gini(PROFICIENCIA_LP_SAEB),'gini_mt'= gini(PROFICIENCIA_MT_SAEB), 'media proficiencia lp'=mean(PROFICIENCIA_LP_SAEB), 'nota maxima lp' = max(PROFICIENCIA_LP_SAEB), 'nota mininma lp' = min(PROFICIENCIA_LP_SAEB),'desvio padrao lp'= sd(PROFICIENCIA_LP_SAEB),'media proficiencia mt'=mean(PROFICIENCIA_MT_SAEB), 'nota maxima mt'=max(PROFICIENCIA_MT_SAEB), 'nota minima mt' = min(PROFICIENCIA_MT_SAEB),'desvio padrao mt'= sd(PROFICIENCIA_MT_SAEB))

escolas_gini <- escolas_gini%>%
  mutate('alunos por turma' = alunos/turmas,
         'gini_adj_lp'= (alunos/(alunos-1))*gini_lp, #Calculando os ginis ajustados segundo o proposto por Deltas (2003)
         'gini_adj_mt'= (alunos/(alunos-1))*gini_mt)%>%
  mutate_at(vars('alunos por turma'), funs(round(.,0)))

escolas_gini<- select(escolas_gini, 1:turmas, 'alunos por turma', 'gini_adj_lp','gini_adj_mt',everything()) #organizando a ordem das colunas




#####################################################################################################################
### !!!!!!! AQUI, RODAR O SCRIPT "nomes_escolas.R" PARA CRIAR UMA TABELA COM OS NOMES DAS ESCOLAS !!!!!!!###############
########################################################################################################################





# MONTANDO AS BASES DE ESCOLA SEPARADAS POR SÉRIE
escolas_gini5 <- filter(escolas_gini, serie==5)
escolas_gini9 <- filter(escolas_gini, serie==9)
escolas_giniEM <- filter(escolas_gini, serie>=12)


# Apenas algumas visualizações
scatter.smooth(escolas_gini9$gini_lp, escolas_gini9$`media proficiencia lp`)
scatter.smooth(escolas_gini9$gini_mt, escolas_gini9$`media proficiencia mt`)

scatter.smooth(escolas_gini9$gini_lp, escolas_gini9$`alunos por turma`)
scatter.smooth(escolas_gini9$gini_mt, escolas_gini9$`alunos por turma`)


# MONTANDO A BASE POR TURMA (TODAS AS SÉRIES)
turmas_gini <- alunos%>%
  group_by(UF, municipio, depadm,turma, serie, escola)%>%
  summarise('alunos'=n(),'gini_lp'= gini(PROFICIENCIA_LP_SAEB),'gini_mt'= gini(PROFICIENCIA_MT_SAEB), 'media proficiencia lp'=mean(PROFICIENCIA_LP_SAEB), 'nota maxima lp' = max(PROFICIENCIA_LP_SAEB), 'nota mininma lp' = min(PROFICIENCIA_LP_SAEB),'desvio padrao lp'= sd(PROFICIENCIA_LP_SAEB),'media proficiencia mt'=mean(PROFICIENCIA_MT_SAEB), 'nota maxima mt'=max(PROFICIENCIA_MT_SAEB), 'nota minima mt' = min(PROFICIENCIA_MT_SAEB),'desvio padrao mt'= sd(PROFICIENCIA_MT_SAEB))%>%
  mutate('gini_adj_lp'= (alunos/(alunos-1))*gini_lp, #Calculando os ginis ajustados segundo o proposto por Deltas (2003)
         'gini_adj_mt'= (alunos/(alunos-1))*gini_mt)


turmas_gini<- select(turmas_gini, 1:alunos, 'gini_adj_lp', 'gini_adj_mt',everything())



# Exclui a tabela com os nomes das escolas
rm(nome_escola)

# MONTANDO A BASE POR TURMA (SEPARADA POR SÉRIE)
turmas_gini5 <- filter(turmas_gini, serie==5)
turmas_gini9 <- filter(turmas_gini, serie==9)
turmas_giniEM <- filter(turmas_gini, serie>=12)


# Apenas algumas visualizações
scatter.smooth(turmas_gini9$gini_lp, turmas_gini9$`media proficiencia lp`)
scatter.smooth(turmas_gini9$gini_mt, turmas_gini9$`media proficiencia mt`)

scatter.smooth(turmas_gini9$gini_lp, turmas_gini9$alunos)
scatter.smooth(turmas_gini9$`media proficiencia lp`, turmas_gini9$alunos)
scatter.smooth(turmas_gini9$gini_mt, turmas_gini9$alunos)



#(NÃO PRECISA RODAR) ############# FIZ ISSO SÓ PARA VERIFICAR A DIFERENÇA ENTRE OS GINI (escola) ###################
# Usei aqui só na parte de verificar o quanto mudou com o gini ajustado
valores_escola <- data.frame('gini_ajustado_lp_escola'=escolas_gini$gini_adj_lp, 
                             'gini_lp_escola'=escolas_gini$gini_lp,
                             'gini_ajustado_mt_escola'=escolas_gini$gini_adj_mt,
                             'gini_mt_escola'=escolas_gini$gini_mt)
valores_escola <- valores_escola%>%
  mutate(dif_gini_lp_escola = gini_ajustado_lp_escola - gini_lp_escola,
         dif_gini_mt_escola = gini_ajustado_mt_escola - gini_mt_escola)


#CALCULANDO A DIFERENÇA ENTRE OS GINI (turma)
valores_turma <- data.frame('gini_ajustado_lp_turma'=turmas_gini$gini_adj_lp, 
                            'gini_lp_turma'=turmas_gini$gini_lp,
                            'gini_ajustado_mt_turma'=turmas_gini$gini_adj_mt,
                            'gini_mt_turma'=turmas_gini$gini_mt)
valores_turma <- valores_turma%>%
  mutate(dif_gini_lp_turma = gini_ajustado_lp_turma - gini_lp_turma,
         dif_gini_mt_turma = gini_ajustado_mt_turma - gini_mt_turma)



#Densidade do gini ajustado x gini (escolas)
plot(density(escolas_gini$gini_adj_lp, na.rm = T), col = "Blue", main = "Densidade gini x gini ajustado LP")
lines(density(escolas_gini$gini_lp), col = "Red", title(main = "Densidade gini x gini ajustado LP"))
legend("topright", legend=c("Gini ajustado", "Gini"),
       col=c("blue", "red"), lty=1:2)


plot(density(escolas_gini$gini_adj_mt, na.rm = T), col = "Blue", main = "Densidade gini x gini ajustado MT")
lines(density(escolas_gini$gini_mt), col = "Red", title(main = "Densidade gini x gini ajustado MT"))
legend("topright", legend=c("Gini ajustado", "Gini"),
       col=c("blue", "red"), lty=1:2)


#Densidade do gini ajustado x gini (turmas)
plot(density(turmas_gini$gini_adj_lp, na.rm = T), col = "Blue", main = "Densidade gini x gini ajustado LP")
lines(density(turmas_gini$gini_lp), col = "Red", title(main = "Densidade gini x gini ajustado LP"))
legend("topright", legend=c("Gini ajustado", "Gini"),
       col=c("blue", "red"), lty=1:2)


plot(density(turmas_gini$gini_adj_mt, na.rm = T), col = "Blue", main = "Densidade gini x gini ajustado MT")
lines(density(turmas_gini$gini_mt), col = "Red", title(main = "Densidade gini x gini ajustado MT"))
legend("topright", legend=c("Gini ajustado", "Gini"),
       col=c("blue", "red"), lty=1:2)


############# GRÁFICOS DE NÚMERO DE ALUNOS X GINI (ESCOLA) ###############

# Gini (eixo x) e alunos (eixo y)
par(mfrow=c(2,2))
scatter.smooth(escolas_gini$gini_lp, escolas_gini$`alunos por turma`, ylim= range(0,60), xlim = range(0,0.35), col ="red", ylab = "alunos por turma", xlab = "gini LP")
scatter.smooth(escolas_gini$gini_adj_lp, escolas_gini$`alunos por turma`, ylim= range(0,60), xlim = range(0,0.35), col ="dark red", ylab = "alunos por turma", xlab = "gini ajustado LP")
scatter.smooth(escolas_gini$gini_mt, escolas_gini$`alunos por turma`, ylim= range(0,60), xlim = range(0,0.35), col ="light blue", ylab = "alunos por turma", xlab = "gini MT")
scatter.smooth(escolas_gini$gini_adj_mt, escolas_gini$`alunos por turma`, ylim= range(0,60), xlim = range(0,0.35), col ="dark blue", ylab = "alunos por turma", xlab = "gini ajustado MT")

# Alunos (eixo x) e Gini (eixo y)
par(mfrow=c(2,2))
scatter.smooth(escolas_gini$`alunos por turma`,escolas_gini$gini_lp,  xlim= range(0,120), ylim = range(0,0.6), col ="red", xlab = "alunos por turma", ylab = "gini LP")
scatter.smooth( escolas_gini$`alunos por turma`,escolas_gini$gini_adj_lp, xlim= range(0,120), ylim = range(0,0.6), col ="dark red", xlab = "alunos por turma", ylab = "gini ajustado LP")
scatter.smooth( escolas_gini$`alunos por turma`,escolas_gini$gini_mt, xlim= range(0,120), ylim = range(0,0.6), col ="light blue", xlab = "alunos por turma", ylab = "gini MT")
scatter.smooth( escolas_gini$`alunos por turma`,escolas_gini$gini_adj_mt, xlim= range(0,120), ylim = range(0,0.6), col ="dark blue", xlab = "alunos por turma", ylab = "gini ajustado MT")


########## GRÁFICOS DE NÚMERO DE ALUNOS X GINI (TURMA) ############

# Gini (eixo x) e alunos (eixo y)
par(mfrow=c(2,2))
scatter.smooth(turmas_gini$gini_lp, turmas_gini$alunos, ylim= range(0,70), xlim = range(0,0.35),  col ="red", ylab = "alunos por turma", xlab = "gini LP")
scatter.smooth(turmas_gini$gini_adj_lp, turmas_gini$alunos, ylim= range(0,70), xlim = range(0,0.35), col ="dark red", ylab = "alunos por turma", xlab = "gini ajustado LP")
scatter.smooth(turmas_gini$gini_mt, turmas_gini$alunos, ylim= range(0,70), xlim = range(0,0.35),  col ="blue", ylab = "alunos por turma", xlab = "gini MT")
scatter.smooth(turmas_gini$gini_adj_mt, turmas_gini$alunos, ylim= range(0,70), xlim = range(0,0.35), col ="dark blue", ylab = "alunos por turma", xlab = "gini ajustado MT")


# Alunos (eixo x) e Gini (eixo y)
par(mfrow=c(2,2))
scatter.smooth(turmas_gini$alunos,turmas_gini$gini_lp,  xlim= range(0,70), ylim = range(0,0.35),  col ="red", xlab = "alunos por turma", ylab = "gini LP")
scatter.smooth( turmas_gini$alunos,turmas_gini$gini_adj_lp, xlim= range(0,70), ylim = range(0,0.35), col ="dark red", xlab = "alunos por turma", ylab = "gini ajustado LP")
scatter.smooth(turmas_gini$alunos,turmas_gini$gini_mt,  xlim= range(0,70), ylim = range(0,0.35),  col ="blue", xlab = "alunos por turma", ylab = "gini MT")
scatter.smooth(turmas_gini$alunos,turmas_gini$gini_adj_mt,  xlim= range(0,70), ylim = range(0,0.35), col ="dark blue", xlab = "alunos por turma", ylab = "gini ajustado MT")


#-----------------------------------------------------------------------------------------------------------------------------------------------------------#

                      ########### BOOTSTRAP PARA DEFINIR ONDE FARÍAMOS O CORTE NO NÚMERO DE ALUNOS POR TURMA #############

## Selecionando uma turma (código 734294) com 45 alunos para realizar o bootstrap
x<- alunos%>%
  filter(turma==734294)%>%
  select(PROFICIENCIA_LP_SAEB)


# Definindo quais serão os tamanhos das amostras retiradas da turma. No total, serão 25 tamanhos diferentes.
tamanho_amostra <- c(1, 2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,25,30,35,40,45)


# Realizando o bootstrap com 100 repetições para cada tamanho de amostra especificado no vetor acima
set.seed(12345)
gini_final <- rep(NA, 25) #criando vetor para alocar os ginis calculados para cada tamanho de amostra
var_final <- rep(NA, 25)  #criando vetor para alocar as variâncias calculados para cada tamanho de amostra
eqm <- rep(NA, 25)        #criando vetor para alocar os erros quadráticos médios calculados para cada tamanho de amostra


for(i in seq_along(tamanho_amostra)){
  bSamples <-100
  
  bResults <- rep(NA, bSamples)
  
  sampleSize <- tamanho_amostra[i]
  
  for (b in seq_len(bSamples)) {
    bRows <- x[sample(nrow(x), sampleSize, replace = F), ]
    bValue <- gini(bRows)
      bResults[[b]] <- bValue
    bVar <- var(bResults)
  }
  
  gini_final[[i]]<- mean(bResults)
  var_final[[i]] <- bVar
  eqm[[i]] <- var_final[i] + (0.08543285 - gini_final[i])^2 # calculando o erro quadrático médio para cada estimação
  
}

var_final
gini_final

plot(tamanho_amostra, var_final) #fazendo o gráfico do tamanho da amostra x variâncias estimadas
plot(tamanho_amostra, eqm) #fazendo o gráfico do tamanho da amostra x EQM


###### Mesma coisa que o bootstrap acima (com a mesma turma), só que usando o gini de matemática #############

y<- alunos%>%
  filter(turma==734294)%>%
  select(PROFICIENCIA_MT_SAEB)

tamanho_amostra <- c(1, 2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,25,30,35,40,45)

set.seed(33333)
gini_final <- rep(NA, 25)
var_final <- rep(NA, 25)
eqm <- rep(NA, 25)


for(i in seq_along(tamanho_amostra)){
  bSamples <-100
  
  bResults <- rep(NA, bSamples)
  
  sampleSize<-tamanho_amostra[i]
  
  for (b in seq_len(bSamples)) {
    bRows <- y[sample(nrow(y), sampleSize, replace = F), ]
    bValue <- gini(bRows)
    # store results in container
    bResults[[b]] <- bValue
    bVar <- var(bResults)
  }
  
  gini_final[[i]]<- mean(bResults)
  var_final[[i]] <- bVar
  eqm[[i]] <- var_final[i] + (0.08985866 - gini_final[i])^2
}

var_final
gini_final

plot(tamanho_amostra, var_final)
plot(tamanho_amostra, eqm)


#------------------------------------------------------------------------------------------------------------------------------------------------------------#



################### GRÁFICOS POR ESCOLA #########################################

# Criando as bases por série para serem usadas na construção dos gráficos 
# Filtrando por turmas com pelo menos 10 alunos, da rede estadual (depadm==2) e por estado.
# Para filtrar os outros estados, trocar o último número de cada linha, após o UF== :
# PI: UF==22
# RN: UF==24
# PB: UF==25
# PE: UF==26
# ES: UF==32
# MS: UF==50
# GO: UF==52
base_5ef <- filter(escolas_gini5, `alunos por turma`>=10 & depadm ==2 & UF==32) 
base_9ef <- filter(escolas_gini9, `alunos por turma`>=10 & depadm ==2 & UF==32)
base_3em <- filter(escolas_giniEM, `alunos por turma`>=10 & depadm ==2 & UF==32)



################## GRÁFICOS 5o ANO ######################

#Gráfico de desigualdade x proficiência (Língua Portuguesa 5o ano)
dist_a <- summary(base_5ef$gini_adj_lp) #olhando os quartis da distribuição do gini ajustado de língua portuguesa
p<-base_5ef %>%
  mutate(quadrant =  case_when(gini_adj_lp<=dist_a[2] & `media proficiencia lp`>=275 ~ "Q9",
                               gini_adj_lp>dist_a[2] & gini_adj_lp <=dist_a[5] & `media proficiencia lp`>=275 ~ "Q8",
                               gini_adj_lp>dist_a[5] & `media proficiencia lp`>=275 ~ "Q7",
                               gini_adj_lp<=dist_a[2] & `media proficiencia lp`>=200 & `media proficiencia lp` < 275 ~ "Q6",
                               gini_adj_lp>dist_a[2] & gini_adj_lp <=dist_a[5] & `media proficiencia lp`>=200 & `media proficiencia lp` < 275 ~ "Q5",
                               gini_adj_lp>dist_a[5] &  `media proficiencia lp`>=200 & `media proficiencia lp` < 275 ~ "Q4",
                               gini_adj_lp<=dist_a[2] & `media proficiencia lp`<200  ~ "Q3",
                               gini_adj_lp>dist_a[2] & gini_adj_lp <=dist_a[5] & `media proficiencia lp`<200 ~ "Q2",
                               gini_adj_lp>dist_a[5] & `media proficiencia lp`<200 ~ "Q1"),
         'Desigualdade LP' = round(gini_adj_lp, 3),
         'Desigualdade MT' = round(gini_adj_mt, 3),
         'Proficiência média LP' = round(`media proficiencia lp`, 1),
         'Proficiência média MT' = round(`media proficiencia mt`, 1),
         'Escola' = nome_escola)

a <- ggplot(p, aes(`Desigualdade LP` , `Proficiência média LP`, color = quadrant, label = Escola))+
  geom_point(alpha = 0.9)+
  theme_minimal()+
  theme(plot.title=element_text(hjust = 0.5, face="bold"), legend.position = "none")+
  ylab('Proficiência')+
  xlab('Gini')+
  labs(title = "Gráfico de desigualdade x proficiência (Língua Portuguesa 5º ano EF)")+
  geom_vline(xintercept = dist_a[2])+
  geom_vline(xintercept = dist_a[5])+
  geom_hline(yintercept = 200)+
  geom_hline(yintercept = 275)+
  scale_color_manual(values = c(Q1 = 'red1',Q2 = 'orangered3',Q3 = 'darkorange3',Q4 = 'darkorange',Q5 = 'gold2',Q6 = 'yellow3',Q7 = 'green2',Q8 = 'green3',Q9 = 'green4'))

ggplotly(a, tooltip = c("x", "y", "label"))



#Gráfico de desigualdade x proficiência (Matemática 5o ano)
dist_b <- summary(base_5ef$gini_adj_mt) #olhando os quartis da distribuição do gini ajustado de matemática
p <- base_5ef %>%
  mutate(quadrant =  case_when(gini_adj_mt<=dist_b[2] & `media proficiencia mt`>=275 ~ "Q9",
                               gini_adj_mt>dist_b[2] & gini_adj_mt <=dist_b[5] & `media proficiencia mt`>=275 ~ "Q8",
                               gini_adj_mt>dist_b[5] & `media proficiencia mt`>=275 ~ "Q7",
                               gini_adj_mt<=dist_b[2] & `media proficiencia mt`>=200 & `media proficiencia mt` < 275 ~ "Q6",
                               gini_adj_mt>dist_b[2] & gini_adj_mt <=dist_b[5] & `media proficiencia mt`>=200 & `media proficiencia mt` < 275 ~ "Q5",
                               gini_adj_mt>dist_b[5] &  `media proficiencia mt`>=200 & `media proficiencia mt` < 275 ~ "Q4",
                               gini_adj_mt<=dist_b[2] & `media proficiencia mt`<200  ~ "Q3",
                               gini_adj_mt>dist_b[2] & gini_adj_mt <=dist_b[5] & `media proficiencia mt`<200 ~ "Q2",
                               gini_adj_mt>dist_b[5] & `media proficiencia mt`<200 ~ "Q1"),
         'Desigualdade LP' = round(gini_adj_lp, 3),
         'Desigualdade MT' = round(gini_adj_mt, 3),
         'Proficiência média LP' = round(`media proficiencia lp`, 1),
         'Proficiência média MT' = round(`media proficiencia mt`, 1),
         'Escola' = nome_escola)

b <-  ggplot(p, aes(`Desigualdade MT` , `Proficiência média MT`, color = quadrant, label = Escola))+
  geom_point(alpha = 0.9)+
  theme_minimal()+
  theme(plot.title=element_text(hjust = 0.5, face="bold"), legend.position = "none")+
  ylab('Proficiência')+
  xlab('Gini')+
  labs(title = "Gráfico de desigualdade x proficiência (Matemática 5º ano EF)")+
  geom_vline(xintercept = dist_b[2])+
  geom_vline(xintercept = dist_b[5])+
  geom_hline(yintercept = 200)+
  geom_hline(yintercept = 275)+
  scale_color_manual(values = c(Q1 = 'red1',Q2 = 'orangered3',Q3 = 'darkorange3',Q4 = 'darkorange',Q5 = 'gold2',Q6 = 'yellow3',Q7 = 'green2',Q8 = 'green3',Q9 = 'green4'))

ggplotly(b, tooltip = c("x", "y", "label"))


############### GRÁFICOS 9o ANO ##################


#Gráfico de desigualdade x proficiência (Língua Portuguesa 9o ano)
dist_c <- summary(base_9ef$gini_adj_lp) #olhando os quartis da distribuição do gini ajustado de língua portuguesa
p <- base_9ef %>%
  mutate(quadrant =  case_when(gini_adj_lp<=dist_c[2] & `media proficiencia lp`>=350 ~ "Q9",
                               gini_adj_lp>dist_c[2] & gini_adj_lp <=dist_c[5] & `media proficiencia lp`>=350 ~ "Q8",
                               gini_adj_lp>dist_c[5] & `media proficiencia lp`>=350 ~ "Q7",
                               gini_adj_lp<=dist_c[2] & `media proficiencia lp`>=275 & `media proficiencia lp` < 350 ~ "Q6",
                               gini_adj_lp>dist_c[2] & gini_adj_lp <=dist_c[5] & `media proficiencia lp`>=275 & `media proficiencia lp` < 350 ~ "Q5",
                               gini_adj_lp>dist_c[5] &  `media proficiencia lp`>=275 & `media proficiencia lp` < 350 ~ "Q4",
                               gini_adj_lp<=dist_c[2] & `media proficiencia lp`<275  ~ "Q3",
                               gini_adj_lp>dist_c[2] & gini_adj_lp <=dist_c[5] & `media proficiencia lp`<275 ~ "Q2",
                               gini_adj_lp>dist_c[5] & `media proficiencia lp`<275 ~ "Q1"),
         'Desigualdade LP' = round(gini_adj_lp, 3),
         'Desigualdade MT' = round(gini_adj_mt, 3),
         'Proficiência média LP' = round(`media proficiencia lp`, 1),
         'Proficiência média MT' = round(`media proficiencia mt`, 1),
         'Escola' = nome_escola)

c <- ggplot(p, aes(`Desigualdade LP` , `Proficiência média LP`, color = quadrant, label = Escola))+
  geom_point(alpha = 0.9)+
  theme_minimal()+
  theme(plot.title=element_text(hjust = 0.5, face="bold"), legend.position = "none")+
  ylab('Proficiência')+
  xlab('Gini')+
  labs(title = "Gráfico de desigualdade x proficiência (Língua Portuguesa 9º ano EF)")+
  geom_vline(xintercept = dist_c[2])+
  geom_vline(xintercept = dist_c[5])+
  geom_hline(yintercept = 275)+
  geom_hline(yintercept = 350)+
  scale_color_manual(values = c(Q1 = 'red1',Q2 = 'orangered3',Q3 = 'darkorange3',Q4 = 'darkorange',Q5 = 'gold2',Q6 = 'yellow3',Q7 = 'green2',Q8 = 'green3',Q9 = 'green4'))


ggplotly(c, tooltip = c("x", "y", "label"))



#Gráfico de desigualdade x proficiência (Matemática 9o ano)
dist_d <- summary(base_9ef$gini_adj_mt) #olhando os quartis da distribuição do gini ajustado de matemática
p <- base_9ef %>%
  mutate(quadrant =  case_when(gini_adj_mt<=dist_d[2] & `media proficiencia mt`>=350 ~ "Q9",
                               gini_adj_mt>dist_d[2] & gini_adj_mt <=dist_d[5] & `media proficiencia mt`>=350 ~ "Q8",
                               gini_adj_mt>dist_d[5] & `media proficiencia mt`>=350 ~ "Q7",
                               gini_adj_mt<=dist_d[2] & `media proficiencia mt`>=275 & `media proficiencia mt` < 350 ~ "Q6",
                               gini_adj_mt>dist_d[2] & gini_adj_mt <=dist_d[5] & `media proficiencia mt`>=275 & `media proficiencia mt` < 350 ~ "Q5",
                               gini_adj_mt>dist_d[5] &  `media proficiencia mt`>=275 & `media proficiencia mt` < 350 ~ "Q4",
                               gini_adj_mt<=dist_d[2] & `media proficiencia mt`<275  ~ "Q3",
                               gini_adj_mt>dist_d[2] & gini_adj_mt <=dist_d[5] & `media proficiencia mt`<275 ~ "Q2",
                               gini_adj_mt>dist_d[5] & `media proficiencia mt`<275 ~ "Q1"),
         'Desigualdade LP' = round(gini_adj_lp, 3),
         'Desigualdade MT' = round(gini_adj_mt, 3),
         'Proficiência média LP' = round(`media proficiencia lp`, 1),
         'Proficiência média MT' = round(`media proficiencia mt`, 1),
         'Escola' = nome_escola)

d <- ggplot(p, aes(`Desigualdade MT` , `Proficiência média MT`, color = quadrant, label = Escola))+
  geom_point(alpha = 0.9)+
  theme_minimal()+
  theme(plot.title=element_text(hjust = 0.5, face="bold"), legend.position = "none")+
  ylab('Proficiência')+
  xlab('Gini')+
  labs(title = "Gráfico de desigualdade x proficiência (Matemática 9º ano EF)")+
  geom_vline(xintercept = dist_d[2])+
  geom_vline(xintercept = dist_d[5])+
  geom_hline(yintercept = 275)+
  geom_hline(yintercept = 350)+
  scale_color_manual(values = c(Q1 = 'red1',Q2 = 'orangered3',Q3 = 'darkorange3',Q4 = 'darkorange',Q5 = 'gold2',Q6 = 'yellow3',Q7 = 'green2',Q8 = 'green3',Q9 = 'green4'))

ggplotly(d, tooltip = c("x", "y", "label"))



############### GRÁFICOS 3ª SERIE E.M. ##################

#Gráfico de desigualdade x proficiência (Língua Portuguesa 3ª série EM)
dist_e <- summary(base_3em$gini_adj_lp) #olhando os quartis da distribuição do gini ajustado de língua portuguesa
p <- base_3em %>%
  mutate(quadrant =  case_when(gini_adj_lp<=dist_e[2] & `media proficiencia lp`>=375 ~ "Q9",
                               gini_adj_lp>dist_e[2] & gini_adj_lp <=dist_e[5] & `media proficiencia lp`>=375 ~ "Q8",
                               gini_adj_lp>dist_e[5] & `media proficiencia lp`>=375 ~ "Q7",
                               gini_adj_lp<=dist_e[2] & `media proficiencia lp`>=300 & `media proficiencia lp` < 375 ~ "Q6",
                               gini_adj_lp>dist_e[2] & gini_adj_lp <=dist_e[5] & `media proficiencia lp`>=300 & `media proficiencia lp` < 375 ~ "Q5",
                               gini_adj_lp>dist_e[5] &  `media proficiencia lp`>=300 & `media proficiencia lp` < 375 ~ "Q4",
                               gini_adj_lp<=dist_e[2] & `media proficiencia lp`<300  ~ "Q3",
                               gini_adj_lp>dist_e[2] & gini_adj_lp <=dist_e[5] & `media proficiencia lp`<300 ~ "Q2",
                               gini_adj_lp>dist_e[5] & `media proficiencia lp`<300 ~ "Q1"),
         'Desigualdade LP' = round(gini_adj_lp, 3),
         'Desigualdade MT' = round(gini_adj_mt, 3),
         'Proficiência média LP' = round(`media proficiencia lp`, 1),
         'Proficiência média MT' = round(`media proficiencia mt`, 1),
         'Escola' = nome_escola)

e <- ggplot(p, aes(`Desigualdade LP` , `Proficiência média LP`, color = quadrant, label = Escola))+
  geom_point(alpha = 0.9)+
  theme_minimal()+
  theme(plot.title=element_text(hjust = 0.5, face="bold"), legend.position = "none")+
  ylab('Proficiência')+
  xlab('Gini')+
  labs(title = "Gráfico de desigualdade x proficiência (Língua Portuguesa 3ª série EM)")+
  geom_vline(xintercept = dist_e[2])+
  geom_vline(xintercept = dist_e[5])+
  geom_hline(yintercept = 300)+
  geom_hline(yintercept = 375)+
  scale_color_manual(values = c(Q1 = 'red1',Q2 = 'orangered3',Q3 = 'darkorange3',Q4 = 'darkorange',Q5 = 'gold2',Q6 = 'yellow3',Q7 = 'green2',Q8 = 'green3',Q9 = 'green4'))


ggplotly(e, tooltip = c("x", "y", "label"))



#Gráfico de desigualdade x proficiência (Matemática 3ª série EM)
dist_f <- summary(base_3em$gini_adj_mt) #olhando os quartis da distribuição do gini ajustado de matemática
p <- base_3em %>%
  mutate(quadrant =  case_when(gini_adj_mt<=dist_f[2] & `media proficiencia mt`>=375 ~ "Q9",
                               gini_adj_mt>dist_f[2] & gini_adj_mt <=dist_f[5] & `media proficiencia mt`>=375 ~ "Q8",
                               gini_adj_mt>dist_f[5] & `media proficiencia mt`>=375 ~ "Q7",
                               gini_adj_mt<=dist_f[2] & `media proficiencia mt`>=300 & `media proficiencia mt` < 375 ~ "Q6",
                               gini_adj_mt>dist_f[2] & gini_adj_mt <=dist_f[5] & `media proficiencia mt`>=300 & `media proficiencia mt` < 375 ~ "Q5",
                               gini_adj_mt>dist_f[5] &  `media proficiencia mt`>=300 & `media proficiencia mt` < 375 ~ "Q4",
                               gini_adj_mt<=dist_f[2] & `media proficiencia mt`<300  ~ "Q3",
                               gini_adj_mt>dist_f[2] & gini_adj_mt <=dist_f[5] & `media proficiencia mt`<300 ~ "Q2",
                               gini_adj_mt>dist_f[5] & `media proficiencia mt`<300 ~ "Q1"),
         'Desigualdade LP' = round(gini_adj_lp, 3),
         'Desigualdade MT' = round(gini_adj_mt, 3),
         'Proficiência média LP' = round(`media proficiencia lp`, 1),
         'Proficiência média MT' = round(`media proficiencia mt`, 1),
         'Escola' = nome_escola)

f <- ggplot(p, aes(`Desigualdade MT` , `Proficiência média MT`, color = quadrant, label = Escola))+
  geom_point(alpha = 0.9)+
  theme_minimal()+
  theme(plot.title=element_text(hjust = 0.5, face="bold"), legend.position = "none")+
  ylab('Proficiência')+
  xlab('Gini')+
  labs(title = "Gráfico de desigualdade x proficiência (Matemática 3ª série EM)")+
  geom_vline(xintercept = dist_f[2])+
  geom_vline(xintercept = dist_f[5])+
  geom_hline(yintercept = 300)+
  geom_hline(yintercept = 375)+
  scale_color_manual(values = c(Q1 = 'red1',Q2 = 'orangered3',Q3 = 'darkorange3',Q4 = 'darkorange',Q5 = 'gold2',Q6 = 'yellow3',Q7 = 'green2',Q8 = 'green3',Q9 = 'green4'))


ggplotly(f, tooltip = c("x", "y", "label"))



###################################### GRÁFICOS POR TURMA ##################################################

# Criando as bases por série para serem usadas na construção dos gráficos 
# Filtrando por turmas com pelo menos 10 alunos, da rede estadual (depadm==2) e por estado.
# Para filtrar os outros estados, trocar o último número de cada linha, após o UF== :
# PI: UF==22
# RN: UF==24
# PB: UF==25
# PE: UF==26
# ES: UF==32
# MS: UF==50
# GO: UF==52

base_turma_5ef <- filter(turmas_gini5, alunos>=10 & depadm == 2 & UF==32)
base_turma_9ef <- filter(turmas_gini9, alunos>=10 & depadm == 2 & UF==32)
base_turma_3em <- filter(turmas_giniEM, alunos>=10 & depadm == 2 & UF==32)



#Gráfico de desigualdade x proficiência (Língua Portuguesa 5o ano)
dist_g <- summary(base_turma_5ef$gini_adj_lp) #olhando os quartis da distribuição do gini ajustado de língua portuguesa
p<-base_turma_5ef %>%
  mutate(quadrant =  case_when(gini_adj_lp<=dist_g[2] & `media proficiencia lp`>=275 ~ "Q9",
                               gini_adj_lp>dist_g[2] & gini_adj_lp <=dist_g[5] & `media proficiencia lp`>=275 ~ "Q8",
                               gini_adj_lp>dist_g[5] & `media proficiencia lp`>=275 ~ "Q7",
                               gini_adj_lp<=dist_g[2] & `media proficiencia lp`>=200 & `media proficiencia lp` < 275 ~ "Q6",
                               gini_adj_lp>dist_g[2] & gini_adj_lp <=dist_g[5] & `media proficiencia lp`>=200 & `media proficiencia lp` < 275 ~ "Q5",
                               gini_adj_lp>dist_g[5] &  `media proficiencia lp`>=200 & `media proficiencia lp` < 275 ~ "Q4",
                               gini_adj_lp<=dist_g[2] & `media proficiencia lp`<200  ~ "Q3",
                               gini_adj_lp>dist_g[2] & gini_adj_lp <=dist_g[5] & `media proficiencia lp`<200 ~ "Q2",
                               gini_adj_lp>dist_g[5] & `media proficiencia lp`<200 ~ "Q1"),
         'Desigualdade LP' = round(gini_adj_lp, 3),
         'Desigualdade MT' = round(gini_adj_mt, 3),
         'Proficiência média LP' = round(`media proficiencia lp`, 1),
         'Proficiência média MT' = round(`media proficiencia mt`, 1),
         'Escola' = nome_escola)

g <- ggplot(p, aes(`Desigualdade LP` , `Proficiência média LP`, color = quadrant, label = Escola))+
  geom_point(alpha = 0.9)+
  theme_minimal()+
  theme(plot.title=element_text(hjust = 0.5, face="bold"), legend.position = "none")+
  ylab('Proficiência')+
  xlab('Gini')+
  labs(title = "Gráfico de desigualdade x proficiência (Língua Portuguesa 5º ano EF)")+
  geom_vline(xintercept = dist_g[2])+
  geom_vline(xintercept = dist_g[5])+
  geom_hline(yintercept = 200)+
  geom_hline(yintercept = 275)+
  scale_color_manual(values = c(Q1 = 'red1',Q2 = 'orangered3',Q3 = 'darkorange3',Q4 = 'darkorange',Q5 = 'gold2',Q6 = 'yellow3',Q7 = 'green2',Q8 = 'green3',Q9 = 'green4'))

ggplotly(g, tooltip = c("x", "y", "label"))




#Gráfico de desigualdade x proficiência (Matemática 5o ano)
dist_h <- summary(base_turma_5ef$gini_adj_mt) #olhando os quartis da distribuição do gini ajustado de matemática
p <- base_turma_5ef %>%
  mutate(quadrant =  case_when(gini_adj_mt<=dist_h[2] & `media proficiencia mt`>=275 ~ "Q9",
                               gini_adj_mt>dist_h[2] & gini_adj_mt <=dist_h[5] & `media proficiencia mt`>=275 ~ "Q8",
                               gini_adj_mt>dist_h[5] & `media proficiencia mt`>=275 ~ "Q7",
                               gini_adj_mt<=dist_h[2] & `media proficiencia mt`>=200 & `media proficiencia mt` < 275 ~ "Q6",
                               gini_adj_mt>dist_h[2] & gini_adj_mt <=dist_h[5] & `media proficiencia mt`>=200 & `media proficiencia mt` < 275 ~ "Q5",
                               gini_adj_mt>dist_h[5] &  `media proficiencia mt`>=200 & `media proficiencia mt` < 275 ~ "Q4",
                               gini_adj_mt<=dist_h[2] & `media proficiencia mt`<200  ~ "Q3",
                               gini_adj_mt>dist_h[2] & gini_adj_mt <=dist_h[5] & `media proficiencia mt`<200 ~ "Q2",
                               gini_adj_mt>dist_h[5] & `media proficiencia mt`<200 ~ "Q1"),
         'Desigualdade LP' = round(gini_adj_lp, 3),
         'Desigualdade MT' = round(gini_adj_mt, 3),
         'Proficiência média LP' = round(`media proficiencia lp`, 1),
         'Proficiência média MT' = round(`media proficiencia mt`, 1),
         'Escola' = nome_escola)

h <-  ggplot(p, aes(`Desigualdade MT` , `Proficiência média MT`, color = quadrant, label = Escola))+
  geom_point(alpha = 0.9)+
  theme_minimal()+
  theme(plot.title=element_text(hjust = 0.5, face="bold"), legend.position = "none")+
  ylab('Proficiência')+
  xlab('Gini')+
  labs(title = "Gráfico de desigualdade x proficiência (Matemática 5º ano EF)")+
  geom_vline(xintercept = dist_h[2])+
  geom_vline(xintercept = dist_h[5])+
  geom_hline(yintercept = 200)+
  geom_hline(yintercept = 275)+
  scale_color_manual(values = c(Q1 = 'red1',Q2 = 'orangered3',Q3 = 'darkorange3',Q4 = 'darkorange',Q5 = 'gold2',Q6 = 'yellow3',Q7 = 'green2',Q8 = 'green3',Q9 = 'green4'))

ggplotly(h, tooltip = c("x", "y", "label"))


############### GRÁFICOS 9o ANO ##################

#Gráfico de desigualdade x proficiência (Língua Portuguesa 9o ano)
dist_i <- summary(base_turma_9ef$gini_adj_lp) #olhando os quartis da distribuição do gini ajustado de língua portuguesa
p <- base_turma_9ef %>%
  mutate(quadrant =  case_when(gini_adj_lp<=dist_i[2] & `media proficiencia lp`>=350 ~ "Q9",
                               gini_adj_lp>dist_i[2] & gini_adj_lp <=dist_i[5] & `media proficiencia lp`>=350 ~ "Q8",
                               gini_adj_lp>dist_i[5] & `media proficiencia lp`>=350 ~ "Q7",
                               gini_adj_lp<=dist_i[2] & `media proficiencia lp`>=275 & `media proficiencia lp` < 350 ~ "Q6",
                               gini_adj_lp>dist_i[2] & gini_adj_lp <=dist_i[5] & `media proficiencia lp`>=275 & `media proficiencia lp` < 350 ~ "Q5",
                               gini_adj_lp>dist_i[5] &  `media proficiencia lp`>=275 & `media proficiencia lp` < 350 ~ "Q4",
                               gini_adj_lp<=dist_i[2] & `media proficiencia lp`<275  ~ "Q3",
                               gini_adj_lp>dist_i[2] & gini_adj_lp <=dist_i[5] & `media proficiencia lp`<275 ~ "Q2",
                               gini_adj_lp>dist_i[5] & `media proficiencia lp`<275 ~ "Q1"),
         'Desigualdade LP' = round(gini_adj_lp, 3),
         'Desigualdade MT' = round(gini_adj_mt, 3),
         'Proficiência média LP' = round(`media proficiencia lp`, 1),
         'Proficiência média MT' = round(`media proficiencia mt`, 1),
         'Escola' = nome_escola)

i <- ggplot(p, aes(`Desigualdade LP` , `Proficiência média LP`, color = quadrant, label = Escola))+
  geom_point(alpha = 0.9)+
  theme_minimal()+
  theme(plot.title=element_text(hjust = 0.5, face="bold"), legend.position = "none")+
  ylab('Proficiência')+
  xlab('Gini')+
  labs(title = "Gráfico de desigualdade x proficiência (Língua Portuguesa 9º ano EF)")+
  geom_vline(xintercept = dist_i[2])+
  geom_vline(xintercept = dist_i[5])+
  geom_hline(yintercept = 275)+
  geom_hline(yintercept = 350)+
  scale_color_manual(values = c(Q1 = 'red1',Q2 = 'orangered3',Q3 = 'darkorange3',Q4 = 'darkorange',Q5 = 'gold2',Q6 = 'yellow3',Q7 = 'green2',Q8 = 'green3',Q9 = 'green4'))

ggplotly(i, tooltip = c("x", "y", "label"))



#Gráfico de desigualdade x proficiência (Matemática 9o ano)
dist_j <- summary(base_turma_9ef$gini_adj_mt) #olhando os quartis da distribuição do gini ajustado de matemática
p <- base_turma_9ef %>%
  mutate(quadrant =  case_when(gini_adj_mt<=dist_j[2] & `media proficiencia mt`>=350 ~ "Q9",
                               gini_adj_mt>dist_j[2] & gini_adj_mt <=dist_j[5] & `media proficiencia mt`>=350 ~ "Q8",
                               gini_adj_mt>dist_j[5] & `media proficiencia mt`>=350 ~ "Q7",
                               gini_adj_mt<=dist_j[2] & `media proficiencia mt`>=275 & `media proficiencia mt` < 350 ~ "Q6",
                               gini_adj_mt>dist_j[2] & gini_adj_mt <=dist_j[5] & `media proficiencia mt`>=275 & `media proficiencia mt` < 350 ~ "Q5",
                               gini_adj_mt>dist_j[5] &  `media proficiencia mt`>=275 & `media proficiencia mt` < 350 ~ "Q4",
                               gini_adj_mt<=dist_j[2] & `media proficiencia mt`<275  ~ "Q3",
                               gini_adj_mt>dist_j[2] & gini_adj_mt <=dist_j[5] & `media proficiencia mt`<275 ~ "Q2",
                               gini_adj_mt>dist_j[5] & `media proficiencia mt`<275 ~ "Q1"),
         'Desigualdade LP' = round(gini_adj_lp, 3),
         'Desigualdade MT' = round(gini_adj_mt, 3),
         'Proficiência média LP' = round(`media proficiencia lp`, 1),
         'Proficiência média MT' = round(`media proficiencia mt`, 1),
         'Escola' = nome_escola)

j <- ggplot(p, aes(`Desigualdade MT` , `Proficiência média MT`, color = quadrant, label = Escola))+
  geom_point(alpha = 0.9)+
  theme_minimal()+
  theme(plot.title=element_text(hjust = 0.5, face="bold"), legend.position = "none")+
  ylab('Proficiência')+
  xlab('Gini')+
  labs(title = "Gráfico de desigualdade x proficiência (Matemática 9º ano EF)")+
  geom_vline(xintercept = dist_j[2])+
  geom_vline(xintercept = dist_j[5])+
  geom_hline(yintercept = 275)+
  geom_hline(yintercept = 350)+
  scale_color_manual(values = c(Q1 = 'red1',Q2 = 'orangered3',Q3 = 'darkorange3',Q4 = 'darkorange',Q5 = 'gold2',Q6 = 'yellow3',Q7 = 'green2',Q8 = 'green3',Q9 = 'green4'))

ggplotly(j, tooltip = c("x", "y", "label"))



############### GRÁFICOS 3ª SERIE E.M. ##################

#Gráfico de desigualdade x proficiência (Língua Portuguesa 3ª série EM)
dist_k <- summary(base_turma_3em$gini_adj_lp) #olhando os quartis da distribuição do gini ajustado de língua portuguesa
p <- base_turma_3em %>%
  mutate(quadrant =  case_when(gini_adj_lp<=dist_k[2] & `media proficiencia lp`>=375 ~ "Q9",
                               gini_adj_lp>dist_k[2] & gini_adj_lp <=dist_k[5] & `media proficiencia lp`>=375 ~ "Q8",
                               gini_adj_lp>dist_k[5] & `media proficiencia lp`>=375 ~ "Q7",
                               gini_adj_lp<=dist_k[2] & `media proficiencia lp`>=300 & `media proficiencia lp` < 375 ~ "Q6",
                               gini_adj_lp>dist_k[2] & gini_adj_lp <=dist_k[5] & `media proficiencia lp`>=300 & `media proficiencia lp` < 375 ~ "Q5",
                               gini_adj_lp>dist_k[5] &  `media proficiencia lp`>=300 & `media proficiencia lp` < 375 ~ "Q4",
                               gini_adj_lp<=dist_k[2] & `media proficiencia lp`<300  ~ "Q3",
                               gini_adj_lp>dist_k[2] & gini_adj_lp <=dist_k[5] & `media proficiencia lp`<300 ~ "Q2",
                               gini_adj_lp>dist_k[5] & `media proficiencia lp`<300 ~ "Q1"),
         'Desigualdade LP' = round(gini_adj_lp, 3),
         'Desigualdade MT' = round(gini_adj_mt, 3),
         'Proficiência média LP' = round(`media proficiencia lp`, 1),
         'Proficiência média MT' = round(`media proficiencia mt`, 1),
         'Escola' = nome_escola)

k <- ggplot(p, aes(`Desigualdade LP` , `Proficiência média LP`, color = quadrant, label = Escola))+
  geom_point(alpha = 0.9)+
  theme_minimal()+
  theme(plot.title=element_text(hjust = 0.5, face="bold"), legend.position = "none")+
  ylab('Proficiência')+
  xlab('Gini')+
  labs(title = "Gráfico de desigualdade x proficiência (Língua Portuguesa 3ª série EM)")+
  geom_vline(xintercept = dist_k[2])+
  geom_vline(xintercept = dist_k[5])+
  geom_hline(yintercept = 300)+
  geom_hline(yintercept = 375)+
  scale_color_manual(values = c(Q1 = 'red1',Q2 = 'orangered3',Q3 = 'darkorange3',Q4 = 'darkorange',Q5 = 'gold2',Q6 = 'yellow3',Q7 = 'green2',Q8 = 'green3',Q9 = 'green4'))

ggplotly(k, tooltip = c("x", "y", "label"))



#Gráfico de desigualdade x proficiência (Matemática 3ª série EM)
dist_l <- summary(base_turma_3em$gini_adj_mt) #olhando os quartis da distribuição do gini ajustado de matemática
p <- base_turma_3em %>%
  mutate(quadrant =  case_when(gini_adj_mt<=dist_l[2] & `media proficiencia mt`>=375 ~ "Q9",
                               gini_adj_mt>dist_l[2] & gini_adj_mt <=dist_l[5] & `media proficiencia mt`>=375 ~ "Q8",
                               gini_adj_mt>dist_l[5] & `media proficiencia mt`>=375 ~ "Q7",
                               gini_adj_mt<=dist_l[2] & `media proficiencia mt`>=300 & `media proficiencia mt` < 375 ~ "Q6",
                               gini_adj_mt>dist_l[2] & gini_adj_mt <=dist_l[5] & `media proficiencia mt`>=300 & `media proficiencia mt` < 375 ~ "Q5",
                               gini_adj_mt>dist_l[5] &  `media proficiencia mt`>=300 & `media proficiencia mt` < 375 ~ "Q4",
                               gini_adj_mt<=dist_l[2] & `media proficiencia mt`<300  ~ "Q3",
                               gini_adj_mt>dist_l[2] & gini_adj_mt <=dist_l[5] & `media proficiencia mt`<300 ~ "Q2",
                               gini_adj_mt>dist_l[5] & `media proficiencia mt`<300 ~ "Q1"),
         'Desigualdade LP' = round(gini_adj_lp, 3),
         'Desigualdade MT' = round(gini_adj_mt, 3),
         'Proficiência média LP' = round(`media proficiencia lp`, 1),
         'Proficiência média MT' = round(`media proficiencia mt`, 1),
         'Escola' = nome_escola)

l <- ggplot(p, aes(`Desigualdade MT` , `Proficiência média MT`, color = quadrant, label = Escola))+
  geom_point(alpha = 0.9)+
  theme_minimal()+
  theme(plot.title=element_text(hjust = 0.5, face="bold"), legend.position = "none")+
  ylab('Proficiência')+
  xlab('Gini')+
  labs(title = "Gráfico de desigualdade x proficiência (Matemática 3ª série EM)")+
  geom_vline(xintercept = dist_l[2])+
  geom_vline(xintercept = dist_l[5])+
  geom_hline(yintercept = 300)+
  geom_hline(yintercept = 375)+
  scale_color_manual(values = c(Q1 = 'red1',Q2 = 'orangered3',Q3 = 'darkorange3',Q4 = 'darkorange',Q5 = 'gold2',Q6 = 'yellow3',Q7 = 'green2',Q8 = 'green3',Q9 = 'green4'))


ggplotly(l, tooltip = c("x", "y", "label"))
