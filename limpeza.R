library(readxl)
library(dplyr)
library(data.table)

#1. Leitura dos dados originais do excel:
dados <- read_excel("../Banco estudantes 29-08-19 completo (2).xlsx",
                    sheet = "Plan1",na = c("999",999,99,9999,"9999","99"))

#1.1 Filtrar apenas os dados de Limeira:
data<-dados%>%filter(campus!="Campinas",campus!="Piracicaba")
data<-as.data.frame(data)

#2. Leitura dos dados "corrigidos" somente de Limeira:
pronto<-read.csv2("dadosLimeiratotal.csv",na.strings = c(999,"999","NA","777",777))

#2.1 Filtro para remover as observações contendo algum NAE:
nae<-pronto%>%filter_all(any_vars(. %in% c("NAE")))
pronto<-anti_join(pronto,nae)

#Arrumando os tipos das variáveis:
#numericas
cols = c(1,3,8,12,13,32:35,52,54,58,61,
         63,104:110,117:124,139,292:295,
         297,349,352,356,364,370:389,392:395,
         397,401,405,408:410,419:421,471,473,
         474,483,485,486,565,566,603,604,610,
         608,614,615,617,630,634,641,640,642,
         653,648,53,59,72,73,75,98)

pronto[,cols] = apply(pronto[,cols], 2, function(x) as.numeric(as.character(x)))

pronto[,-cols] = apply(pronto[,-cols], 2, function(x) as.character(x))

#de texto
colschar<-c(10,11,16,21:23,26:28,36,37,39,66,
            69,91,95,96,99,103,111,125,127,
            133,135,137,140,143,145,151,154,
            156,165,169,171,174:177,184,185,
            186,227:229,231,233,235,248,256,
            263,265,267,270,271,273,275,
            278:281,287,290,298,303,304,308,
            331,334,336,338,340,341,343,346,
            348,351,354,355,363,365,367,369,
            400,404,407,411,416,417,421,422,
            424,426,428,430,432:434,445,495,
            505,506,512,563,568,573,582,590,
            600,626,631,633,635,649,652,655,656)

#fator
all<-c(1:length(pronto))
'%ni%' <- Negate('%in%')
facts<-all[all %ni% colschar]
facts<-facts[facts %ni% cols]

pronto[,facts] = lapply(pronto[,facts], function(x) as.factor(x))

#Remover as variáveis de texto (não usaremos para as análises aqui):
pronto[,colschar]<-NULL

#Criar um filtro para as variáveis com mais de x% de valores faltantes:
pronto<-as.data.table(pronto)

na_rate <- as.vector(
  pronto[, sapply(.SD, function(x) sum(is.na(x))) / .N]
)

#Ao longo do questionário, os valores faltantes foram aumentando??
#Sem contar os erros...
faltantes<-ts(na_rate[-c(1,3:6)],start = 1,frequency = 1)
plot((faltantes)*100,
     ylim=c(0,100),
     xlim=c(1,540),
     ylab="Porcentagem",
     xlab="Número da Questão",
     main="Porcentagem de Valores Faltantes\npor Questão")

idx <- 0
for (col_name in names(pronto)) {
  idx <- idx + 1
  if (na_rate[idx] > 0.8) {
    pronto[, (col_name)]<-NULL
  }
}

#Criando a variável de score mental:
pronto2<-as.data.table(pronto)

#135a até 135t:
pronto2<-pronto2[,lapply(.SD,function(x){ifelse(is.na(x),0,x)}),.SDcols=c(219:238)]
pronto2<-pronto2[,lapply(.SD,function(x){ifelse(x==1,0,x)})]
pronto2<-pronto2[,lapply(.SD,function(x){ifelse(x==2,1,x)})]

Score<-pronto2%>%as_tibble()%>%
               mutate(Score_Mental = rowSums(.))%>%
               select(Score_Mental)

pronto$Score_Mental<-Score

#TIPOS:
library(knitr)
library(magrittr)
tipos<-data.frame(variavel = names(pronto),
                  tipo= c("dependente"),
                  classe = sapply(pronto, class),
                  possiveis = sapply(pronto, function(x) paste0(unique(x),  collapse = "# ")),
                  row.names = NULL)

tipos<-as_tibble(tipos)
tipos$tipo<-as.character(tipos$tipo)
tipos[217,2]<-"resposta"
write.csv(tipos,"tipos.csv")

#Exportando a base
write.csv2(pronto,"dadosfinal.csv",row.names = F)
