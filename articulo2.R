#articulo2
data <- read.csv("C:/Users/ALEN/OneDrive/Documentos/articulo1/articulo2/covid19cancer/covid-liver.csv")
summary(data)
library(modeest)
library(multimode)
#Obtenemos las modas
mlv(data$Month,method = "mfv")
mlv(data$Age,method = "mfv")
mlv(data$Size,method = "mfv",na.omit(data$Size))
mlv(data$Survival_fromMDM,method = "mfv")
mlv(data$PS,method = "mfv")

library(e1071)

coef_var = function(x){
  sd(x,na.rm = T) / mean(x,na.rm = T)
}
var(data$Month,na.rm = T)
sd(data$Month,na.rm = T)
coef_var(data$Month)

var(data$Age,na.rm = T)
sd(data$Age,na.rm = T)
coef_var(data$Age)

var(data$Size,na.rm = T)
sd(data$Size,na.rm = T)
coef_var(data$Size)

var(data$Survival_fromMDM,na.rm = T)
sd(data$Survival_fromMDM,na.rm = T)
coef_var(data$Survival_fromMDM)


var(data$PS,na.rm = T)
sd(data$PS,na.rm = T)
coef_var(data$PS)

checktype<-function(datos){
  for(i in 1:ncol(datos))
    print(paste("columna",colnames(datos[i]),"-",class(datos[,i])))
  
}
checktype(data)

buscaNaN<-function(data2){
  for(i in 1:ncol(data2)){
    vectorcito<-data2[,i]
    sumaNaN<-sum(is.na(vectorcito))
    #Para saber el nombre de la columna, vamos a usar
    #colnames(Dataset)
    if(sumaNaN>0)
      print(paste("Columna", i,"-",colnames(data2)[i], "Tiene", sumaNaN))
    
    
  }
}

buscaNaN(data)
nrow(data)

# Cáncer: Bandera de cáncer [S/N]
# Año: Categórico [Prepandemia (marzo de 2019 a febrero de 2020)/pandémica (marzo de 2020 a febrero de 2021)]
# Mes: Mes del año 1-12
# Sangrado: hemorragia tumoral espontánea [S/N]
# Modo Presentación: Vigilancia, Incidental o Sintomático
# Edad: Edad del paciente
# Género: Masculino o Femenino [M/F]
# Etiología: forma de causalidad de una enfermedad o condición. Ya sea "CLD no establecida" (enfermedad hepática crónica), "ARLD" (enfermedad hepática relacionada con el alcohol), "NAFLD" (enfermedad del hígado graso no alcohólico), "HCV" (virus de la hepatitis C), "HH" (hemocromatosis hereditaria ), "PBC/AIH" (colangitis biliar primaria/hepatitis autoinmune), "HBV" (virus de la hepatitis B) u "Otro".
# Cirrosis: enfermedad hepática subyacente [S/N]
# Tamaño: diámetro del tumor en mm Size
# Etapa HCC TNM: Carcinoma hepatocelular: Etapa de metástasis en ganglios tumorales ("I", "II", "IIIA+IIIB", "IV") Hepatocellular carcinoma Tumour node metastasis Stage 
# HCC BCLC Etapas: Carcinoma hepatocelular con aproximación de barcelona ("0", "A", "B", "C", "D") Hepatocellular carcinoma Barcelona Clinic for Liver Cancer Stage
# Etapas ICC TNM: colangiocarcinoma intrahepático Estadio de metástasis del ganglio tumoral ("I", "II", "III", "IV")
# Grupos de tratamiento: Tratamiento de primera línea recibido ["OLTx" (trasplante hepático ortotópico), "Resección", "Ablación", "TACE"" (quimioembolización transarterial), "SIRT" (radioterapia interna selectiva), "Médico", "Cuidados de apoyo"]
# Supervivencia desde MDM: Supervivencia desde reunión multidisciplinar
# Vivo Muerto: "Vivo", "Muerto"
# Tipo de hallazgo incidental: ("Atención primaria-rutinaria", "Atención secundaria-rutina", "Atención primaria-aguda", "Atención secundaria-aguda")
# Programa de vigilancia: Paciente en un programa de vigilancia formal ("Y", "N")
# Efectividad de la vigilancia: Cumplimiento de la vigilancia con respecto al año anterior ("Consistente", "Inconsistente", "Perdido")
# Modo de detección de vigilancia: Modo de prueba de vigilancia de incidentes ["US" (ultrasonido), "AFP sola" (alfa-fetoproteína sola), "CT/MRI"]
# Tiempo diagnóstico 1er Tx:
# Fecha de escaneo de vigilancia del incidente: ("Y", "N")
# PD: estado de rendimiento [0, 1, 2, 3, 4]
# Tiempo MDM 1er tratamiento: Tiempo hasta reunión multidisciplinar 1er tratamiento
# Decisión de tiempo para tratar el 1er tratamiento: Decisión de tiempo para tratar el 1er tratamiento
# Cirrosis anterior conocida: ["Y", "N"]
# Meses desde la última vigilancia: Meses desde la última vigilancia


#Eliminamos las variables que tienen demasiados na

drop<-names(data) %in% c('ICC_TNM_Stage','Type_of_incidental_finding','Surveillance_effectiveness',
                         'Mode_of_surveillance_detection','Time_diagnosis_1st_Tx',
                         'Date_incident_surveillance_scan','Time_MDM_1st_treatment',
                         'Time_decisiontotreat_1st_treatment','Months_from_last_surveillance')

# data=na.omit(data)
data2<-data[,!drop]


# ICC_TNM_Stage 311,Type_of_incidental_finding 326,
# 'Surveillance_effectiveness'333,
# 'Mode_of_surveillance_detection' 352,
# 'Time_diagnosis_1st_Tx'292,
# 'Date_incident_surveillance_scan' 417,
# 'Time_MDM_1st_treatment' 288,
# 'Time_decisiontotreat_1st_treatment' 343,
# 'Months_from_last_surveillance' 338



checktype(data2)

colores <- c("Indianred1","lightslateblue", "yellow", "blue", "orange", "brown",'green','red','cyan','black','')
hist(data2$Survival_fromMDM) 



t1=table(data2$Cancer)

barp<-barplot(t1,main='Cancer',
        col=c("darkblue","green"),           #H) Colorear las barras
        density = c(5, 10),             #I) Para dar intensidades dentro de las barras
        angle = c(45, 90),               #J) Para el ángulo de la intensidad 
        ylim = c(0, 400) 
        )

legend("topleft",                       #M) Leyenda en parte superior izquierda
       c("No","Yes"),              #N) Las leyendas
       fill = c("darkblue","green"),         #O) Colores de las leyendas 
       title = "Cancer",                #P) Título de la leyenda]
       text.font = 1,                   #Q) Fuente del texto de la leyenda
       bg='lightblue'                   #R) Color del fondo de la caja de la leyenda
)
text(barp, t1+20, labels = t1)



t2=table(data2$Year)
bar2<-barplot(t2,main='Year',
        col=c("darkblue","green"),           #H) Colorear las barras
        density = c(5, 10),             #I) Para dar intensidades dentro de las barras
        angle = c(45, 90),               #J) Para el ángulo de la intensidad 
        ylim = c(0, 580)
        )
legend("topleft",                       #M) Leyenda en parte superior izquierda
       c("Prepandemic (Mar 2019-Feb 2020)","Postpandemic (Mar 2020-Feb 2021)"),              #N) Las leyendas
       fill = c("darkblue","green"),         #O) Colores de las leyendas 
       title = "Time",                #P) Título de la leyenda]
       text.font = 1,                   #Q) Fuente del texto de la leyenda
       bg='lightblue'                   #R) Color del fondo de la caja de la leyenda
) 
text(bar2, t2+30, labels = t2)




t3=table(data2$Month)
bar3<-barplot(t3,main='Month',
        col=rainbow(12),           #H) Colorear las barras
        ylim = c(0, 100)
)
text(bar3,t3+5,labels = t3)

table(data2$Month,data2$Cancer)

t4=table(data2$Bleed)
barplot(t4,main='Bleed')

t5=table(data2$Mode_Presentation)
barplot(t5,main='Mode_presentation')


Age=table(data2$Age)
barplot(Age,main='Age',col=rainbow(40))

t7=table(data2$Gender)
barplot(t7,main='Gender')

t8=table(data2$Etiology)
barplot(t8,main='Age')

t9=table(data2$Cirrhosis)
barplot(t9,main='Age')

size=table(data2$Size)
hist(size,main='Age')

t11=table(data2$HCC_TNM_Stage)
barplot(t11,main='Hepatocellular carcinoma Tumour node metastasis Stage')

t12=table(data2$HCC_BCLC_Stage)
barplot(t12,main='Hepatocellular carcinoma Barcelona Clinic for Liver Cancer Stage')

t13=table(data2$Treatment_grps)
barplot(t13,main='Treatment groups')#grupos de tratamiento

t14=table(data2$Survival_fromMDM)
hist(t14,main='Survival from Multidisciplinary meeting')

t15=table(data2$Alive_Dead)
barplot(t15,main='Alive-Dead')

t16=table(data2$Surveillance_programme)
barplot(t16,main='Patient in a formal surveillance programme')

t17=table(data2$PS)
barplot(t17,main='Performance Status')

t18=table(data2$Prev_known_cirrhosis)
barplot(t18,main="Previous known cirrhosis")
# 
# percantage of people effecting for cancer in pandemic and prepandemic are same
# 
# Symptomatic(involving medical symptoms) are highly sensitive during pandemic
# 
# tumor surveillance(suspected) is more sensitive during prepandemic
# 
# pandemic doesnt have any special effect on cirrhosis cases of cirrhosis in both pandemic and pre pandemic are same
# 
# ablation treatment has high recover rate compare to other treatment. due ablation treatment there are more recover rate from cancer
# 
# no use of supportive care treatment it has more death rates compare to other treatment groups
# 
# HCC_TNM_Stage II,III,IV has more death rates that mean increase of satges in HCC_TNM_Stage increase of death rate
# 
# death rate are more between age 40-68
# 
# average age during pandemic and pre-pandemic are same
# 
# male effected with cirrhosis same as female effected with cirrhosis
# 
# death rate during pandemic and prepandemic are not same while prepandemic deaths are more


# for(i in 1:ncol(data)){
#   print(table(data[i]))
# }
#   print(table(data[1]))
#   

#### Etiology 139
### Bleed
###Cirrhosis
#HCC_TNM_Stage
#HCC_BCLC_Stage
#Surveillance_programme

data3<-data2
buscaNaN(data3)
#drop<-names(data2) %in% c('Bleed','Etiology','Cirrhosis','HCC_TNM_Stage','HCC_BCLC_Stage',
#                        'Surveillance_programme')
#data3<-data2[,!drop]
buscaNaN(data3)


data4=na.omit(data3)
drop<-names(data3)%in% c('Cancer')
data4<-data4[,!drop]
summary(data4)
#0=Prepandemic (mar-feb 2019-2020)
#1=Postpandemic (mar-feb 2020 2021)
buscaNaN(data4)

data4$Alive_Dead<-ifelse(data4$Alive_Dead=='Alive',1,0)
set.seed(111)

library(caret)


table(data4$Treatment_grps)
#Forward selection
forwardvacio<-lm(Alive_Dead~1,data=data4)
forwardcompleto<-lm(Alive_Dead~.,data=data4)
summary(forwardcompleto)
forward<-step(forwardvacio, scope = list(lower=forwardvacio,upper=forwardcompleto),
              direction='forward')
summary(forward)


#surveillance = 0
#Incidental = 1
#Symptomatic = 2




library(dplyr)
data5<-select(data4,Alive_Dead,Survival_fromMDM, Year,Month,Treatment_grps, 
  PS,Size,HCC_TNM_Stage,Gender)


data5$Alive_Dead<-as.factor(ifelse(data4$Alive_Dead==1,'Y','N'))
for(i in 1:nrow(data5)){
data5$Survival_fromMDM[i]=3+((data5$Survival_fromMDM[i]-mean(data5$Survival_fromMDM))/sd((data5$Survival_fromMDM)))
data5$Size[i]=3+((data5$Size[i]-mean(data5$Size))/sd((data5$Size)))
data5$Month[i]=3+((data5$Month[i]-mean(data5$Month))/sd((data5$Month)))

}



library(caret)
indices<-createDataPartition(data5$Alive_Dead,
                             p=0.75,list=FALSE)
datos_entrenamiento<-data5[indices,]
datos_prueba<-data5[-indices,]
nrow(datos_entrenamiento)
nrow(datos_prueba)


parametrosEnt<-trainControl(
  method='cv',
  number=5,
  savePredictions='final',
  classProbs=TRUE
)
predictores<-c('Survival_fromMDM', 'Year','Month','Treatment_grps', 
               'PS','Size','HCC_TNM_Stage','Gender')
salida<-'Alive_Dead'

modelo_RF<-train(datos_entrenamiento[,predictores],
                 datos_entrenamiento[,salida],
                 method='rf',
                 trControl = parametrosEnt,
                 tuneLength = 3)



datos_prueba$predicciones_RF<-predict(object=modelo_RF,
                                      datos_prueba[,predictores])
confusionMatrix(as.factor(datos_prueba$Alive_Dead),
                as.factor(datos_prueba$predicciones_RF),
                positive='Y')
#Sensitivity : 0.9730         
#Specificity : 1.0000
#Accuracy : 0.9863


modelo_glm<-train(datos_entrenamiento[,predictores],
                 datos_entrenamiento[,salida],
                 method='glm',
                 trControl = parametrosEnt,
                 tuneLength = 3)



datos_prueba$predicciones_glm<-predict(object=modelo_glm,
                                      datos_prueba[,predictores])
confusionMatrix(as.factor(datos_prueba$Alive_Dead),
                as.factor(datos_prueba$predicciones_glm),
                positive='Y')
# Sensitivity : 1.0000          
# Specificity : 0.9487
# Accuracy : 0.9726

library(pROC)
datos_prueba$Alive_Dead<-ifelse(datos_prueba$Alive_Dead=='N',0,1)
datos_prueba$predicciones_RF<-ifelse(datos_prueba$predicciones_RF=='N',0,1)
datos_prueba$predicciones_glm<-ifelse(datos_prueba$predicciones_glm=='N',0,1)

curva<-roc(response=datos_prueba$Alive_Dead,
           predictor=datos_prueba$predicciones_glm,
           levels=c(0,1),
           plot=TRUE,
           col='red',
           ci=TRUE,
           smooth=FALSE,
           direction='auto',
           print.auc=T,print.thres = "best", main='Linear Regretion 9 characteristics')

curva<-roc(response=datos_prueba$Alive_Dead,
           predictor=datos_prueba$predicciones_RF,
           levels=c(0,1),
           plot=TRUE,
           col='red',
           ci=TRUE,
           smooth=FALSE,
           direction='auto',
           print.auc=T,print.thres = "best", main='Random Forest 9 characteristics')

############################# COn 17 características
##############
#########
####
data4$Alive_Dead<-data5$Alive_Dead
for(i in 1:nrow(data4)){
  data4$Survival_fromMDM[i]=3+((data4$Survival_fromMDM[i]-mean(data4$Survival_fromMDM))/sd((data4$Survival_fromMDM)))
  data4$Size[i]=3+((data4$Size[i]-mean(data4$Size))/sd((data4$Size)))
  data4$Month[i]=3+((data4$Month[i]-mean(data4$Month))/sd((data4$Month)))
  
}

indices<-createDataPartition(data4$Alive_Dead,
                             p=0.75,list=FALSE)
datos_entrenamiento<-data4[indices,]
datos_prueba<-data4[-indices,]
nrow(datos_entrenamiento)
nrow(datos_prueba)


parametrosEnt<-trainControl(
  method='cv',
  number=5,
  savePredictions='final',
  classProbs=TRUE
)
predictores<-c('Bleed','Mode_Presentation','Age','Cirrhosis',
               'HCC_TNM_Stage','HCC_BCLC_Stage',
               'Prev_known_cirrhosis', 'Year','Month',
               'Treatment_grps','PS','Size','Gender')
salida<-'Alive_Dead'

modelo_RF<-train(datos_entrenamiento[,predictores],
                 datos_entrenamiento[,salida],
                 method='rf',
                 trControl = parametrosEnt,
                 tuneLength = 3)



datos_prueba$predicciones_RF<-predict(object=modelo_RF,
                                      datos_prueba[,predictores])

confusionMatrix(as.factor(datos_prueba$Alive_Dead),
                as.factor(datos_prueba$predicciones_RF),
                positive='Y')
#Sensitivity : 0.7273        
#Specificity : 0.7000
#Accuracy :  0.7123


modelo_glm<-train(datos_entrenamiento[,predictores],
                  datos_entrenamiento[,salida],
                  method='glm',
                  trControl = parametrosEnt,
                  tuneLength = 3)



datos_prueba$predicciones_glm<-predict(object=modelo_glm,
                                       datos_prueba[,predictores])
confusionMatrix(as.factor(datos_prueba$Alive_Dead),
                as.factor(datos_prueba$predicciones_glm),
                positive='Y')
# Sensitivity : 0.7500        
# Specificity : 0.7568
# Accuracy : 0.6944

library(pROC)
datos_prueba$Alive_Dead<-ifelse(datos_prueba$Alive_Dead=='N',0,1)
datos_prueba$predicciones_RF<-ifelse(datos_prueba$predicciones_RF=='N',0,1)
datos_prueba$predicciones_glm<-ifelse(datos_prueba$predicciones_glm=='N',0,1)

curva<-roc(response=datos_prueba$Alive_Dead,
           predictor=datos_prueba$predicciones_glm,
           levels=c(0,1),
           plot=TRUE,
           col='blue',
           ci=TRUE,
           smooth=FALSE,
           direction='auto',
           print.auc=T,print.thres = "best", main='Linear Regretion 17 characteristics')

curva<-roc(response=datos_prueba$Alive_Dead,
           predictor=datos_prueba$predicciones_RF,
           levels=c(0,1),
           plot=TRUE,
           col='blue',
           ci=TRUE,
           smooth=FALSE,
           direction='auto',
           print.auc=T,print.thres = "best", main='Random Forest 17 characteristics')
