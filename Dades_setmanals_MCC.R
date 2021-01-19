#@sm:Script per treure informes setmanals directament de les dades de MCC
#data:18/12/2020

# Càrrega de Paquets i dataset --------------------------------------------
#Carreguem paquets:
suppressMessages(suppressWarnings(if (!require("pacman") == TRUE) { install.packages("pacman", quietly=T) }))
suppressMessages(suppressWarnings(pacman::p_load(tidyverse,lubridate,readxl,plyr,xlsx,eeptools,RColorBrewer,scales,ISOweek,rstudioapi,ggrepel)))

#Set working directory
current_working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_working_dir)

#Carreguem Llibreria de funcions
source2 <- function(f, encoding = 'UTF-8') { #per llegir arxiu R en UTF-8
  l <- readLines(f, encoding=encoding)
  eval(parse(text=l),envir=.GlobalEnv)
}#per evitar el problema de l'encoding UTF-8 de source()
source2('Llibreria_setmanal.R')

#CARREGUEM DADES:
#Taga<-as.data.frame(read_excel('./Dataset/taga-covid.xlsx')) #Ja no venen pel Luca 
codis<-as.data.frame(read_excel('./Metaarxius/codis_municipis.xlsx'))
codis$nom_unitat_de_poblacio<-toupper(codis$nom_unitat_de_poblacio)
Indicadors<-read_excel('./Metaarxius/Resum_setmanal.xlsx') #per taula resum setmanal
Taula8.Informe<-read_excel('./Metaarxius/Esquema_taula8.xlsx')

pacients<-as.data.frame(read_excel('./Dataset/pacients.xlsx',col_types = 'text')) %>% distinct()
contactes<-as.data.frame(read_excel('./Dataset/contactes.xlsx',col_types = 'text')) %>% distinct()
t.historica<-as.data.frame(read_excel('./Metaarxius/taula3_historica.xlsx',col_types = 'text'))
#carrego dades setmana-2:
pacients_sem_2<-as.data.frame(read_excel('./Dataset/pacients_setmana-2.xlsx',col_types = 'text')) %>% distinct()
contactes_sem_2<-as.data.frame(read_excel('./Dataset/contactes_setmana-2.xlsx',col_types = 'text')) %>% distinct()
#carrego dades setmana -3:
pacients_sem_3<-as.data.frame(read_excel('./Dataset/pacients_setmana-3.xlsx',col_types = 'text')) %>% distinct()
contactes_sem_3<-as.data.frame(read_excel('./Dataset/contactes_setmana-3.xlsx',col_types = 'text')) %>% distinct()
rs_sve<-as.data.frame(read_excel('./Metaarxius/taula_conversor_SVE.xlsx'))


# MODIFICACIÓ TAULA DE PACIENTS: CÀLCUL SVE -------------------------------------------
#Càlcul de SVE per cada setmana
pacients.2_sve<-funcio_posar_sve_pacients(pacients,codis)
pacients.2_sve$contactes<-as.numeric(pacients.2_sve$contactes)
pacients.2_sve_sem_2<-funcio_posar_sve_pacients(pacients_sem_2,codis)
pacients.2_sve_sem_3<-funcio_posar_sve_pacients(pacients_sem_3,codis)



# MODIFICACIÓ TAULA CONTACTES: CÀLCUL SVE ---------------------------------

#Necessitem afegir la data cas de Mediador als contactes entrats
pacients.c_d<-pacients.2_sve%>%select(CIP,SVE) %>% distinct()
names(pacients.c_d)<-c("CIP",'SVE')
pacients.c_d_sem2<-pacients.2_sve_sem_2%>%select(CIP,SVE) %>% distinct()#setm anterior
names(pacients.c_d_sem2)<-c("CIP",'SVE')
pacients.c_d_sem3<-pacients.2_sve_sem_3%>%select(CIP,SVE) %>% distinct()#setm -3
names(pacients.c_d_sem2)<-c("CIP",'SVE')

#Join columnes de data mediador del cas i SVE del cas
contactes_2<-left_join(contactes,pacients.c_d, by=c("cas"="CIP")) %>% select(1:2,35,3:34)
contactes_2_sve<-funcio_sve_contactes(contactes_2,codis)


contactes_2_sem2<-left_join(contactes_sem_2,pacients.c_d_sem2, by=c("cas"="CIP"))%>%select(1:2,35,3:34)
contactes_2_sve_sem2<-funcio_sve_contactes(contactes_2_sem2,codis)

contactes_2_sem3<-left_join(contactes_sem_3,pacients.c_d_sem3, by=c("cas"="CIP"))%>%select(1:2,35,3:34)
contactes_2_sve_sem3<-funcio_sve_contactes(contactes_2_sem3,codis)




# INDICADOR 1 --------------------------------------------------------

#Indicador 1.1
Taula.pacients.setmana<-as.data.frame(pacients.2_sve%>%
                                      group_by(SVE)%>%dplyr::summarize('1.1-casos_totals'=n()))


#Indicadors 1.2
pacients.casos<-as.data.frame(pacients.2_sve%>%filter(contactes>0))%>%
  group_by(SVE)%>%
  dplyr::summarize('1.2-N_casos_reporten_contactes'=n(),'2.1-N total de CE'=sum(contactes),
                   '2.2-Mitjana'=round(mean(contactes),1),'2.2Mediana'=median(contactes),'2.2-IQR'=paste0(IQR(contactes),' (',quantile(contactes,0.25),'-',quantile(contactes,0.75),')'),
                   '2.2-Min'=min(contactes),'2.2-max'=max(contactes))

#combinem
Indicadors1_2<-left_join(Taula.pacients.setmana,pacients.casos,by=c('SVE'='SVE')) %>% 
  mutate('1.3-%casos amb contactes'=paste0(round((`1.2-N_casos_reporten_contactes`/`1.1-casos_totals`)*100,1),'%'))%>%
  mutate('CE/CI'=round(`2.1-N total de CE`/`1.2-N_casos_reporten_contactes`,1)) %>% 
  select(1:3,10:11,4:9) %>% funcio_corregir_sve() %>% arrange(SVE)

Indicadors1_2<-rbind(Indicadors1_2,c("Totals",sum(Indicadors1_2$`1.1-casos_totals`),
                                     sum(Indicadors1_2$`1.2-N_casos_reporten_contactes`,na.rm = TRUE),
                                     paste0(round(sum(Indicadors1_2$`1.2-N_casos_reporten_contactes`,na.rm = TRUE)/sum(Indicadors1_2$`1.1-casos_totals`,na.rm = TRUE)*100,1),'%'),
                                     round(sum(Indicadors1_2$`2.1-N total de CE`)/sum(Indicadors1_2$`1.2-N_casos_reporten_contactes`),1),
                                     sum(Indicadors1_2$`2.1-N total de CE`,na.rm = TRUE),
                                     round(mean(pacients.2_sve$contactes[pacients.2_sve$contactes>0]),1),
                                     median(pacients.2_sve$contactes[pacients.2_sve$contactes>0])
                                     ,paste0(IQR(pacients.2_sve$contactes[pacients.2_sve$contactes>0]),
                                             ' (',quantile(pacients.2_sve$contactes[pacients.2_sve$contactes>0],0.25),'-',
                                             quantile(pacients.2_sve$contactes[pacients.2_sve$contactes>0],0.75),')'),
                                     min(pacients.2_sve$contactes[pacients.2_sve$contactes>0]),
                                     max(pacients.2_sve$contactes[pacients.2_sve$contactes>0])))
Indicadors1_2_subset<-Indicadors1_2 %>% select(-`CE/CI`)

#carrego dades de Taga:
Taga<-funcio_aCAs_MCC(contactes_2_sve,Indicadors1_2)

#origen dades eCAP/TAGA
origen.dades<-data.frame(
  eCAP<-pacients.2_sve %>% filter(orígen%in%'eCap') %>% group_by(SVE) %>% dplyr::summarize(Origen_eCap=n()),
  Covid19<-pacients.2_sve %>% filter(orígen%in%'COVID19') %>% group_by(SVE) %>% dplyr::summarize(Origen_COVID19=n())
) %>% replace(is.na(.), 0) %>%  #Afegim%
  mutate('%Origen_eCAP'=paste0(round(Origen_eCap/(Origen_eCap+Origen_COVID19)*100,1),'%'),
         '%Origen_COVID19'=paste0(round(Origen_COVID19/(Origen_eCap+Origen_COVID19)*100,1),'%'),
         TOTAL=Origen_eCap+Origen_COVID19
  )%>%select(1,2,5,4,6,7) %>% funcio_corregir_sve() %>% arrange(SVE)

origen.dades<-rbind(origen.dades,c('TOTAL',sum(origen.dades$Origen_eCap),
                                   paste0(round(sum(origen.dades$Origen_eCap)/(sum(origen.dades$Origen_eCap)+sum(origen.dades$Origen_COVID19))*100,1),'%'),
                                   sum(origen.dades$Origen_COVID19),
                                   paste0(round(sum(origen.dades$Origen_COVID19)/(sum(origen.dades$Origen_eCap)+sum(origen.dades$Origen_COVID19))*100,1),'%'),
                                   sum(origen.dades$TOTAL))
)

#taula3
N_setmana<-isoweek(as.Date(Sys.Date()))-1
N_setmana_ant<- N_setmana - 1
taula3<-Indicadors1_2 %>% select(1:3) %>% 
  mutate(CNI=as.numeric(`1.1-casos_totals`)-as.numeric(`1.2-N_casos_reporten_contactes`),
         CI_Setmana_anterior=t.historica[,paste0('SE',N_setmana_ant,'_CI')],
         CNI_Setmana_anterior=t.historica[,paste0('SE',N_setmana_ant,'_CNI')],
         Totals_setmana_anterior=as.numeric(CI_Setmana_anterior)+as.numeric(CNI_Setmana_anterior)) %>% 
           select(1,3,4,2,5:7)

taula3[,2:7]<-sapply(taula3[,2:7],as.numeric)
taula3<-taula3 %>% mutate('%_I_SE_A'=round(`1.2-N_casos_reporten_contactes`/`1.1-casos_totals`*100,1),'%_NI_SEA'=round(CNI/`1.1-casos_totals`*100,1),
               '%_I_SEan'=round(CI_Setmana_anterior/Totals_setmana_anterior*100,1),'%_NI_SEan'=round(CNI_Setmana_anterior/Totals_setmana_anterior*100,1))
Taula3_final<-taula3 %>% select(c(1:4,8,10)) 
Taula3_final<-Taula3_final %>% mutate(Variació=unlist(Taula3_final[5]-Taula3_final[6])) %>% arrange(SVE) %>% slice(1:8,10,9)

names(Taula3_final)<-c('SVE','I','NI','Total',paste0('%_I_SE',N_setmana),paste0('%_I_SE',N_setmana_ant),'Variació %')


#fem la taula 3 per a poder imprimir el plot
taula3_actual<-taula3 %>% select(1,8:9)%>% slice(-10)
taula3_actual_i<-taula3_actual %>% select(1) %>% mutate('%'=taula3_actual$`%_I_SE_A` ,Estat=paste0('I_SE',N_setmana))
taula3_actual_ni<-taula3_actual %>% select(1) %>% mutate('%'=taula3_actual$`%_NI_SEA`,Estat=paste0('NI_SE',N_setmana))
taula3_actual<-rbind(taula3_actual_i,taula3_actual_ni)

taula3_anterior<-taula3 %>% select(1,10:11)%>% slice(-10)
taula3_anterior_i<-taula3_anterior %>% select(1) %>% mutate('%'=taula3_anterior$`%_I_SEan` ,Estat=paste0('I_SE',N_setmana_ant))
taula3_anterior_ni<-taula3_anterior %>% select(1) %>% mutate('%'=taula3_anterior$`%_NI_SEan` ,Estat=paste0('NI_SE',N_setmana_ant))
taula3_anterior<-rbind(taula3_anterior_i,taula3_anterior_ni)



# INDICADOR 2 -------------------------------------------------------------

# ESTUDI DELS CE/CI PER SVE I PER ÀMBIT ESCOLAR/ NO ESCOLAR
#miro els cips que tenen àmbit escolar, i resto a la resta de CIPS. Atenció!!!Un CIP pot tenir casos amb més d'un àmbit!
CIPS_casos_escolars<-contactes_2_sve %>%filter(`àmbit contacte`=='escolar')%>% distinct(cas)
CIPS_casos_pacients_informats<-pacients %>% filter(contactes>0) %>% distinct(CIP)
CIPS_casos_no_escolars <-anti_join(CIPS_casos_pacients_informats,CIPS_casos_escolars,by=c('CIP'='cas'))

#Faig extracció dels casos escolars/no escolars pels CIPS que he tret a taula contactes filtrats per àmbits escolars per SVE de pacient
taula_casos_contactes_escolars<-pacients.2_sve %>% filter(CIP%in%CIPS_casos_escolars$cas) %>% group_by(SVE) %>%
  dplyr::summarize('Casos_contactes_escolars'=n())
taula_casos_contactes_no_escolars<-pacients.2_sve %>% filter(CIP%in%CIPS_casos_no_escolars$CIP) %>% group_by(SVE) %>%
  dplyr::summarize('Casos_contactes_no_escolars'=n())
taula_casos_escolars<-full_join(taula_casos_contactes_escolars,taula_casos_contactes_no_escolars,by='SVE')


#Trec els contactes amb àmbit escolar per SVE de contacte
taula_contactes_ambits_escolars<-contactes_2_sve %>%filter(`àmbit contacte`=='escolar') %>% group_by(SVE) %>% 
  dplyr::summarize(contactes_escolars=n()) %>% as.data.frame()
taula_contactes_ambits_no_escolars<-contactes_2_sve %>% filter(`àmbit contacte`!='escolar'| is.na(`àmbit contacte`)) %>% group_by(SVE) %>%
  dplyr::summarize(contactes_no_escolars=n()) %>% as.data.frame()
taula_contactes_ambits<-full_join(taula_contactes_ambits_escolars,taula_contactes_ambits_no_escolars,by='SVE')

taula_Escoles<-full_join(taula_casos_escolars,taula_contactes_ambits,by='SVE') %>% 
  mutate('CE/CI_àmbit_escolar'=round(contactes_escolars/Casos_contactes_escolars,1)) %>% 
  mutate('CE/CI_àmbit_no_escolar'=round(contactes_no_escolars/Casos_contactes_no_escolars,1)) %>% replace(is.na(.), 0) %>% 
  funcio_corregir_sve() %>% arrange(SVE)
names(taula_Escoles)<-c('SVE','Casos amb contactes escolars','Casos sense contactes escolars','Contactes àmbit escolar','Contactes àmbit no escolar', 'CE/CI àmbit escolar','CE/CI àmbit no escolar')
taula_Escoles<-rbind(taula_Escoles,c('Totals',sum(taula_Escoles$`Casos amb contactes escolars`),
                                                       sum(taula_Escoles$`Casos sense contactes escolars`),
                                                       sum(taula_Escoles$`Contactes àmbit escolar`),
                                                       sum(taula_Escoles$`Contactes àmbit no escolar`),
                                                       round(sum(taula_Escoles$`Contactes àmbit escolar`)/sum(taula_Escoles$`Casos amb contactes escolars`),1),
                                                       round(sum(taula_Escoles$`Contactes àmbit no escolar`)/sum(taula_Escoles$`Casos sense contactes escolars`),1)))


#Calculo la taula 5, on agrupem els pacients per número de contactes
Taula5<-pacients %>%filter(contactes>0) %>%  group_by(contactes) %>% dplyr::summarize('nº casos informats'=n())
Taula5<-Taula5 %>%mutate('% de casos amb CE'= round(`nº casos informats`/sum(`nº casos informats`)*100,3)) %>% arrange(desc(`nº casos informats`)) %>% as.data.frame()
#Taula5<-rbind(Taula5,c('Totals',sum(Taula5$`nº casos informats`),sum(Taula5$`% de casos amb CE`)))


# Superspreaders ----------------------------------------------------------
#Mirem que n'ni hagin
n.max.cont<-max(as.numeric(pacients$contactes))
p.se<-pacients.2_sve %>% filter(between(as.numeric(contactes),20,n.max.cont)) %>%
  mutate(`data naixement`=parse_date_time(`data naixement`,c('dmy','ymd'))) %>%
  mutate(Edat=age_calc(dob=as.Date(`data naixement`),enddate = as.Date((now())),units = 'years') %>% floor()) %>% 
  select(CIP,gènere,Edat,nom_unitat_de_poblacio,SVE,contactes)

c.se<-contactes %>% filter(cas%in%p.se$CIP) %>% group_by(cas,`àmbit contacte`) %>% 
  dplyr::summarize(N=n()) %>% 
  spread(`àmbit contacte`,N) %>% #canviem columnes
  as.data.frame() %>% 
  replace(is.na(.), 0) %>% 
  mutate(N_contactes_segons_pestanya_contactes=rowSums(.[2:ncol(.)]))
superspreaders<-p.se %>% full_join(c.se,by=c('CIP'='cas')) %>% arrange(desc(contactes))

colnames(superspreaders)[6]<-'N_contactes_segons_pestanya_pacients'




#Estudi d'àmbits dels contactes:
ambits_rang_df<-contactes_2_sve

#Càlcul de les edats:
ambits_rang_df$`data naixement`<-parse_date_time(ambits_rang_df$`data naixement`,c('dmy','mdy','ymd'))

data_avui<-as.Date(now())
data_min<-as.Date('1901-01-01')
#netejo dates errònies:
for (i in 1:length(ambits_rang_df$`data naixement`)){
  if(!is.na(ambits_rang_df$`data naixement`[i])){
    if(ambits_rang_df$`data naixement`[i] > data_avui){
      ambits_rang_df$`data naixement`[i]<-NA
    }else if(ambits_rang_df$`data naixement`[i]<data_min){
      ambits_rang_df$`data naixement`[i]<-NA
    }
  }
}


#Afegim una columna amb l'edat:
ambits_rang_df<-ambits_rang_df%>%filter(!is.na(`data naixement`))%>%mutate(edat=age_calc(dob = as.Date(`data naixement`),enddate = data_avui, units = 'years'))%>%mutate(edat=floor(edat))

#Afegim una segona columna on afegirem el rang al que pertany
ordre_edats<-c('< 12 anys','12-19 anys','20-39 anys','40-59 anys','60-79 anys','> 79 anys')
ordre_ambits<-c('social','medi transport','laboral','escolar','domicili','desconegut','centre sociosanitari','centre sanitari','altres')
ambits_rang_df<-ambits_rang_df%>%mutate(Rang_edat=(case_when(edat>79 ~'> 79 anys',
                                                             between(edat,60,79)~'60-79 anys',
                                                             between(edat,40,59)~'40-59 anys',
                                                             between(edat,20,39)~'20-39 anys',
                                                             between(edat,12,19)~'12-19 anys',
                                                             edat<12~'< 12 anys'
)))
#Els àmbits totals sense agregar
taula_ambits_contacte_totals<-as.data.frame(contactes_2_sve%>%group_by(`àmbit contacte`)%>%dplyr::summarize(N_contactes=n()))
taula_ambits_contacte_totals_plot<-taula_ambits_contacte_totals[!is.na(taula_ambits_contacte_totals$`àmbit contacte`),]#trec els NA
taula_ambits_contacte_totals_plot$`àmbit contacte`<- as.factor(taula_ambits_contacte_totals_plot$`àmbit contacte`)
taula_ambits_contacte_totals_plot$`àmbit contacte`<- factor(taula_ambits_contacte_totals_plot$`àmbit contacte`,levels=ordre_ambits)

#Àmbits agregats per SVE
taula_ambits_contacte_SVE<-as.data.frame(contactes_2_sve%>%group_by(SVE,`àmbit contacte`)%>%dplyr::summarize(N_contactes=n())) %>% 
  funcio_corregir_sve() %>% arrange(SVE)
#Àmbits agregats per Rang d'edat
taula_ambits_rangs<-as.data.frame(ambits_rang_df%>%group_by(Rang_edat,`àmbit contacte`)%>%dplyr::summarize(N_contactes=n()))
#Àmbits agregats per Rang d'edat i SVE
taula_ambits_rangs_sve<-as.data.frame(ambits_rang_df%>%group_by(SVE,Rang_edat,`àmbit contacte`)%>%dplyr::summarize(N_contactes=n())) %>% 
  funcio_corregir_sve() %>% arrange(SVE)
taula_ambits_rangs_sve$Rang_edat<-as.factor(taula_ambits_rangs_sve$Rang_edat) #Endrecem la taula per ordre de rangs d'edat
taula_ambits_rangs_sve$`àmbit contacte` <-as.factor(taula_ambits_rangs_sve$`àmbit contacte`)
taula_ambits_rangs_sve$Rang_edat<-factor(taula_ambits_rangs_sve$Rang_edat,levels = ordre_edats)
taula_ambits_rangs_sve$`àmbit contacte` <-factor(taula_ambits_rangs_sve$`àmbit contacte`,levels = ordre_ambits)

#Àmbits per verificació
taula_ambits_verificacio<-contactes_2_sve %>% group_by(`àmbit contacte`,verificació) %>% dplyr::summarize(N=n()) %>% 
  arrange(`àmbit contacte`,verificació) %>% as.data.frame()


#Taula 8 Casuístiques de no seguiment de contactes estrets verificats:
#Taula 8a:Incidències de no seguiment de contactes
prueba<-contactes_2_sve %>% filter(!is.na(verificació) & verificació%in%c('error dades','rebutja','no contesta','CIP no vàlid')) %>% 
  group_by(verificació) %>% dplyr::summarize(N=n()) %>% arrange(verificació) %>% 
  mutate(Percentatge=round(N/sum(N)*100,1),
         'verificació'=c('CIP no vàlid','Error de dades','No respon','Rebutja seguiment'
         ))
Taula8.a<-contactes_2_sve %>% filter(!is.na(verificació) & verificació%in%c('error dades','rebutja','no contesta','CIP no vàlid')) %>% 
  group_by(verificació) %>% dplyr::summarize(N=n()) %>% arrange(verificació) %>% 
  mutate(Percentatge=round(N/sum(N)*100,1),
         'verificació'=c('CIP no vàlid','Error de dades','No respon','Rebutja seguiment')) %>% as.data.frame() %>% 
  add_row(verificació='Totals',N=sum(.$N),Percentatge=sum(.$Percentatge))
#taula8b:Contactes que no requereixen seguiment
Taula8.b<-contactes_2_sve %>%  filter(!is.na(verificació) & verificació%in%c('migrat SVE','contacte altre cas','no és contacte','és cas','contacte no vàlid')) %>% 
  mutate(verificació=str_replace(verificació,'contacte no vàlid','no és contacte')) %>% 
  group_by(verificació) %>% dplyr::summarize(N=n()) %>% arrange(verificació) %>% 
  mutate(Percentatge=round(N/sum(N)*100,1),
         'verificació'=str_to_sentence(verificació) %>% str_replace_all('sve','SVE')) %>%  
  as.data.frame() %>% 
  add_row(verificació='Totals',N=sum(.$N),Percentatge=sum(.$Percentatge))        

#taula 8 agregada amb totes les verificacions
dataset.escolar.nc<-contactes_2_sve %>% filter( verificació=='no contesta' & `àmbit contacte`=='escolar') #escolars no contesten
dataset.escolar.nc.sve<-dataset.escolar.nc %>% group_by(SVE) %>% dplyr::summarize(N=n())%>%
  funcio_corregir_sve() %>% arrange(SVE)%>%
  mutate(Percentatge=paste0(round(N/sum(N)*100,1),'%')) %>%
  add_row(SVE='Totals',N=sum(.$N),Percentatge='100%') %>% as.data.frame()

#Escolars que no contesten (N):
Escolars.NC<-taula_ambits_verificacio %>% filter(`àmbit contacte`=='escolar' & verificació=='no contesta') %>% .$N %>% sum() %>% as.numeric()

ordre.t8<-c('Cip vàlid','Error dades','Rebutja','No contesta','Cip no vàlid','És cas','Contacte altre cas',
            'Migrat SVE','No és contacte','No contesta (\"escolar\")','Totals')

#còpia per fer el gèfic sense '%'


Taula8.plot<-contactes_2_sve %>% anti_join(dataset.escolar.nc) %>% 
  filter(!is.na(verificació)) %>% 
  mutate(verificació=str_replace(verificació,'contacte no vàlid','no és contacte')) %>% 
  group_by(verificació) %>% dplyr::summarize(N=n()) %>%
  add_row(verificació='No contesta (\"escolar\")',N=Escolars.NC) %>% 
  mutate(Percentatge=round(N/sum(N)*100,1),
         'verificació'=str_to_sentence(verificació) %>% str_replace_all('sve','SVE')) %>%  
  as.data.frame() %>% 
  add_row(verificació='Totals',N=sum(.$N),Percentatge=sum(.$Percentatge)) %>% 
  mutate(verificació=factor(verificació,levels = ordre.t8)) %>% arrange(verificació)

#Taula per l'informe
Taula8<-Taula8.plot %>% mutate(Percentatge=paste0(Percentatge,'%'))

#taula 8 amb el format de l'informe:

Taula8.Informe<-Taula8.Informe %>% 
  mutate(
    N=c('',Taula8[1,2],'',Taula8[2:5,2],sum(as.numeric(Taula8[1:5,2])),'N',
        Taula8[6:9,2],Taula8[10,2],sum(as.numeric(Taula8[6:10,2])),Taula8[11,2]),
    Percentatge=c('',Taula8[1,3],'',Taula8[2:5,3],paste0(round(as.numeric(.$N[8])/as.numeric(.$N[16])*100,1),'%'),
                  'Percentatge',Taula8[6:10,3],paste0(round(as.numeric(.$N[15])/as.numeric(.$N[16])*100,1),'%'),'100%')
  ) %>% as.data.frame()

# INDICADOR3 seguiment de contactes estrets (CE) ------------------------


#CE agregats per SVE i verificació
Indicadors_3_SVE<-as.data.frame(contactes_2_sve%>%group_by(SVE,verificació)%>%dplyr::summarize(N_contactes=n())) %>% 
  funcio_corregir_sve() %>% arrange(SVE)
taula_indicador_3_totals<-as.data.frame(contactes_2_sve%>%group_by(verificació)%>%dplyr::summarize(N_contactes=n()))


# Seguiment de CE ---------------------------------------------------------

#Apliquem funció que torna els seguiments que es fan, els motius de no confinament i Simptomàtics/Asimtomàtics dia 0,7,10 i 14:
list.dia7<-funcio_CE_seguiment2(contactes_2_sve_sem2,7)
list.dia10_2<-funcio_CE_seguiment2(contactes_2_sve_sem2,10)
list.dia10_3<-funcio_CE_seguiment2(contactes_2_sve_sem3,10)
list.dia14<-funcio_CE_seguiment2(contactes_2_sve_sem3,14)

#seguiment dia 7
cont_seguim_dia7<-list.dia7[[1]]

#Els seguiments a dia 10, poden estar a la setmana -2 i -3
cont_seguim_dia10_2<-list.dia10_2[[1]] %>% mutate(Seguiment_dia10=as.numeric(Seguiment_dia10))
cont_seguim_dia10_3<-list.dia10_3[[1]] %>% mutate(Seguiment_dia10=as.numeric(Seguiment_dia10))
cont_seguim_dia10_2.3<-rbind(cont_seguim_dia10_2,cont_seguim_dia10_3) %>% #sumen els coincidents
  group_by(SVE,Estat) %>% summarise_all(list(sum))

#seguiment dia 14
cont_seguim_dia14<-list.dia14[[1]]

Taula_seguiment<-full_join(cont_seguim_dia7,cont_seguim_dia10_2.3, by=c('SVE','Estat'))%>%full_join(cont_seguim_dia14,by=c('SVE','Estat')) %>%
  replace(is.na(.), 0) %>% funcio_corregir_sve() %>%funcio_sense_seg() %>%arrange(SVE)

Taula_seguiment<-rbind(Taula_seguiment,c("Totals",'---', as.character(sum(as.numeric(Taula_seguiment$Seguiment_dia7))),as.character(sum(as.numeric(Taula_seguiment$Seguiment_dia10))),as.character(sum(as.numeric(Taula_seguiment$Seguiment_dia14)))))




# Motius de no confinament ------------------------------------------------

#Tenim els motius com a segona df dels llistats obtinguts a la funció de seguiment:
#Motius dia 7:
Motius_dia7<-list.dia7[[2]]

#Els seguiments a dia 10, poden estar a la setmana -2 i -3
Motius_dia10_2<-list.dia10_2[[2]] %>% as.data.frame() %>% mutate(Motius_dia_10=as.numeric(Motius_dia_10))
Motius_dia10_3<-list.dia10_3[[2]] %>% as.data.frame() %>% mutate(Motius_dia_10=as.numeric(Motius_dia_10))
Motius_dia10_2.3<-rbind(Motius_dia10_2,Motius_dia10_3) %>% #sumen els coincidents
  group_by(SVE,Motiu) %>% summarise_all(list(sum))

#Motius dia 14:
Motius_dia14<-list.dia14[[2]]

Taula_motius_SVE<-full_join(Motius_dia7,Motius_dia10_2.3,by=c('SVE','Motiu'))%>%full_join(Motius_dia14,by=c('SVE','Motiu')) %>% 
  mutate_at(vars(Motius_dia_7,Motius_dia_10,Motius_dia_14),~replace_na(.,0))%>%
  funcio_corregir_sve() %>% funcio_sense_seg() %>%  arrange(SVE) %>% as.data.frame()

# INDICADOR 4: Evolució de contactes estrets a cas en el període d’estudi -------------------------------------------

#Simptomàtics
I4_simptomàtics_0<-as.data.frame(contactes_2_sve%>%filter(símptomes=='Sí')%>%group_by(SVE)%>%dplyr::summarize(Simptomàtics_dia0=n()))%>%replace(is.na(.), 0)
I4_simptomàtics_7<-list.dia7[[3]]
I4_simptomàtics_dia10_2<-list.dia10_2[[3]]%>% as.data.frame() %>% mutate(Simptomàtics_dia_10=as.numeric(Simptomàtics_dia_10))
I4_simptomàtics_dia10_3<-list.dia10_3[[3]] %>% as.data.frame() %>% mutate(Simptomàtics_dia_10=as.numeric(Simptomàtics_dia_10))
I4_simptomàtics_dia10_2.3<-rbind(I4_simptomàtics_dia10_2,I4_simptomàtics_dia10_3) %>% #sumen els coincidents
  group_by(SVE) %>% summarise_all(list(sum))

I4_simptomàtics_14<-list.dia14[[3]]

I4_simptomàtics0_7_14<-full_join(I4_simptomàtics_0,I4_simptomàtics_7,by=c('SVE')) %>% 
                    full_join(I4_simptomàtics_dia10_2.3,by=c('SVE')) %>% 
                    full_join(I4_simptomàtics_14,by=c('SVE')) %>%
                    replace(is.na(.), 0) %>% 
                    mutate(Totals= na.omit(as.numeric(Simptomàtics_dia0) + as.numeric(Simptomàtics_dia_7) + as.numeric(Simptomàtics_dia_10)+ as.numeric(Simptomàtics_dia_14))) %>%
                    funcio_corregir_sve() %>% funcio_sense_seg()%>%arrange(SVE)
  
I4_simptomàtics0_7_14<-rbind(I4_simptomàtics0_7_14,c('Totals',sum(I4_simptomàtics0_7_14$Simptomàtics_dia0),
                                                     sum(as.numeric(I4_simptomàtics0_7_14$Simptomàtics_dia_7)),
                                                     sum(as.numeric(I4_simptomàtics0_7_14$Simptomàtics_dia_10)),
                                                     sum(as.numeric(I4_simptomàtics0_7_14$Simptomàtics_dia_14)),
                                                     sum(as.numeric(I4_simptomàtics0_7_14$Totals))
                                                    )
                            )

#Asimptomàtics
I4_asimptomàtics_0<-as.data.frame(contactes_2_sve%>%filter(símptomes=='No')%>% group_by(SVE)%>%dplyr::summarize(Asimptomàtics_dia0=n())) %>% replace(is.na(.),0) 
I4_asimptomàtics_7<-list.dia7[[4]]
I4_asimptomàtics_dia10_2<-list.dia10_2[[4]] %>% as.data.frame() %>% mutate(Asimptomàtics_dia_10=as.numeric(Asimptomàtics_dia_10))
I4_asimptomàtics_dia10_3<-list.dia10_3[[4]] %>% as.data.frame() %>% mutate(Asimptomàtics_dia_10=as.numeric(Asimptomàtics_dia_10))
I4_asimptomàtics_dia10_2.3<-rbind(I4_asimptomàtics_dia10_2,I4_asimptomàtics_dia10_3) %>% #sumen els coincidents
  group_by(SVE) %>% summarise_all(list(sum))

I4_asimptomàtics_14<-list.dia14[[4]]

I4_asimptomàtics0_7_14<-full_join(I4_asimptomàtics_0,I4_asimptomàtics_7,by=c('SVE')) %>% 
  full_join(I4_asimptomàtics_dia10_2.3,by=c('SVE')) %>% 
  full_join(I4_asimptomàtics_14,by=c('SVE')) %>%
  replace(is.na(.), 0) %>% 
  mutate(Totals= na.omit(as.numeric(Asimptomàtics_dia0) + as.numeric(Asimptomàtics_dia_7) + as.numeric(Asimptomàtics_dia_10)+ as.numeric(Asimptomàtics_dia_14))) %>%
  funcio_corregir_sve() %>% funcio_sense_seg() %>%  arrange(SVE)

I4_asimptomàtics0_7_14<-rbind(I4_asimptomàtics0_7_14,c('Totals',sum(I4_asimptomàtics0_7_14$Asimptomàtics_dia0),
                                                     sum(as.numeric(I4_asimptomàtics0_7_14$Asimptomàtics_dia_7)),
                                                     sum(as.numeric(I4_asimptomàtics0_7_14$Asimptomàtics_dia_10)),
                                                     sum(as.numeric(I4_asimptomàtics0_7_14$Asimptomàtics_dia_14)),
                                                     sum(as.numeric(I4_asimptomàtics0_7_14$Totals))
)
)


# Taules per Madrid -------------------------------------------------------


#TAULA MADRID Provincies
Taula_madrid<-Indicadors1_2 %>% select(1:2,6) %>% slice(1:(nrow(Indicadors1_2)-1))
Madrid_simptomes<-I4_simptomàtics0_7_14 %>% select(1,ncol(I4_simptomàtics0_7_14))%>% slice(1:(nrow(I4_simptomàtics0_7_14)-1))
Madrid_soncas<-Taga %>% select(1,4)%>% slice(1:(nrow(Taga)-1)) %>% funcio_corregir_sve()
Taula_madrid<-full_join(Taula_madrid,Madrid_simptomes,by=c('SVE'))
Taula_madrid<-full_join(Taula_madrid,Madrid_soncas,by=c('SVE'))
Taula_madrid<-Taula_madrid %>% mutate(Provincia=NA)
for (i in 1:nrow(Taula_madrid)){
  tempSVE<-as.character(Taula_madrid$SVE[i])
  Taula_madrid$Provincia[i]<-rs_sve$Provincia[rs_sve$SVE_corregida==tempSVE]
}
Taula_madrid<-arrange(Taula_madrid,SVE)
Taula_madrid_provincia<-Taula_madrid %>% select(6,2:5)
Taula_madrid_provincia[,2:5]<- sapply(Taula_madrid_provincia[,2:5],as.numeric) %>% replace(is.na(.), 0)
Taula_madrid_provincia<-Taula_madrid_provincia %>% group_by(Provincia) %>% summarise_all(funs(sum)) %>% as.data.frame()

names(Taula_madrid_provincia)<-c('Provincia','Casos notificats','Total de CE','CE que desenvolupen símptomes','CE que es confirmen com a Casos')
Taula_madrid_provincia<-rbind(Taula_madrid_provincia,c('Totals',sum(Taula_madrid_provincia$`Casos notificats`),sum(Taula_madrid_provincia$`Total de CE`),
                                                       sum(Taula_madrid_provincia$`CE que desenvolupen símptomes`),sum(Taula_madrid_provincia$`CE que es confirmen com a Casos`)))

#TAULA Madrid Resum
#fem una taula resum que anirà al final de l'informe:
Taula_resum<-Indicadors1_2 %>% select(1:2,6)
Total_simptom<-I4_simptomàtics0_7_14 %>% select(1,ncol(I4_simptomàtics0_7_14))
Total_soncas<-Taga %>% select(1,4) %>% funcio_corregir_sve()
Taula_resum<-full_join(Taula_resum,Total_simptom, by='SVE')
Taula_resum<-full_join(Taula_resum,Total_soncas, by='SVE')
Taula_resum[,2:5]<-sapply(Taula_resum[,2:5],as.numeric)

Taula_resum<-Taula_resum %>% mutate('Percentatge(%)'= round((`1.1-casos_totals`/`1.1-casos_totals`[10])*100,1)) %>% select(1,2,6,3:5) 
names(Taula_resum)<-c('SVE','Total de casos notificats','Percentatge(%)','Contactes estrets(CE)','CE amb símptomes','CE que es confirmen com a cas')


#faig una còpia per a poder utilitzar en l'infogràfic
Taula_resum_infografic<-Taula_resum

#Agrupem terres de l'Ebre i Tarragona i corregim noms
Taula_resum<-Taula_resum %>% funcio_corregir_sve() %>% 
                             mutate(SVE=str_replace_all(SVE,'Terres de l\'Ebre','Tarragona')) %>%   #Ajuntem Tarragona i Terres de l'Ebre:
                             group_by(SVE)%>%summarise_all(funs(sum)) %>% 
                             mutate(SVE=str_replace_all(SVE,'Tarragona','Tarragona i Terres de L\'Ebre')) %>% 
                             arrange(SVE) %>% slice(1:7,9,8)
#Fem versió en Castellà per l'output
Taula_resum_castellano<-Taula_resum %>% select(-3)#no volen els %
Taula_resum_castellano$SVE<-str_replace_all(Taula_resum_castellano$SVE,'Totals','Total')
names(Taula_resum_castellano)<-c('Subdivisión territorial epidemiológica','Número total de casos notificados','Número total de contactos estrechos',
                                 'Número de contactos estrechos que desarrollan síntomas','Número de contactos estrechos que se confirman como casos')


Taula7_soncas<-Total_soncas %>%mutate(SVE=str_replace_all(SVE,'Barcelona$','Barcelona Ciutat')) %>%
                               mutate(SVE=str_replace_all(SVE,'Barcelonès Sud','Barcelona Sud')) %>%
                               arrange(SVE) %>% slice(1:8,10,9)
names(Taula7_soncas)<-c('SVE','Contactes a cas')

Taula6_simptomàtics<-Total_simptom%>%funcio_corregir_sve() %>% 
                                    arrange(SVE) %>% slice(1:8,10,9)
names(Taula6_simptomàtics)<-c('SVE','Contactes simptomàtics')



# INFOGRÀFIC --------------------------------------------------------------

t.infografic<-Indicadors1_2 %>% mutate(SVE=str_replace_all(SVE,'Barcelona$','Barcelona Ciutat')) %>%
  mutate(SVE=str_replace_all(SVE,'Vallès Oriental i Vallès Occidental','Vallès')) %>% select(1:4,6:7)
#repleguem dades per l'I3
t.infografic_I3_ver<-contactes_2_sve %>% anti_join(dataset.escolar.nc) %>% 
  filter(!is.na(verificació) & verificació!='migrat SVE' & verificació!='contacte altre cas' & verificació!='no és contacte' & verificació!='és cas'& verificació!='contacte no vàlid') %>% 
  group_by(SVE) %>% dplyr::summarize(CE_verificats=n()) %>% 
  funcio_corregir_sve()
t.infografic_I3_t0<-contactes_2_sve %>% filter(verificació=='CIP vàlid') %>% group_by(SVE) %>% dplyr::summarize(CE_t0=n()) %>%
  funcio_corregir_sve()
  

t.infografic_I3_seguiment<-Taula_seguiment %>% filter(Estat=='Realitzat') %>% 
                            group_by(SVE) %>% dplyr::summarize(CE_7_14=as.numeric(Seguiment_dia7)+as.numeric(Seguiment_dia10) +as.numeric(Seguiment_dia14)) %>%
                            full_join(t.infografic_I3_t0,by='SVE') %>%
                            mutate(CE_7_14=replace_na(CE_7_14,0)) %>% #si tenim NAs, els passem a 0
                            mutate(Total_CE_0_7_14=CE_7_14+CE_t0) %>% select(1,4)

t.infografic_I3<-left_join(t.infografic_I3_ver,t.infografic_I3_t0,by='SVE') %>% 
                            mutate('%CE_seguiment'=paste0(round(CE_t0/CE_verificats*100,1),'%')) %>% 
                            left_join(t.infografic_I3_seguiment,  by='SVE') %>% arrange(SVE) %>% 
                            as.data.frame()

t.infografic_I3<-rbind(t.infografic_I3,c('Totals',sum(t.infografic_I3$CE_verificats),sum(t.infografic_I3$CE_t0),
                                         paste0(round(sum(t.infografic_I3$CE_t0)/sum(t.infografic_I3$CE_verificats)*100,1),'%'),sum(t.infografic_I3$Total_CE_0_7_14)))
  

#repleguem dades per l'I4
Taga_SVE_cas<-Taga  %>% funcio_corregir_sve() %>% replace(is.na(.), 0) %>% 
  mutate('%CE_cas'=paste0(round(as.numeric(Si_N)/as.numeric(Total_N)*100,1),'%')) %>% 
  select(1,7)

t.infografic_I4_simp<-Taula_resum_infografic%>% select(1,5,6) %>% full_join(Taga_SVE_cas,by='SVE')


#agrupem
t.infografic.f<- t.infografic %>% full_join(t.infografic_I3,by='SVE') %>% full_join(t.infografic_I4_simp,by='SVE') %>%
                            mutate('%CE_simptomes'=paste0(round(as.numeric(`CE amb símptomes`)/as.numeric(Total_CE_0_7_14) *100,1),'%')) %>% 
                            select(1:11,14,12,13) %>%arrange(SVE) %>%slice(1:8,10,9) %>%replace(is.na(.), 0) %>%  as.data.frame()
                            


names(t.infografic.f)<-c('SVE',
                       '1.1. Nombre total de casos nous confirmats',
                       '1.2. Nombre de casos nous informats',
                       '1.3. Percentatge de casos en els que s\'identifiquen contactes', 
                       '2.1. Nombre total de contactes estrets identificats',
                       '2.2. Mitjana de contactes estrets identificats per cas',
                       '3.1. Total de contactes estrets verificats (excloent 3.1b)',
                       '3.2. Nombre de CE als que se’ls fa la “trucada 0” de seguiment i accepten',
                       '3.3. Percentatge de CE nous que fan seguiment',
                       paste0('3.4. Total de contactes en seguiment setmana ',N_setmana,' (trucada 0, 7 i 14 dies)'),
                       '4.1. Nombre total de contactes que desenvolupen símptomes',
                       '4.2. Percentatge de contactes estrets desenvolupen símptomes',
                       '4.3. Nombre total de contactes estrets que es confirmen com a cas',
                       '4.4. Percentatge de contactes estrets que es confirmen com a cas'
                       )
t.infografic_final<-as.data.frame(t(t.infografic.f[-1]))
names(t.infografic_final)<-t.infografic.f[,1]


# TAULA RESUM SETMANAL ----------------------------------------------------

Resum.setmanal<-c('',
                  t.infografic.f[10,2],#1.1
                  t.infografic.f[10,3],#1.2
                  t.infografic.f[10,4],#1.3
                  '','',
                  t.infografic.f[10,5],#2.1
                  t.infografic.f[10,6],#2.2
                  Indicadors1_2[10,9],#2.2.a
                  paste0('(',Indicadors1_2[10,10],'-',Indicadors1_2[10,11],')'),#2.2.b
                  '','',
                  sum(taula_indicador_3_totals[!is.na(taula_indicador_3_totals$verificació),]$N_contactes),#3.1
                  t.infografic.f[10,7],#3.1a
                  Taula8.b[5,2],#3.1b
                  t.infografic.f[10,8],#3.2
                  t.infografic.f[10,9],#3.3
                  paste0(round(as.numeric(t.infografic.f[10,8])/(as.numeric(t.infografic.f[10,7])-Escolars.NC)*100,1),'%') ,#% escoles 3.2/3.1a-denomEscolars
                  t.infografic.f[10,10],#3.4
                  '','',
                  t.infografic.f[10,11],#4.1
                  t.infografic.f[10,12],#4.2
                  t.infografic.f[10,13],#4.3
                  t.infografic.f[10,14],#4.4
                  ''
                  )

Taula_resum_setmanal<-data.frame(Indicadors$Indicadors,Resum.setmanal)
colnames(Taula_resum_setmanal)[2]<-paste0('Valor SE',N_setmana)

# GRÀFICS -----------------------------------------------------------------


#Figura 3: Distribució del percentatge de contactes estrets per àmbits d’exposició i rang d’edat
colores<-c("#FFCD33","#698ED0","#8CC168","#997300","#264478","#43682B","#FFC000","#4472C4","#9CA09F") ##70AD47

Fig.3<-ggplot(taula_ambits_rangs_sve[!is.na(taula_ambits_rangs_sve$`àmbit contacte`),] %>% mutate(`àmbit contacte`=str_to_sentence(`àmbit contacte`)),
       aes(x=Rang_edat,y=N_contactes,fill=`àmbit contacte`)) +
  geom_bar(
    stat='identity',
    position = 'fill',
    width=0.6
  ) + 
  guides(fill=guide_legend(title=""))+
  scale_y_continuous(labels = scales::percent_format())+ #passem l'eix de les y a %!!!
  scale_fill_manual(values = colores) + 

  ylab(NULL)+
  xlab(NULL)+
  theme_minimal()+
  theme(axis.text.x = element_text(colour = 'black',face = 'bold',size = 16),
        axis.text.y = element_text(face = 'bold',colour='black',size = 16),
        legend.text = element_text(face='bold',size = 18),
        legend.key.size = unit(0.7,'cm'))


ggsave(paste0('./Output/Figura3_SE',N_setmana,'.png'),plot = Fig.3,width = 30.10,height = 17.64,units = 'cm',dpi = 500)

#Figura1:Freqüència relativa de casos informats (I) i no informats (NI),
taula3.plot<-rbind(taula3_actual,taula3_anterior)
taula3.plot<-arrange(taula3.plot,SVE)

taula3.plot$`%`<-as.numeric(taula3.plot$`%`)
taula3.plot$Estat<-as.factor(taula3.plot$Estat)
taula3.plot$Estat<-factor(taula3.plot$Estat,levels=c(paste0('I_SE',N_setmana),
                                                     paste0('I_SE',N_setmana_ant),
                                                     paste0('NI_SE',N_setmana),
                                                     paste0('NI_SE',N_setmana_ant)))

taula3.plot2<-taula3.plot %>% group_by(SVE,Estat,`%`)

Fig1<-ggplot(taula3.plot2, aes(x=SVE,y=`%`,fill=Estat))+
  geom_text(aes(label=Estat),position=position_dodge(width = 0.9),size=5,angle=90,hjust=-0.2,fontface='bold')+
  geom_bar(
    stat='identity',
    position = 'dodge',
    width=0.9
  )+
  geom_col(colour = "black", position = 'fill')+
  guides(fill=guide_legend(title=""))+
  scale_y_continuous(labels = function(x)paste0(x,'%'),limits=c(0,100),breaks = seq(0,100,10))+ #Afegim el '%' al valor i expandim limits fins 100%
  scale_fill_manual(values = c('darkolivegreen4','darkseagreen','firebrick4','coral3')) + 
  ylab(NULL)+
  xlab(NULL)+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust = 0.5,size = 16,colour = 'black',face = 'bold'),
        axis.text.y = element_text(face = 'bold',colour='black',size = 16),
        legend.text = element_text(face='bold',size = 18))
ggsave(paste0('./Output/Figura1_SE',N_setmana,'.png'),plot = Fig1,width = 30.10,height = 17.64,units = 'cm',dpi = 400)


#Figura 2:ambits totals
taula_ambits_contacte_totals_plot$`àmbit contacte`<-str_to_sentence(taula_ambits_contacte_totals_plot$`àmbit contacte`)

plot.data.ambits.t<-taula_ambits_contacte_totals_plot %>% 
  mutate(Percentatge=round(N_contactes/sum(N_contactes)*100,1),
         P.Acumulat=cumsum(Percentatge)) %>%  
  mutate(lab.ypos = P.Acumulat - 0.5*Percentatge)
  
plot.data.ambits.t$`àmbit contacte`<-factor(plot.data.ambits.t$`àmbit contacte`,levels = 
                                              c('Social','Medi transport','Laboral','Escolar','Domicili','Desconegut','Centre sociosanitari',
                                               'Centre sanitari','Altres'))
color.text<-c(replicate(length(colores)-3,'white'),'black','white','black')

Fig.2<-ggplot(plot.data.ambits.t,aes(x='', y=Percentatge,fill=`àmbit contacte`))+
  geom_bar(stat = 'identity',width=1,color='white') +
  coord_polar("y", start = 0)+
  geom_label_repel(aes(y = lab.ypos, label = paste0(`àmbit contacte`,' ',Percentatge, "%"),'',fontface='bold'),
                   size = 8, show.legend = F, nudge_x = 0.7,nudge_y = 0.5, colour='white',segment.colour = 'black',segment.size = 0.8,force = 10,box.padding = 0)+
  #repetim per enviar labels a sobre:
  #geom_label_repel(aes(y = lab.ypos, label = paste0(`àmbit contacte`,' ',Percentatge, "%"),'',fontface='bold'),
                   #size = 8, show.legend = F, nudge_x = 0.7,nudge_y = 1, colour='white',segment.colour = 'black',segment.size = 0.8,force = 10,box.padding = 0,segment.alpha = 0)+ 
  scale_fill_manual(values = colores)+
  theme_void() +
  theme(legend.position = "none")
ggsave(paste0('./Output/Figura2_SE',N_setmana,'.png'),plot = Fig.2,width = 30.10,height = 20,units = 'cm',dpi = 400)

#Figura 4
Fig.4.plot<-Taula8.plot %>% slice(-11) %>% 
  mutate(lab.ypos = sum(Percentatge) - cumsum(Percentatge)+ 0.5*Percentatge)
Fig.4.colors<-c('#4F81BD','#9BBB59','#8064A2','#2C4D75','#4F81BD','#C0504D','#4BACC6','#F79646','#772C2A','#256d7b')


Fig4<-ggplot(Fig.4.plot,aes(x='', y=Percentatge,fill=`verificació`))+
  geom_bar(stat = 'identity',width=0.5,color='white') +
  coord_polar("y", start = 0)+
  geom_label_repel(aes(y = lab.ypos, label = paste0(verificació,' ',Percentatge, "%"),'',fontface='bold'),
                   size = 8, show.legend = F, nudge_x = 0.35,nudge_y = 0.5, colour='white',segment.colour = 'black',segment.size = 0.8,force = 10,box.padding = 0)+
  #repetim per enviar labels a sobre:
  geom_label_repel(aes(y = lab.ypos, label = paste0(verificació,' ',Percentatge, "%"),'',fontface='bold'),
                   size = 8, show.legend = F, nudge_x = 0.35,nudge_y = 0.5, colour='white',segment.colour = 'black',segment.size = 0.8,force = 10,box.padding = 0,segment.alpha = 0)+
  
  scale_fill_manual(values = Fig.4.colors) +
  theme_void() +
  theme(legend.position = "Set1")

ggsave(paste0('./Output/Figura4_SE',N_setmana,'.png'),plot = Fig4,width =30.10,height = 20,units = 'cm',dpi = 400) 


# OUTPUT EXCEL --------------------------------------------------------

write.xlsx(Taula_resum_setmanal,sheetName = paste0('Resum_setmanal_SE_',N_setmana),file = './Output/Indicadorssetmanals.xlsx')
write.xlsx(Indicadors1_2_subset,sheetName = 'Indicadors1_2',file = './Output/Indicadorssetmanals.xlsx',append = T)
write.xlsx(origen.dades,sheetName = 'Origen_dades',file = './Output/Indicadorssetmanals.xlsx',append = T)
#write.xlsx(Taula6_simptomàtics,sheetName = 'Taula6',file = './Output/Indicadorssetmanals.xlsx',append = T) 
write.xlsx(Taula5,sheetName = 'Taula5',file = './Output/Indicadorssetmanals.xlsx',append = T)
write.xlsx(superspreaders,sheetName = 'Superspreaders',file = './Output/Indicadorssetmanals.xlsx',append = T)
write.xlsx(Taula3_final,sheetName = 'Taula3',file = './Output/Indicadorssetmanals.xlsx',append = T) #sense Totals
write.xlsx(taula_indicador_3_totals,sheetName = 'Indicadors_3_Totals',file = './Output/Indicadorssetmanals.xlsx',append = T)
write.xlsx(Indicadors_3_SVE,sheetName = 'Indicadors_3_SVE',file = './Output/Indicadorssetmanals.xlsx',append = T)
write.xlsx(taula_ambits_contacte_SVE,sheetName = 'Àmbits_sve',file = './Output/Indicadorssetmanals.xlsx',append = T)
write.xlsx(taula_ambits_contacte_totals,sheetName = 'Àmbits_totals',file = './Output/Indicadorssetmanals.xlsx',append = T)
write.xlsx(taula_Escoles,sheetName = 'Taula_Escoles',file = './Output/Indicadorssetmanals.xlsx',append = T)
write.xlsx(taula_ambits_rangs,sheetName = 'Àmbits_Rang',file = './Output/Indicadorssetmanals.xlsx',append = T)
write.xlsx(taula_ambits_rangs_sve,sheetName = 'Àmbits_Rang_SVE_mes',file = './Output/Indicadorssetmanals.xlsx',append = T)
write.xlsx(taula_ambits_verificacio,sheetName = 'Àmbits_verificació',file = './Output/Indicadorssetmanals.xlsx',append = T)
write.xlsx(I4_simptomàtics0_7_14,sheetName = 'Simptomàtics',file = './Output/Indicadorssetmanals.xlsx',append = T)
write.xlsx(I4_asimptomàtics0_7_14,sheetName = 'Asimptomàtics',file = './Output/Indicadorssetmanals.xlsx',append = T)
write.xlsx(Taula_seguiment,sheetName = 'Seguiment',file = './Output/Indicadorssetmanals.xlsx',append = T)
write.xlsx(Taula_motius_SVE,sheetName = 'Motius_no_confinament_SVE',file = './Output/Indicadorssetmanals.xlsx',append = T)
#write.xlsx(Taula7_soncas,sheetName = 'Taula7',file = './Output/Indicadorssetmanals.xlsx',append = T)
write.xlsx(Taula8.Informe ,sheetName = 'Taula8',file = './Output/Indicadorssetmanals.xlsx',append = T)
write.xlsx(dataset.escolar.nc.sve ,sheetName = 'Escolars.NoContesten',file = './Output/Indicadorssetmanals.xlsx',append = T)
write.xlsx(t.infografic_final,sheetName = 'Taula_infogr',file = './Output/Indicadorssetmanals.xlsx',append = T)
write.xlsx(Taula_resum_castellano,sheetName = 'taula Madrid SVE',file = './Output/Indicadorssetmanals.xlsx',append=T)
write.xlsx(Taula_madrid_provincia,sheetName = 'Taula Madrid provicias',file = './Output/Indicadorssetmanals.xlsx',append = T)
write.xlsx(Taga,sheetName = 'Ind4_EsCas',file = './Output/Indicadorssetmanals.xlsx',append = T)


# UPDATE TABLES --------------------------------------------------------
`%notin%` <- Negate(`%in%`)
if (paste0('SE',N_setmana,'_CI')%notin%colnames(t.historica)){
  t3.s.act<-Taula3_final %>% select(1:3) %>% mutate_at(c(1:3),as.character())
  colnames(t3.s.act)[2:3]<-c(paste0('SE',N_setmana,'_CI'),paste0('SE',N_setmana,'_CNI'))
  t.historica<-full_join(t.historica,t3.s.act,by='SVE')
  write.xlsx(t.historica,sheetName = 'Hoja1','./Metaarxius/taula3_historica.xlsx',append = F,row.names = F)
}

