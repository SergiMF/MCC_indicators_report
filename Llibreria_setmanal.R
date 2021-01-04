#Funció per a posar la SVE en tots els pacients, en funció de la seva UVE,, municipi, o regió, per aquest ordre de preferència
funcio_posar_sve_pacients<- function(df, codis){
  codis$nom_unitat_de_poblacio<-toupper(codis$nom_unitat_de_poblacio) #passem a majuscules
  
  #mirem la funció de posar SVE:
  #corretgim el que sempre esta mal escrit, si hi es
  if("PARC DELS PRÍNCEPS UN EL" %in% df$municipi){
    df$municipi[df$municipi == "PARC DELS PRÍNCEPS UN EL"] <- "FOGARS DE LA SELVA"
    #print("ok1")
  }
  if("VALL DE CARDÓS LA" %in% df$municipi){
    df$municipi[df$municipi == "VALL DE CARDÓS LA"] <- "VALL DE CARDÓS"
    #print("ok1")
  }
  if("SAUS. CAMALLERA I LLAMPAIES" %in% df$municipi){
    df$municipi[df$municipi == "SAUS. CAMALLERA I LLAMPAIES"] <- "SAUS"
    #print("ok1")
  }
  
  if("SANT MIQUEL DE BALANYÀ" %in% df$municipi){
    df$municipi[df$municipi == "SANT MIQUEL DE BALANYÀ"] <- "SANT MIQUEL DE BALENYA"
    #print("ok1")
  }
  if("CRUÏLLES. MONELLS I SSADURNÍ L'HEURA" %in% df$municipi){
    df$municipi[df$municipi == "CRUÏLLES. MONELLS I SSADURNÍ L'HEURA"] <- "MONELLS"
    #print("ok1")
  }
  if("FALS" %in% df$municipi){
    df$municipi[df$municipi == "FALS"] <- "FONOLLOSA"
    #print("ok1")
  }
  if("CALVET EL" %in% df$municipi){
    df$municipi[df$municipi == "CALVET EL"] <- "CALVET"
    #print("ok1")
  }
  # treiem totes les comes
  if("," %in% df$municipi){
    df$municipi <- gsub(",", "", df$municipi)
    #print("ok2")
  }
  #fem que la columna Municipi es digui igual que al arxiu "codis"
  df$nom_unitat_de_poblacio <- df$municipi
  drops <- c("municipi")
  df <- df[,!(names(df) %in% drops)]
  #print("ok3")
  #join municipis
  df_sve <- join(df, 
                 codis[codis$categoria == "Municipi" | codis$categoria == "Població" | codis$categoria=="Ciutat" | codis$categoria=="Poble" | codis$categoria=="Vila",],
                 by="nom_unitat_de_poblacio", 
                 type="left", 
                 match="first")
  
  for(i in 1:nrow(df_sve)){
    if(!is.na(df_sve$UVE[i]) & df_sve$UVE[i] !='N/A'){
      SVE_UVE<-rs_sve$SVE[rs_sve$UVE==df_sve$UVE[i]]
      df_sve$SVE[i]<-SVE_UVE
    }
  }
  
  #print("ok4")
  # guardem tots els municipis trobats com a municipi
  df_muni <- df_sve[which(df_sve$categoria == "Municipi" | df_sve$categoria == "Població" | df_sve$categoria=="Ciutat" | df_sve$categoria=="Poble" | df_sve$categoria=="Vila"),]
  #print("ok5")
  # guardem els que no hem trobat, a veure què podem fer
  df_na <- df[which(!df$nom_unitat_de_poblacio %in% df_muni$nom_unitat_de_poblacio),]
  #print("ok6")
  # si per sort eren tot municipis, que ens ho digui i ens quedem nomes amb els de municipis
  if(nrow(df_na) == 0){ #si el nombre de files del df amb no-municipis es 0
    return(df_sve) #retorna'm el primer df join
    print(paste0(nrow(df_na) == nrow(df)))
  }else{ #sino, fem aixo:
    
    #dels que no hem trobat com a municipi farem 3 outputs
    output1 <- c() # aquells que nomes apareixen un cop (ideal)
    output2 <- c() # els que apareixen dos cops però perque l'arxiu "codis" es la fussio de dues bbdd i en realitat nomes apareix un cop
    output3 <- c() # els que apareixen més d'un cop
    
    for(i in 1:length(unique(df_na$nom_unitat_de_poblacio))){
      # aquells que nomes apareixen un cop (ideal)
      if(
        nrow(
          codis[codis$nom_unitat_de_poblacio ==
                unique(df_na$nom_unitat_de_poblacio)[i]
                ,]
        ) == 1
      ){
        output1 <- c(output1,
                     unique(df_na$nom_unitat_de_poblacio)[i])
        #print("ok8")
      }
      # apareix dues vegades
      if(
        nrow(
          codis[codis$nom_unitat_de_poblacio ==
                unique(df_na$nom_unitat_de_poblacio)[i]
                ,]
        ) == 2 &
        #pero una es un "NA", perque la 2na bbdd de codis no et deia la cateogria ergo apareix com a NA
        NA %in% codis$categoria[codis$nom_unitat_de_poblacio ==
                                unique(df_na$nom_unitat_de_poblacio)[i]]
      ){
        output2 <- c(output2,
                     unique(df_na$nom_unitat_de_poblacio)[i])
        #print("ok9")
      }
      if(
        #estan dos cops sense NA
        nrow(
          codis[codis$nom_unitat_de_poblacio ==
                unique(df_na$nom_unitat_de_poblacio)[i]
                ,]
        ) == 2 &
        !NA %in% codis$categoria[codis$nom_unitat_de_poblacio ==
                                 unique(df_na$nom_unitat_de_poblacio)[i]]
        | #  o més de dos cops
        nrow(
          codis[codis$nom_unitat_de_poblacio ==
                unique(df_na$nom_unitat_de_poblacio)[i]
                ,]
        ) > 2
      ){
        output3 <- c(output3,
                     unique(df_na$nom_unitat_de_poblacio)[i])
        #print("ok10")
      }
    }
  }
  #print(output3)
  
  # dels que nomes estan un cop o dos cops amb NA (com si fos 1 cop), els juntem directament
  df_sve12_na <- join(
    df_na[
      which(df_na$nom_unitat_de_poblacio %in% output1 |
              df_na$nom_unitat_de_poblacio %in% output2),], 
    codis, 
    by="nom_unitat_de_poblacio",
    type="left", 
    match="first")
  #print("ok11")
  #print(df_sve12_na$Regió)
  
  # com es possible que tinguem "NULL"s, en output1 i output2,
  # si tenim null els assignem forçadament la SVE que els tocaria per RS
  # el mateix amb la provincia
  if("regió" %in% colnames(df_sve12_na)){
    for(i in 1:nrow(df_sve12_na)){
      if(df_sve12_na$nom_unitat_de_poblacio[i] == "NULL"){
        df_sve12_na$SVE[i] <- rs_sve$SVE[rs_sve$RS == df_sve12_na$regió[i]]
        df_sve12_na$Provincia[i] <- rs_sve$Provincia[rs_sve$RS == df_sve12_na$regió[i]] 
      }
      # si pel que sigui algun nom de l'output no estava a codis, també li posem la SVE
      # segons la seva RS
      if(!df_sve12_na$nom_unitat_de_poblacio[i] %in% codis$nom_unitat_de_poblacio){
        df_sve12_na$SVE[i] <- rs_sve$SVE[rs_sve$RS == df_sve12_na$regió[i]]
        df_sve12_na$Provincia[i] <- rs_sve$Provincia[rs_sve$RS == df_sve12_na$regió[i]] 
      }
    }
  }
  
  # en cas de que no tinguin la columna "Regio", com es el cas de
  #contactes nous a seguir per dia, contactes simptomatics,etc
  # i tinguin output3, es a dir els noms repetits en llocs diferents
  #posa'm la primera SVE on coincideixi el nom de la poblacio, sigui on sigui
  if(!"regió" %in% colnames(df_sve12_na) &
     length(output3)>0){
    df_sve3_na <- join(df_na[
      which(df_na$nom_unitat_de_poblacio %in% output3),], 
      codis,
      by="nom_unitat_de_poblacio", 
      type="left", 
      match="first")
    df_sve123_na <- rbind(df_muni, #join final
                          df_sve12_na,
                          df_sve3_na)
  }
  
  #en cas de que tinguem RS, pero tampoc cap nom de poblacio repetit, 
  # no fem cap join nou, l'output 3 es 0
  if(!"regió" %in% colnames(df_sve12_na) &
     !length(output3)>0){
    df_sve123_na <- rbind(df_muni, #join final
                          df_sve12_na)
    df_sve3_na <- c()
  }
  
  
  codis_o3 <- codis[-c(1:nrow(codis)),] # dummy codis
  #print(codis_o3)
  
  #en cas de que SI tinguem Regio i el output 3 sigui mes gran que 0, es a dir tenim repetits
  # eliminem totes les poblacions a codis que tenen una SVE diferent a la que tocaría segons RS (dit codis_o3, de codis output3)
  
  
  if(length(output3)>0 &
     "regió" %in% colnames(df_na)){
    print("si sve3")
    for(i in 1:length(unique(output3))){
      #print("ok12.3")
      rs_o3 <- na.omit(df_na$regió[df_na$nom_unitat_de_poblacio == unique(output3)[i]])
      #print("ok12.4")
      sve_o3 <- na.omit(rs_sve$SVE[rs_sve$RS == rs_o3])
      #print("ok12.5")
      codis_o3_loop <- codis[
        which(codis$nom_unitat_de_poblacio == unique(output3)[i] & codis$SVE == sve_o3) ,]
      #print("ok12.6")
      codis_o3 <- rbind(codis_o3, codis_o3_loop)
    }
    #print("ok13")
    
    # els que no hagis trobat, treu-me'ls de codis
    codis_o3 <- codis_o3[!is.na(codis_o3$nom_unitat_de_poblacio),]
    
    # passam el nom dels municipis a majuscules
    codis_o3$nom_municipi <- toupper(codis_o3$nom_municipi)
    
    # despres fem el join amb el df_na amb noms de l'output3 + codis_o3
    
    df_sve3_na <- join(df_na[
      which(df_na$nom_unitat_de_poblacio %in% output3),], 
      codis_o3,
      by="nom_unitat_de_poblacio", 
      type="left", 
      match="first")
    
    #print(unique(df_sve3_na$nom_unitat_de_poblacio))
  }
  
  #sino tenim cap NA a la SVE, fem el join final
  if(!NA %in% unique(df_sve3_na$SVE) &
     "regió" %in% colnames(df_na)){
    df_sve123_na <- rbind(df_muni,  #join final
                          df_sve12_na, 
                          df_sve3_na)
    
  }
  
  # si tenim algun NA al df amb sve de les poblacions de l'output3, 
  # es a dir ens hem passat filtrant per RS a codis_o3 i ens hem carregat la poblacio 
  # vol dir que tenim dos registres amb el mateix nom a RS diferents, ergo s'han esborrat una a l'altra i per aixo no s'han trobat
  #en aquest cas cal crear un codis amb un registre unic, on esta el seu nom de poblacio i la seva RS
  
  if(NA %in% unique(df_sve3_na$SVE) &
     "regió" %in% colnames(df_na)){
    keepers <- c(
      which(colnames(df_sve3_na) %in% colnames(df_na))
    ) #agafem les columnes que teniem al inici, eliminant les del join codis
    
    df_sve4_na <- df_sve3_na[-c(1:nrow(df_sve3_na)),] #dummy
    
    #fem un 4rt df_na amb els df_sve_3 que ens han donat NA, es a dir
    # els noms de l'output3 que no hem trobat per culpa de filtrar per RS
    df_na_4 <- df_sve3_na[is.na(df_sve3_na$SVE),keepers] 
    
    #filtrem codis per aquella unica poblacio a una unica RS
    codis_o4 <- codis[which(codis$nom_unitat_de_poblacio %in% df_na_4$nom_unitat_de_poblacio),]
    
    #join de codis amb una unica fila de df_na dels no trobats de l'output3 pel filtre de RS
    for(i in 1:nrow(df_na_4)){
      #print("ok12.3")
      rs_o4 <- df_na_4$regió[i]
      sve_o4 <- rs_sve$SVE[rs_sve$RS == rs_o4]
      codis_o4_i <- codis_o4[which(codis_o4$SVE == sve_o4),]
      df_na_i <- df_na_4[i,]
      
      #print(codis_o4_i$SVE)
      #print(df_na_i$nom_unitat_de_poblacio)
      
      df_sve4_na_i <- join(df_na_i, 
                           codis_o4,
                           by="nom_unitat_de_poblacio", 
                           type="left", 
                           match="first")
      df_sve4_na <- rbind(df_sve4_na, df_sve4_na_i)
    }
    #@sm:els que queden amb SVE=NA els forcem a canviar SVE per la regió a codis
    for(i in 1:nrow(df_sve4_na)){
      if(is.na(df_sve4_na$SVE[i])){
        rs_05<-df_sve4_na$regió[i]
        df_sve4_na$SVE[i]<-rs_sve$SVE[rs_sve$RS == rs_05]
      }
      
    }
    
    
    #print(df_sve4_na$SVE)
    print("yes sve4+Regio")
    df_sve123_na <- rbind(df_muni, #join final
                          df_sve12_na, 
                          df_sve3_na[!is.na(df_sve3_na$SVE),],
                          df_sve4_na)
    
  }
  #@sm:miro diferències per detectar casos amb errors de dades als seus municipis
  cip.diferencies<-as.vector(setdiff(df$CIP,df_sve123_na$CIP))
  municipi.diferences<-unlist(lapply(cip.diferencies,FUN = function(x){return(df$nom_unitat_de_poblacio[df$CIP==x])}))
  Errors.municipis<-data.frame(CIP=cip.diferencies,Municipi=municipi.diferences)
  
  #vaig a buscar a df aquells cips i els hi forço 
  
  print("Alguna fila sense SVE?")
  print(NA %in% unique(df_sve123_na$SVE))
  print("Tenim les mateixes files que a l'inici?")
  print(paste0(nrow(df_sve123_na)))
  print(nrow(df))
  print('Detectat algun CIP amb municipi erroni?')
  print(Errors.municipis)
  return(df_sve123_na)
  
}
#funció per posar la SVE en els contactes. Enfunció de la seva UVE, municipi, o la SVE del cas:
funcio_sve_contactes<-function(df,codis){
  codis$nom_unitat_de_poblacio<-toupper(codis$nom_unitat_de_poblacio) #passem a majuscules
  
  #mirem la funció de posar SVE:
  #corretgim el que sempre esta mal escrit, si hi es
  
  if("SANT MIQUEL DE BALANYÀ" %in% df$municipi){
    df$municipi[df$municipi == "SANT MIQUEL DE BALANYÀ"] <- "SANT MIQUEL DE BALENYA"
    #print("ok1")
  }
  if("CRUÏLLES. MONELLS I SSADURNÍ L'HEURA" %in% df$municipi){
    df$municipi[df$municipi == "CRUÏLLES. MONELLS I SSADURNÍ L'HEURA"] <- "MONELLS"
    #print("ok1")
  }
  # treiem totes les comes
  if("," %in% df$municipi){
    df$municipi <- gsub(",", "", df$municipi)
    #print("ok2")
  }
  #fem que la columna Municipi es digui igual que al arxiu "codis"
  df$nom_unitat_de_poblacio <- df$municipi
  drops <- c("municipi")
  df <- df[,!(names(df) %in% drops)]
  #print("ok3")
  #join municipis
  
  #mirem les poblacions que aporten els contactes i els hi busquem les seves SVE
  df_sve <- join(df, 
                 codis[codis$categoria == "Municipi" | codis$categoria == "Població" | codis$categoria=="Ciutat" | codis$categoria=="Poble" | codis$categoria=="Vila",],
                 by="nom_unitat_de_poblacio", 
                 type="left", 
                 match="first")
  names(df_sve)[42]<-'SVE_contactes'
  #mantenim les SVE dels pacients, excepte per aquells contactes que aporten municipi
  for (i in 1:nrow(df)){
    if(!is.na(df_sve$SVE_contactes[i])){
      df$SVE[i]<-df_sve$SVE_contactes[i]
    }
  }
  
  return(df)
  
}

#@sm:IMORTANT PELS MOTIUS!!!!
#Pensar en obtenir una segona df amb els motisu de no confinament per a aquells que fem seguiment. Sortida com a llista de df!!

funcio_CE_seguiment<-function(df,dia.seg,setmana){
  #manego possibles errors
  if((dia.seg==7 & setmana==-3)|(dia.seg==14 & setmana==-2)){stop(paste0('Error: No hi poden haver seguiments a dia ',dia.seg,' a la setmana ',setmana, ', per la setmana que estas estudiant'))}
  
  dill<-floor_date(x = as.Date(Sys.Date()), 
                   unit = "week",
                   week_start = 1)
  dill<-dill-7
  dium<-dill+6
  # Mirem si tenim dades de seguiment (que l'estat estigui pendent o realitzat i d'aquests que tinguin data de seguiment a la setm actual)
  if(
    (if(setmana==-2){is.element('Realitzat',df$estat)==T||is.element('Pendent',df$estat==T)}
     else if(setmana==-3){is.element('Realitzat',df$estat3)==T||is.element('Pendent',df$estat3==T)}
    )
    && 
    (
      if(setmana==-2){nrow(df %>% filter(dmy(data)>=dill & dmy(data)<=dium))>0}
      else if(setmana==-3){nrow(df %>% filter(dmy(data4)>=dill & dmy(data4)<=dium))>0}
    )
  ){
    #calculo dif data verificació i data seguiment
    if(setmana==-2){
      n<- as.data.frame(df%>%filter(estat%in%c('Realitzat','Pendent'))%>% filter(dmy(data)>=dill & dmy(data)<=dium) %>% 
                          mutate(Dia_seguiment=round(time_length(interval(as_date(dmy_hms(`data verificació`)),dmy(data)),'days'),1)) %>%
                          filter(Dia_seguiment==dia.seg) %>% #filtrem pels dies de seguiment que estem buscant a la consulta
                          group_by(SVE,estat)%>%dplyr::summarize(N=n()))
      colnames(n)[2:3]<-c('Estat',paste0('Seguiment_dia',dia.seg))
      if(nrow(n)==0){n[nrow(n)+1,]<-c('Sense seguiments','Sense seguiments',0)}
      #motius:
      
      Mot<-as.data.frame(df%>%filter(estat%in%c('Realitzat','Pendent'))%>% filter(dmy(data)>=dill & dmy(data)<=dium) %>% 
                           mutate(Dia_seguiment=round(time_length(interval(as_date(dmy_hms(`data verificació`)),dmy(data)),'days'),1)) %>%
                           filter(Dia_seguiment==dia.seg) %>%
                           filter(`confinat casa`=='No')%>%group_by(SVE,`confinament altre`)%>%dplyr::summarize(N=n()))
      colnames(Mot)[2:3]=c('Motiu',paste0('Motius_dia_',dia.seg))
      if(nrow(Mot)==0){Mot[nrow(Mot)+1,]<-c('Sense seguiments','Sense seguiments',0)}
      
      #Simptomatics
      Simpt<-as.data.frame(df%>%filter(estat%in%c('Realitzat','Pendent'))%>% filter(dmy(data)>=dill & dmy(data)<=dium) %>% 
                             mutate(Dia_seguiment=round(time_length(interval(as_date(dmy_hms(`data verificació`)),dmy(data)),'days'),1)) %>%
                             filter(Dia_seguiment==dia.seg) %>%
                             filter(símptomes2=='Sí')%>%group_by(SVE)%>%dplyr::summarize(N=n()))
      colnames(Simpt)[2]=c(paste0('Simptomàtics_dia_',dia.seg))
      if(nrow(Simpt)==0){Simpt[nrow(Simpt)+1,]<-c('Sense seguiments',0)}
      
      #Asimptomatics
      Asimpt<-as.data.frame(df%>%filter(estat%in%c('Realitzat','Pendent'))%>% filter(dmy(data)>=dill & dmy(data)<=dium) %>% 
                              mutate(Dia_seguiment=round(time_length(interval(as_date(dmy_hms(`data verificació`)),dmy(data)),'days'),1)) %>%
                              filter(Dia_seguiment==dia.seg) %>%
                              filter(símptomes2=='No')%>%group_by(SVE)%>%dplyr::summarize(N=n()))
      colnames(Asimpt)[2]=c(paste0('Asimptomàtics_dia_',dia.seg))
      if(nrow(Asimpt)==0){Asimpt[nrow(Asimpt)+1,]<-c('Sense seguiments',0)}
      
      
    }else if(setmana==-3){
      n<- as.data.frame(df%>%filter(estat3%in%c('Realitzat','Pendent'))%>%filter(dmy(data4)>=dill & dmy(data4)<=dium) %>% 
                          mutate(Dia_seguiment=round(time_length(interval(as_date(dmy_hms(`data verificació`)),dmy(data4)),'days'),1)) %>%
                          filter(Dia_seguiment==dia.seg) %>% #filtrem pels dies de seguiment que estem buscant a la consulta
                          group_by(SVE,estat3)%>%dplyr::summarize(N=n()))
      colnames(n)[2:3]<-c('Estat',paste0('Seguiment_dia',dia.seg))
      if(nrow(n)==0){n[nrow(n)+1,]<-c('Sense seguiments','Sense seguiments',0)}
      
      #motius:
      Mot<-as.data.frame(df%>%
                           filter(estat3%in%c('Realitzat','Pendent'))%>%filter(dmy(data4)>=dill & dmy(data4)<=dium) %>% 
                           mutate(Dia_seguiment=round(time_length(interval(as_date(dmy_hms(`data verificació`)),dmy(data4)),'days'),1)) %>%
                           filter(Dia_seguiment==dia.seg) %>% 
                           filter(`confinat casa5`=='No')%>%group_by(SVE,`confinament altre6`)%>%dplyr::summarize(N=n()))
      colnames(Mot)[2:3]=c('Motiu',paste0('Motius_dia_',dia.seg))
      if(nrow(Mot)==0){Mot[nrow(Mot)+1,]<-c('Sense seguiments','Sense seguiments',0)}
      
      #simptomatics
      Simpt<-as.data.frame(df%>%filter(estat%in%c('Realitzat','Pendent'))%>%filter(dmy(data4)>=dill & dmy(data4)<=dium) %>% 
                             mutate(Dia_seguiment=round(time_length(interval(as_date(dmy_hms(`data verificació`)),dmy(data4)),'days'),1)) %>%
                             filter(Dia_seguiment==dia.seg) %>%
                             filter(símptomes7=='Sí')%>%group_by(SVE)%>%dplyr::summarize(N=n()))
      colnames(Simpt)[2]=c(paste0('Simptomàtics_dia_',dia.seg))
      if(nrow(Simpt)==0){Simpt[nrow(Simpt)+1,]<-c('Sense seguiments',0)}
      
      #Asimptomatics
      Asimpt<-as.data.frame(df%>%filter(estat%in%c('Realitzat','Pendent'))%>%filter(dmy(data4)>=dill & dmy(data4)<=dium) %>% 
                              mutate(Dia_seguiment=round(time_length(interval(as_date(dmy_hms(`data verificació`)),dmy(data4)),'days'),1)) %>%
                              filter(Dia_seguiment==dia.seg) %>%
                              filter(símptomes7=='No')%>%group_by(SVE)%>%dplyr::summarize(N=n()))
      colnames(Asimpt)[2]=c(paste0('Asimptomàtics_dia_',dia.seg))
      if(nrow(Asimpt)==0){Asimpt[nrow(Asimpt)+1,]<-c('Sense seguiments',0)}
      
    }
    
  }else{
    if(setmana==-2){
      n<-as.data.frame(df%>% filter(estat%in%c('Realitzat','Pendent') && (dmy(data)>=dill & dmy(data)<=dium))%>%group_by(SVE,estat)%>%
                         dplyr::summarize(N=n()))
      n[nrow(n)+1,]<-c('Sense seguiments','Sense seguiments',0)
      colnames(n)[2:3]<-c('Estat',paste0('Seguiment_dia',dia.seg))
      Mot<-n
      colnames(Mot)[2:3]<-c('Motiu',paste0('Motius_dia_',dia.seg))
      
      Simpt<-select(n,1,3)
      colnames(Simpt)[2]<-paste0('Simptomàtics_dia_',dia.seg)
      
      Asimpt<-Simpt
      colnames(Asimpt)[2]<-paste0('Asimptomàtics_dia_',dia.seg)
      
    }else if(setmana==-3){
      n<-as.data.frame(df%>% filter(estat3%in%c('Realitzat','Pendent') && (dmy(data4)>=dill & dmy(data4)<=dium))%>%group_by(SVE,estat3)%>%
                         dplyr::summarize(N=n()))
      n[nrow(n)+1,]<-c('Sense seguiments','Sense seguiments',0)
      colnames(n)[2:3]<-c('Estat',paste0('Seguiment_dia',dia.seg))
      Mot<-n
      colnames(Mot)[2:3]<-c('Motiu',paste0('Motius_dia_',dia.seg))
      
      Simpt<-select(n,1,3)
      colnames(Simpt)[2]<-paste0('Simptomàtics_dia_',dia.seg)
      Asimpt<-Simpt
      colnames(Asimpt)[2]<-paste0('Asimptomàtics_dia_',dia.seg)
    }
  }
  list.df<-list(n,Mot,Simpt,Asimpt)
  return(list.df)
}
#Funció per canviar els noms de les SVE a taules agregades
#Input: Df amb columna SVE
#output: df amb SVE correctes
funcio_corregir_sve<-function(df){
  if('SVE'%in%colnames(df)){
    df_modif<-df %>% 
      mutate(SVE=str_replace_all(SVE,'Barcelona$','Barcelona Ciutat')) %>%
      mutate(SVE=str_replace_all(SVE,'Barcelonès Sud','Barcelona Sud')) %>% 
      mutate(SVE=str_replace_all(SVE,'Vallès Oriental i Vallès Occidental','Vallès'))
    return(df_modif)
  }else{
    stop('Error: el df d\'estudi no té una columna SVE')
  }
}
funcio_CE_seguiment2<-function(df,dia.seg){
  #manego possibles errors
  #if((dia.seg==7 & setmana==-3)|(dia.seg==14 & setmana==-2)){stop(paste0('Error: No hi poden haver seguiments a dia ',dia.seg,' a la setmana ',setmana, ', per la setmana que estas estudiant'))}
  
  dill<-floor_date(x = as.Date(Sys.Date()), 
                   unit = "week",
                   week_start = 1)
  dill<-dill-7
  dium<-dill+6
  # Mirem si tenim dades de seguiment (que l'estat estigui pendent o realitzat i d'aquests que tinguin data de seguiment a la setm actual)
  
  #calculo dif data verificació i data seguiment
  n<- as.data.frame(df%>%filter(estat%in%c('Realitzat','Pendent'))%>% filter((dmy(data)>=dill||dmy(data4)>=dill) & (dmy(data)<=dium||dmy(data4))) %>% 
                      mutate(Dia_seguiment1=round(time_length(interval(as_date(dmy_hms(`data verificació`)),dmy(data)),'days'),1),
                             Dia_seguiment2=round(time_length(interval(as_date(dmy_hms(`data verificació`)),dmy(data4)),'days'),1)) %>%
                      filter(Dia_seguiment1==dia.seg|Dia_seguiment2==dia.seg) %>% #filtrem pels dies de seguiment que estem buscant a la consulta
                      group_by(SVE,estat)%>%dplyr::summarize(N=n())) %>% as.data.frame()
  colnames(n)[2:3]<-c('Estat',paste0('Seguiment_dia',dia.seg))
  if(nrow(n)==0){n[nrow(n)+1,]<-c('Sense seguiments','Sense seguiments',0)}

  #motius:
  
  Mot<-as.data.frame(df%>%filter(estat%in%c('Realitzat','Pendent'))%>% filter((dmy(data)>=dill||dmy(data4)>=dill) & (dmy(data)<=dium||dmy(data4))) %>% 
                       mutate(Dia_seguiment1=round(time_length(interval(as_date(dmy_hms(`data verificació`)),dmy(data)),'days'),1),
                              Dia_seguiment2=round(time_length(interval(as_date(dmy_hms(`data verificació`)),dmy(data4)),'days'),1)) %>%
                       filter(Dia_seguiment1==dia.seg|Dia_seguiment2==dia.seg))
  Mot.1<-Mot %>% filter(Dia_seguiment1==dia.seg & `confinat casa`=='No') %>% group_by(SVE,`confinament altre`)%>%dplyr::summarize(N=n()) %>% as.data.frame()
  colnames(Mot.1)[2:3]=c('Motiu',paste0('Motius_dia_',dia.seg))
  Mot.2<-Mot %>% filter(Dia_seguiment2==dia.seg & `confinat casa5`=='No') %>% group_by(SVE,`confinament altre6`)%>%dplyr::summarize(N=n()) %>% as.data.frame()
  colnames(Mot.2)[2:3]=c('Motiu',paste0('Motius_dia_',dia.seg))
  if(nrow(Mot.1)==0 & nrow(Mot.2)==0){
    Mot<-Mot.1 %>% as.data.frame()
    
    Mot[nrow(Mot)+1,]<-c('Sense seguiments','Sense seguiments',0)
  }else{
    Mot<-rbind(Mot.1,Mot.2)
  }
  
  
  #Simptomatics
  Simpt<-as.data.frame(df%>%filter(estat%in%c('Realitzat','Pendent'))%>% filter((dmy(data)>=dill||dmy(data4)>=dill) & (dmy(data)<=dium||dmy(data4))) %>% 
                         mutate(Dia_seguiment1=round(time_length(interval(as_date(dmy_hms(`data verificació`)),dmy(data)),'days'),1),
                                Dia_seguiment2=round(time_length(interval(as_date(dmy_hms(`data verificació`)),dmy(data4)),'days'),1)) %>%
                         filter(Dia_seguiment1==dia.seg|Dia_seguiment2==dia.seg))
  Simpt1<-Simpt %>% filter(Dia_seguiment1==dia.seg & símptomes2=='Sí')%>%group_by(SVE)%>%dplyr::summarize(N=n()) %>% as.data.frame()
  colnames(Simpt1)[2]=paste0('Simptomàtics_dia_',dia.seg)
  Simpt2<-Simpt %>% filter(Dia_seguiment2==dia.seg & símptomes7=='Sí')%>%group_by(SVE)%>%dplyr::summarize(N=n()) %>% as.data.frame()
  colnames(Simpt2)[2]=c(paste0('Simptomàtics_dia_',dia.seg))
  if(nrow(Simpt1)==0 & nrow(Simpt2)==0){
    Simpt<-Simpt1 %>% as.data.frame()
    
    Simpt[nrow(Simpt)+1,]<-c('Sense seguiments',0)
  }else{
    Simpt<-rbind(Simpt1,Simpt2)
  }                      
  
  #Asimptomatics
  Asimpt<-as.data.frame(df%>%filter(estat%in%c('Realitzat','Pendent'))%>% filter((dmy(data)>=dill||dmy(data4)>=dill) & (dmy(data)<=dium||dmy(data4))) %>% 
                          mutate(Dia_seguiment1=round(time_length(interval(as_date(dmy_hms(`data verificació`)),dmy(data)),'days'),1),
                                 Dia_seguiment2=round(time_length(interval(as_date(dmy_hms(`data verificació`)),dmy(data4)),'days'),1)) %>%
                          filter(Dia_seguiment1==dia.seg|Dia_seguiment2==dia.seg))
  Asimpt1<-Asimpt %>% filter(Dia_seguiment1==dia.seg & símptomes2=='No')%>%group_by(SVE)%>%dplyr::summarize(N=n()) %>% as.data.frame()
  colnames(Asimpt1)[2]=paste0('Asimptomàtics_dia_',dia.seg)
  Asimpt2<-Asimpt %>% filter(Dia_seguiment2==dia.seg & símptomes7=='No')%>%group_by(SVE)%>%dplyr::summarize(N=n()) %>% as.data.frame()
  colnames(Asimpt2)[2]=c(paste0('Asimptomàtics_dia_',dia.seg))
  if(nrow(Asimpt1)==0 & nrow(Asimpt2)==0){
    Asimpt<-Asimpt1 %>% as.data.frame()
    
    Asimpt[nrow(Asimpt)+1,]<-c('Sense seguiments',0)
  }else{
    Asimpt<-rbind(Asimpt1,Asimpt2)
  } 
  
  
  
  list.df<-list(n,Mot,Simpt,Asimpt)
  return(list.df)
}
function.excelcombined<- function (file, ...){
  require(xlsx, quietly = TRUE)
  objects <- list(...)
  fargs <- as.list(match.call(expand.dots = TRUE))
  objnames <- as.character(fargs)[-c(1, 2)]
  nobjects <- length(objects)
  for (i in 1:nobjects) {
    if (i == 1)
      write.xlsx(objects[[i]], file, sheetName = objnames[i])
    else write.xlsx(objects[[i]], file, sheetName = objnames[i],
                    append = TRUE)
  }
}

#Obj:Calcular contactes a Cas desde MCC
#inp:contactes df amb SVE calculades + indicadors1_2 amb casos per sve
#out:df amb dades agregades de pacients a cas
funcio_aCAs_MCC<-function(df,dfInd){
  agregat<-df %>% filter(verificació%in%'és cas') %>% group_by(SVE) %>% dplyr::summarize(Si_N=n()) %>% funcio_corregir_sve()
  Taula<- agregat %>% left_join(dfInd %>% select(SVE,`1.1-casos_totals`),by='SVE') %>% 
    mutate(No_N=as.numeric(`1.1-casos_totals`)- as.numeric(Si_N)) %>% 
    mutate('No_%'=paste0(round(No_N/(No_N+Si_N)*100,1),'%'),
           'Si_%'=paste0(round(Si_N/(No_N+Si_N)*100,1),'%')
    ) %>% select(SVE,No_N,`No_%`,Si_N,`Si_%`,`1.1-casos_totals`) %>% arrange(SVE)
  
  Taula[,c(2,4,6)]<- sapply(Taula[,c(2,4,6)],as.numeric) %>% replace(is.na(.), 0)
  colnames(Taula)[6]<-'Total_N'
  Taula<-rbind(Taula,c('Totals',
                       sum(Taula$No_N),
                       paste0(round(sum(Taula$No_N)/(sum(Taula$No_N)+sum(Taula$Si_N))*100,1),'%'),
                       sum(Taula$Si_N),
                       paste0(round(sum(Taula$Si_N)/(sum(Taula$No_N)+sum(Taula$Si_N))*100,1),'%'),
                       sum(Taula$Total_N)
                         ))
  Taula<-Taula %>% as.data.frame()
  return(Taula)            
           
}

#Seguiment, motius i (A)Simptomàtics:
#Corregir aquells rows que tenen SVE= Sense seguiments, resultat d'un rbind
funcio_sense_seg<-function(df){
  if(nrow(df[df$SVE!='Sense seguiments',])>0){
    df<-df[df$SVE!='Sense seguiments',]
  }else{
    df<-df %>% distinct()
  }
  return(df)
}
