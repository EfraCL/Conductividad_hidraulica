# Función chs() ----
chs <- function(df, vol, time, radio, vol.converse = T, time.converse = T ){
  
  if(vol.converse == T){
    df[vol] <- df[vol]*.001
  } 
  
  if (time.converse == T) {
    df[time] <- df[time]*60
  }
  
  CompleteObs <- !(is.na(df[[vol]]) | is.na(df[[time]])) # Para posteriormente eliminar aquellas observaciones de tiempo y volumen INCOMPLETAS,
  # evitando prblemas con la función cumsum(); ¿En qué fila la variable tiempo o la de volumen tiene NA's?
  df <- df[CompleteObs, ]
  rm(CompleteObs)
  
  df$cs_I <- cumsum(df[[vol]] / (pi * radio^2)) # Calculo de infiltracion acumulada
  df[vol] <- NULL
  
  df$cs_t <- cumsum(df[[time]]) # Calculo de tiempo acumulado
  df[time] <- NULL
  
  modelo <- lm(cs_I ~ cs_t, data = df)
  IR <- modelo$coefficients[[2]]
  rm(modelo, df)
  
  alfa <- .0262 + .0035 * log(IR)
  ks <- IR / (.467 * (1 + 2.92 / (radio * alfa)))
  rm(alfa, IR)
  
  ks
}


# Función chs.groups() ----
chs.groups <- function(df, group.by ,vol, time, radio, vol.converse = T, time.converse = T, unit = "mms-1"){
  
  if(exists("chs")){
    df <- df[, names(df) %in% c(group.by, vol, time)] # Eliminamos del dataframe las columnas que no interesan
    
    if(length(group.by) > 1){ # Si queremos agrupar por más de una variable, p.ej.: tratamiento, bloque y zona
      
      temp <- df[[group.by[1]]]
      for (i in 2:length(group.by)){
        temp <- paste(temp, df[[i]], sep = "-") # Creamos un vector que aglutine todas las variables de agrupación
        }
      
      df$grouping <- temp # Incluimos el vector anterior en la columna del dataframe
      rm(temp)
      df[, names(df) %in% group.by] <- NULL # Eliminamos las columnas que no nos interesan
      
      list_temp <- split(df, df["grouping"]) # Dividimos el dataframe en base a la nueva columna creada
      
      df_def <- data.frame(do.call("rbind", strsplit(names(list_temp), split = "-"))) # Creamos el dataframe para almacenar los datos de Ks

    } else { # En caso de querer agrupar sólo por una variable, p.ej.: tratamiento o bloque, no hace falta tanta parafernalia
      list_temp <- split(df, df[group.by]) # Dividimos el data.frame en base a la columna agrupadora
      
      df_def <- data.frame(a = names(list_temp))
    }
    
    if (unit == "mms-1"){
      df_def$Ks <- NA # Creamos la columna donde vamos a almacenar los datos de Ks
      names(df_def) <- c(group.by, "Ks_mms-1")
    } else {
      df_def$Ks <- NA # Creamos la columna donde vamos a almacenar los datos de Ks
      names(df_def) <- c(group.by, "Ks_cmh-1")
    }
    rm(df, group.by)
    
    temp <- c()
    e <- 1
    ks_column <- dim(df_def)[2]
    
    for (i in names(list_temp)){
      temp <- list_temp[[i]]
      Ks <- chs(df = temp, vol = vol, time = time, 
                radio = radio, vol.converse = vol.converse,
                time.converse = time.converse)
      if(unit == "mms-1") {
        df_def[e, ks_column] <- Ks
      } else if (unit == "cmh-1"){
        df_def[e, ks_column] <- Ks * 360
      }
     e <- e + 1 
    }
    rm(i, e, Ks, ks_column, time, unit, vol)
    df_def
    }
  else {
    print("Error: Debes definir primero la función chs()")
  }
}
