# Función chs() ----
chs <- function(df, vol, time, radio, vol.converse = T, time.converse = T ){
  
  if(vol.converse == T){
    df[vol] <- df[vol]*.001
  } 
  
  if (time.converse == T) {
    df[time] <- df[time]*60
  }
  
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
    
    temp <- df[[group.by[1]]]
    for (i in 2:length(group.by)){
      temp <- paste(temp, df[[i]]) # Creamos un vector que aglutine todas las variables de agrupación
    }
    
    df$grouping <- temp # Incluimos el vector anterior en la columna del dataframe
    rm(temp)
    
    for (i in group.by){
      x[[i]] <- NULL # Eliminamos las columnas que no nos interesan
    }
    rm(group.by)
    
    list_temp <- split(df, df["grouping"]) # Dividimos el dataframe en base a la nueva columna creada
    temp <- c()
    df_def <- data.frame(Factor = names(list_temp), Ks_mm_s = NA) # Creamos un nuevo dataframe donde almacenaremos la Ks
    for (i in names(list_temp)){
      temp <- list_temp[[i]]
      Ks <- chs(df = temp, vol = "Vol", time = "t", radio = .05)
      if(unit == "mms-1") {
        df_def[df_def$Factor == i, 2] <- Ks
      } else if (unit == "cmh-1"){
        df_def[df_def$Factor == i, 2] <- Ks * 360
      }
    }
    df_def
  } else {
    print("Error: Debes definir primero la función chs()")
  }
}
