*chs()* & *chsgroups()*

## Finalidad

El objetivo de la función *chs()* es calcular la conductividad hidráulica saturada de un suelo, siguiendo la metodología establecida en [Handbook of Plant and Soil Analysis for Agricultural Systems](https://zenodo.org/record/2553445) (página 227).

Por su parte, la función *chsgroups()* no es más que una función que contiene la función *chs()* y que ha sido diseñada para agrupar en uno o más factores (variables categóricas) los resultados de conductividad hidráulica saturada. De hecho, para poder utilizar esta última función, es indispensable haber definido previamente la función *chs()* en el entorno de trabajo.

## Argumentos de las funciones

La función [**chs()**](https://github.com/EfraCL/Conductividad_hidraulica/blob/main/Script_chs_chsgroups_functions.R) admite los siguientes argumentos:
- **df**: dataframe que contiene, entre otra información, los datos de volumen de agua usado en los ensayos y el tiempo necesario para su total infiltración en el suelo.
- **vol**: nombre de la columna del dataframe que contiene la información sobre el volumen de agua utilizado (en litros) en los ensayos. El nombre debe indicarse por medio de un vector de longitud 1 y de tipo caracter.
- **time**: nombre de la columna del dataframe que contiene la información sobre el tiempo (en segundos) que tardó el agua en infiltrarse en los ensayos. El nombre debe indicarse por medio de un vector de longitud 1 y de tipo caracter.
- **radio**: vector numérico de longitud 1 indicando el radio (en metros) del anillo utilizado para hacer los ensayos de conductividad hidráulica saturada.
- **vol.converse**: por defecto este argumento es TRUE. Indica que el volumen de la columna especificada en el argumento *vol* está expresado en mililitros y por tanto, debe transformarse a litros.
- **time.converse**: por defecto este argumento es TRUE. Indica que el tiempo de la columna especificada en el argumento *time* está expresado en minutos y por tanto, debe transformarse a segundos.

La función [**chsgroups()**](https://github.com/EfraCL/Conductividad_hidraulica/blob/main/Script_chs_chsgroups_functions.R) admite los mismos argumentos que *chs()* y dos nuevos:
- **group.by**: vector de tipo caracter que recoge los nombres de las columnas del dataframe del argumento *df* que se quieren utilizar para agrupar los valores de conductividad hidráulica saturada.
- **unit**: vector de tipo caracter y longitud 1 que indica las unidades en las que se debe expresar el valor de conductividad hidráulica saturada. Las opciones son: "mms-1" y "cmh-1"). Por defecto la conductividad hidráulica saturada se expresa en mm s-1.

## Ejemplos

### Función chs()
Para ver cómo trabaja la función chs() descarga [este dataset](https://github.com/EfraCL/Conductividad_hidraulica/blob/main/prueba_chs.csv) y ejecuta el siguiente código:

~~~~
# Cargamos los datos
link <- "https://raw.githubusercontent.com/EfraCL/Conductividad_hidraulica/main/prueba_chs.csv"
x <- read.csv(link, header = T, sep = ";", dec = ",")
rm(link)

# Definimos la función chs()
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

# Prueba de chs()
chs(df = x, vol = "Vol", time = "t", radio = .05)

~~~~

### Función chsgroups()
Para ver cómo trabaja la función chsgroups() descarga [este dataset](https://github.com/EfraCL/Conductividad_hidraulica/blob/main/prueba_chsgroups.csv) y el script con el código de ejemplo

~~~~
link <- "https://raw.githubusercontent.com/EfraCL/Conductividad_hidraulica/main/prueba_chsgroups.csv"
x <- read.csv(link, header = T, sep = ";", dec = ",")
rm(link)

chs.groups <- function(df, group.by ,vol, time, radio, vol.converse = T, time.converse = T, unit = "mms-1"){
  
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
  
  print(df_def)
}

grupos <- c("Año", "Zona", "Bloque", "Tratamiento", "Repeticion")
chs.groups(df = x, group.by = grupos, vol = "Vol", time = "t", radio = .05, unit = "cmh-1")
~~~~


¡Espero que os sea de utilidad! :P
