[![es](https://img.shields.io/badge/lang-es-yellow.svg)](https://github.com/EfraCL/Conductividad_hidraulica/blob/main/README.md)

## Purpose

The objective of the *chs()* function is to calculate the saturated hydraulic conductivity of a soil, following the methodology established in [Handbook of Plant and Soil Analysis for Agricultural Systems](https://zenodo.org/record/2553445) (page 227).

The function *chs.groups()* is simply a function designed to group the saturated hydraulic conductivity results obtained by applying the function *chs()* into one or more factors (categorical variables). In fact, in order to use the latter function, it is essential to have previously defined the *chs()* function in the working environment.

## Functions´ arguments

[**chs()**](https://github.com/EfraCL/Conductividad_hidraulica/blob/main/Script_chs_chsgroups_functions.R) function takes the following arguments:
- **df**: dataframe that contains, among other information, the data on the volume of water used in the tests and the time necessary for its total infiltration into the soil.
- **vol**: name of the column of the dataframe that contains the information on the volume of water used (in liters) in the tests. The name must be indicated by a vector of length 1 and character type.
- **time**: name of the column of the dataframe containing the information on the time (in seconds) that it took for the water to infiltrate in the tests. The name must be indicated by a vector of length 1 and character type.
- **radio**: numerical vector of length 1 indicating the radius (in meters) of the ring (cylinder) used to perform the saturated hydraulic conductivity tests.
- **vol.converse**: TRUE by default. It indicates that the volume of the column specified in the *vol* argument is expressed in milliliters and therefore must be converted to liters.
- **time.converse**: TRUE by default. It indicates that the time of the column specified in the *time* argument is expressed in minutes and should therefore be converted to seconds.

[**chs.groups()**](https://github.com/EfraCL/Conductividad_hidraulica/blob/main/Script_chs_chsgroups_functions.R) function takes the same arguments that *chs()* and two new ones:
- **group.by**: character type vector that collects the names of the dataframe columns of the *df* argument that you want to use to group the saturated hydraulic conductivity values.
- **unit**: vector of character type and length 1 indicating the units in which the saturated hydraulic conductivity value is to be expressed. The options are: "mms-1" and "cmh-1"). By default the saturated hydraulic conductivity is expressed in mm s-1.

## Examples

The data we will work with in the following examples ([prueba_chs.csv](https://github.com/EfraCL/Conductividad_hidraulica/blob/main/prueba_chs.csv) and [prueba_chsgroups.csv](https://github.com/EfraCL/Conductividad_hidraulica/blob/main/prueba_chsgroups.csv)) have been kindly provided by the [Grupo de Erosión y Conservación De Suelos y Agua](http://www.soilwaterconservation.es/) of [CEBAS-CSIC](http://www.cebas.csic.es/index.html).

### chs() function
To see how the chs() function works, copy and execute the following code: 

~~~~
# Load the data
link <- "https://raw.githubusercontent.com/EfraCL/Conductividad_hidraulica/main/prueba_chs.csv"
x <- read.csv(link, header = T, sep = ";", dec = ",")
rm(link)

# Definition of chs() function
chs <- function(df, vol, time, radio, vol.converse = T, time.converse = T ){
  
  if(vol.converse == T){
    df[vol] <- df[vol]*.001
  } 
  
  if (time.converse == T) {
    df[time] <- df[time]*60
  }

  CompleteObs <- !(is.na(df[[vol]]) | is.na(df[[time]])) # To subsequently eliminate those INCOMPLETE time and volume observations,
  # avoiding problems with the cumsum() function; In which row does the time or volume variable have NA's?
  df <- df[CompleteObs, ]
  rm(CompleteObs)
  
  df$cs_I <- cumsum(df[[vol]] / (pi * radio^2)) # Calculation of cummulative infitration
  df[vol] <- NULL
  
  df$cs_t <- cumsum(df[[time]]) # Calculation of cummulative time
  df[time] <- NULL
  
  modelo <- lm(cs_I ~ cs_t, data = df)
  IR <- modelo$coefficients[[2]]
  rm(modelo, df)
  
  alfa <- .0262 + .0035 * log(IR)
  ks <- IR / (.467 * (1 + 2.92 / (radio * alfa)))
  rm(alfa, IR)
  
  ks
}

# Test of chs()
chs(df = x, vol = "Vol", time = "t", radio = .05)
~~~~

You can download the [dataset](https://github.com/EfraCL/Conductividad_hidraulica/blob/main/prueba_chsgroups.csv) and the [script](https://github.com/EfraCL/Conductividad_hidraulica/blob/main/Script_chs_chsgroups_functions.R) with all the functions.


### chsgroups() function
To see how the chs() function works, copy and execute the following code: 

~~~~
# Loading the data
link <- "https://raw.githubusercontent.com/EfraCL/Conductividad_hidraulica/main/prueba_chsgroups.csv"
x <- read.csv(link, header = T, sep = ";", dec = ",")
rm(link)

# Definimos la función chs.groups(). IMPORTANT: do not forget to define previosly the chs() function
chs.groups <- function(df, group.by ,vol, time, radio, vol.converse = T, time.converse = T, unit = "mms-1"){
  
  if(exists("chs")){
    df <- df[, names(df) %in% c(group.by, vol, time)] # Remove columns that are not of interest to us
    
    temp <- df[[group.by[1]]]
    for (i in 2:length(group.by)){
      temp <- paste(temp, df[[i]]) # Create a vector that groups together all the grouping variables
    }
    
    df$grouping <- temp # Include the previous vector in the column of the dataframe
    rm(temp)
    
    for (i in group.by){
      x[[i]] <- NULL # Remove columns that are not of interest to us
    }
    rm(group.by)
    
    list_temp <- split(df, df["grouping"]) # Split the dataframe following the new column created
    temp <- c()
    df_def <- data.frame(Factor = names(list_temp), Ks_mm_s = NA) # Create a new dataframe where we store the Ks values
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

# chsgroups() trial
grupos <- c("Año", "Zona", "Bloque", "Tratamiento", "Repeticion")
chs.groups(df = x, group.by = grupos, vol = "Vol", time = "t", radio = .05, unit = "cmh-1")
~~~~

You can download the [dataset](https://github.com/EfraCL/Conductividad_hidraulica/blob/main/prueba_chsgroups.csv) and the [script](https://github.com/EfraCL/Conductividad_hidraulica/blob/main/Script_chs_chsgroups_functions.R) with all the functions.

I hope you find it useful! :P
