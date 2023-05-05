*chs()* & *chsgroups()*

## Finalidad

El objetivo de la función *chs()* es calcular la conductividad hidráulica saturada de un suelo, siguiendo la metodología establecida en el [Handbook of Plant and Soil Analysis for Agricultural Systems](https://zenodo.org/record/2553445) (página 227).

Por su parte, la función *chsgroups()* no es más que una función que contiene la función *chs()* y que ha sido diseñada para agrupar en uno o más factores (variables categóricas) los resultados de conductividad hidráulica saturada. De hecho, para poder utilizar esta última función, es indispensable haber definido previamente la función *chs()* en el entorno de trabajo.

## Argumentos de las funciones

La función [**chs()**](https://github.com/EfraCL/Conductividad_hidraulica/blob/main/Script_chs_chsgroups_functions.R) admite los siguientes argumentos:
- **df**: dataframe que contiene, entre otra información, los datos de volumen de agua usado en los ensayos y el tiempo necesario para su total infiltración en el suelo.
- **vol**: nombre de la columna del dataframe que contiene la información sobre el volumen de agua utilizado (en litros) en los ensayos. El nombre debe indicarse entrecomillado o por medio de un vector de longitud 1 y de tipo caracter.
- **time**: nombre de la columna del dataframe que contiene la información sobre el tiempo (en segundos) que tardó el agua en infiltrarse en los ensayos. El nombre debe indicarse entrecomillado o por medio de un vector de longitud 1 y de tipo caracter.
- **radio**: vector numérico de longitud 1 indicando el radio (en metros) del anillo utilizado para hacer los ensayos de conductividad hidráulica saturada.
- **vol.converse**: por defecto este argumento es TRUE. Indica que el volumen de la columna especificada en el argumento *vol* está expresado en mililitros y por tanto, debe transformarse a litros.
- **time.converse**: por defecto este argumento es TRUE. Indica que el tiempo de la columna especificada en el argumento *time* está expresado en minutos y por tanto, debe transformarse a segundos.

La función [**chsgroups()**](https://github.com/EfraCL/Conductividad_hidraulica/blob/main/Script_chs_chsgroups_functions.R) admite los mismos argumentos que *chs()* y dos nuevos:
- **group.by**: vector de tipo caracter que recoge los nombres de las columnas del dataframe del argumento *df* que se quieren utilizar para agrupar los valores de conductividad hidráulica saturada.
- **unit**: vector de tipo caracter y longitud 1 que indica las unidades en las que se debe expresar el valor de conductividad hidráulica saturada. Las opciones son: "mms-1" y "cmh-1"). Por defecto la conductividad hidráulica saturada se expresa en mm s-1.

## Ejemplo:

Copia y pega las siguientes líneas de código y observa cómo trabaja la función:
~~~
intensidad<-function(x,amplitud){
  c<-c()
  i<-1
  a<-amplitud-1
  lon<-length(x)-a
  while(i<=lon){
    c[i]<-sum(x[i:(i+a)])
    i<-i+1}
  return(c)}

a<-1:5
intensidad(a,amplitud=2)
~~~
