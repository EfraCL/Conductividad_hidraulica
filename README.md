*chs()* & *chsgroups()*

## Finalidad

El objetivo de la función *chs()* es calcular la conductividad hidráulica saturada de un suelo, siguiendo la metodología establecida en el [Handbook of Plant and Soil Analysis for Agricultural Systems](https://zenodo.org/record/2553445) (página 227).

Por su parte, la función *chsgroups()* no es más que una función que contiene la función *chs()* y que ha sido diseñada para agrupar en uno o más factores (variables categóricas) los resultados de conductividad hidráulica saturada. De hecho, para poder utilizar esta última función, es indispensable haber definido previamente la función *chs()* en el entorno de trabajo.

## Argumentos de las funciones

La función **chs()** admite dos argumentos:
- **df**: es el vector a utilizar para realizar los subconjuntos sobre los que se aplicará una función determinada.
- **amplitud**: este argumento hace referencia a, como su propio nombre indica, a la amplitud de los subconjuntos sobre los que
queremos aplicar una función concreta. Por ejemplo, si queremos aplicar la función sum() en subconjuntos de 3, 4 o 5 elementos.

## Observaciones
Por defecto la función aplica la función sum() a cada subconjunto de datos, ya que es la función necesaria para
calcular la intensidad de lluvia.

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
