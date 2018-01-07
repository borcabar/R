# Práctica 4

# Fundamentos Matemáticas. Profe: Cristina Sanchez
# Estudiante: Boris Cabrera 201710808

# 1.¿Existe una clara relación entre el carácter de los perros y la raza? ¿Y de los gatos? 
library(dplyr)
library(tidyr)
ADP4p1<-select(filter(AnimalData,AnimalData$Animal.Type=="Dog"),Aggressive:Good.with.Kids,Dog.Group)
View(ADP4p1)

#con esta función cuento todos los valores de cada característica
ADP4p1 %>%
  group_by(Dog.Group) %>%
  summarise(Aggressive=n(),Independent=n(),Intelligent=n(),Loyal=n(),Social=n(),Good.with.Kids=n())

#A tibble: 8 x 7
#Dog.Group Aggressive Independent Intelligent Loyal Social Good.with.Kids
#<chr>      <int>       <int>       <int> <int>  <int>          <int>
#  1   Crossbreed          2           2           2     2      2              2
#2      Herding         34          34          34    34     34             34
#3        Hound         22          22          22    22     22             22
#4 Non-Sporting         19          19          19    19     19             19
#5     Sporting         49          49          49    49     49             49
#6      Terrier         73          73          73    73     73             73
#7          Toy         64          64          64    64     64             64
#8      Working         28          28          28    28     28             28

#Hago una tabla mostrando el numero de registros por cada animal
RpR<-ADP4p1 %>%
  group_by(Dog.Group) %>%
  summarise(Aggressive=n())

#registro los positivos del caracter de cada tipo de perro
caracter<-ADP4p1 %>%
  group_by(Dog.Group) %>%
  summarise(Aggressive=length(Aggressive[Aggressive=="Y"]),
            Independent=length(Independent[Independent=="Y"]),
            Intelligent=length(Intelligent[Intelligent=="Y"]),
            Loyal=length(Loyal[Loyal=="Y"]),
            Social=length(Social[Social=="Y"]),
            Good.with.Kids=length(Good.with.Kids[Good.with.Kids=="Y"]))

n_distinc(Aggressive)

# A tibble: 8 x 7
#Dog.Group Aggressive Independent Intelligent Loyal Social Good.with.Kids
#<chr>      <int>       <int>       <int> <int>  <int>          <int>
#  1   Crossbreed          0           1           2     1      2              2
#2      Herding          8           5          28    21     15             34
#3        Hound          6           2          10    11     18             13
#4 Non-Sporting          2           9           5    14      5             17
#5     Sporting          0          32          41    47     36             49
#6      Terrier          6           7          11    53     13             70
#7          Toy          0           7          52    11      5             18
#8      Working          1           1          27    26     11             26

#Asi facilmente podemos comparar y ver que razas tienen que caranterísticas
# Herding: siendo 34 especímenes podemos inferir que son Inteligentes y leales, y sobretodo Good.with.Kids
# Hound: sociales 18/22
# Non-Sporting: leales y buenos con los niños
# Sporting: Sobretodo buenos con los niños, pero también inteligentes y leales
# Terrier: destqacan por ser sociales y buenos con los niños
# Toy: son inteligentes
# WorkingIneligentes, sociales y buenos con los niños

Como



# 2. Caracteriza qué tipo de perro suelen abandonar los dueños. Analiza por un lado el carácter y 
#  por otro edad, condición, raza, sexo, si estaban castrados o no... 
ADP4p2<-filter(AnimalData,AnimalData$Intake.Type=="Owner Surrender")

#aprovechando el script anterior, veamos el carácter
ADP4p2 %>%
  summarise(Aggressive=n(),Independent=n(),Intelligent=n(),Loyal=n(),Social=n(),Good.with.Kids=n())

# A tibble: 1 x 6
Aggressive Independent Intelligent Loyal Social Good.with.Kids
<int>       <int>       <int> <int>  <int>          <int>
  1        129         129         129   129    129            129

ADP4p2 %>%
  summarise(Aggressive=length(Aggressive[Aggressive=="Y"]),
            Independent=length(Independent[Independent=="Y"]),
            Intelligent=length(Intelligent[Intelligent=="Y"]),
            Loyal=length(Loyal[Loyal=="Y"]),
            Social=length(Social[Social=="Y"]),
            Good.with.Kids=length(Good.with.Kids[Good.with.Kids=="Y"]))
# A tibble: 1 x 6
Aggressive Independent Intelligent Loyal Social Good.with.Kids
<int>       <int>       <int> <int>  <int>          <int>
  1         57          67          93   102     80            114

# Es curioso!, el principal rasgo es que son buenos con los niños!, luego que son animales leales. 
# la correlación con agresivos e independientes es la más baja.
# La gente abandona los perros cuando estos son buenos.

#Veamos otras variables

ADP4p2 %>%
  group_by(Neutered.Status) %>%
  summarise(n())
# A tibble: 3 x 2
Neutered.Status `n()`
<chr> <int>
  1          Intact    10
2        Neutered    70
3          Spayed    49
#la mayoría de los animales estan castrados

ADP4p2 %>%
  group_by(Sex) %>%
  summarise(n())
# A tibble: 2 x 2
Sex `n()`
<chr> <int>
  1 Female    52
2   Male    77
# Los machos son más abandonados, pero no parece muy concluyente debido a la pequeña diferencia.


ADP4p2 %>%
  group_by(Condition) %>%
  summarise(n())
# A tibble: 2 x 2
Condition `n()`
<chr> <int>
  1 Injured or Sick    19
2          Normal   110
#Estan en buena forma la mayoría de animales

ADP4p2 %>%
  summarize_each(funs(mean,max),Age.Intake,Weight)
# A tibble: 1 x 4
Age.Intake_mean Weight_mean Age.Intake_max Weight_max
<dbl>       <dbl>          <dbl>      <dbl>
  1        2.736434    20.86721             17         97
#Son animales jovenes, pero sobretodo ligeros, no pesan mucho.



# 3. Caracteriza qué tipo de perro suelen adoptar en menos de 15 días. Analiza por un lado el 
#  carácter y por otro edad, condición, raza, sexo, si estaban castrados o no... 

ADP4p3<-filter(AnimalData,AnimalData$Days.Shelter<15 & AnimalData$Outcome.Type=="Adoption")
# son 114 entradas, veamos

ADP4p3 %>%
  summarise(Aggressive=length(Aggressive[Aggressive=="Y"]),
            Independent=length(Independent[Independent=="Y"]),
            Intelligent=length(Intelligent[Intelligent=="Y"]),
            Loyal=length(Loyal[Loyal=="Y"]),
            Social=length(Social[Social=="Y"]),
            Good.with.Kids=length(Good.with.Kids[Good.with.Kids=="Y"]))
# A tibble: 1 x 6
Aggressive Independent Intelligent Loyal Social Good.with.Kids
<int>       <int>       <int> <int>  <int>          <int>
  1         33          48          82    80     62             92
# El resultado es similar al del abandono, que curioso!, los animales adoptados más rápidamente 
# son buenos con los niños, inteligentes y leales.

ADP4p3 %>%
  group_by(Neutered.Status) %>%
  summarise(n())
# A tibble: 2 x 2
Neutered.Status `n()`
<chr> <int>
  1        Neutered    60
2          Spayed    54
#Todos están castrados... probablemente sea política del refugio de dar en adopción solo animales castrados

ADP4p3 %>%
  group_by(Sex) %>%
  summarise(n())
# A tibble: 2 x 2
Sex `n()`
<chr> <int>
  1 Female    54
2   Male    60
#Se adoptan de ambos sexos

ADP4p3 %>%
  group_by(Condition) %>%
  summarise(n())
# A tibble: 2 x 2
Condition `n()`
<chr> <int>
  1 Injured or Sick     7
2          Normal   107
# tampoco aprece que haya relación con la salud

ADP4p3 %>%
  summarize_each(funs(mean,max),Age.Intake,Weight)
# A tibble: 1 x 4
Age.Intake_mean Weight_mean Age.Intake_max Weight_max
<dbl>       <dbl>          <dbl>      <dbl>
  1        1.526316    17.08026             15         70
#Son animales jovenes, aunque uno de 17 años fué adoptado. Y son peso medio.


#PRACTICA 5

#Pregunta 1

#con ADP5p1 selecciono la edad y raza de los perros dejados por sus dueños
ADP5p1<-select(filter(AnimalData,Animal.Type=='Dog' & Intake.Type=='Owner Surrender'),Dog.Group,Age.Intake)

#por el histograma de edades me parece lo mejor dividir en (0,1,5,10,20), como ('cachorro','joven','adulto','mayor')
ADP5p1$Age.Intake <- cut(ADP5p1$Age.Intake,c(0,1,5,10,20), labels=c('cachorro','joven','adulto','mayor'),include.lowest = TRUE)

#averiguo el total de perros por cada raza para conocer la proporción de los dejados por dueños
ADP5p11<-select(filter(AnimalData,Animal.Type=='Dog'),Dog.Group,Age.Intake)
ADP5p11$Age.Intake <- cut(ADP5p11$Age.Intake,c(0,1,5,10,20), labels=c('cachorro','joven','adulto','mayor'),include.lowest = TRUE)

#data frame de los perros dejados por raza
View(ADP5p1)
RaEd<-ADP5p1 %>%
  group_by(Dog.Group) %>%
  summarise(Age.Intake=n())
colnames(RaEd)<-c('Dog.Group','Age.Surrended')
#averigüo el total de animales dejados por sus dueños
sum(RaEd[,2])

#dataframe de los perros totales por raza
RaEd1<-ADP5p11 %>%
  group_by(Dog.Group) %>%
  summarise(Age.Intake=n())
sum(RaEd1[,2])
colnames(RaEd1)<-c('Dog.Group','Age.All')

#uno los dos data frame para tener los valores lado a lado
RaEdt<-left_join(RaEd,RaEd1,by='Dog.Group')

#con esta operación obtengo los porcentajes
RaEdt1<-RaEdt %>%
  mutate(Age=Age.Surrended/Age.All) %>%
  mutate(Age.Surrended=NULL,Age.All=NULL)

#segunda parte del ejercicio

#con este dataframe proceso las edades de los dejados
RaEd2<-ADP5p1 %>%
  group_by(Age.Intake) %>%
  summarise(Dog.Group=n())
colnames(RaEd2)<-c('Age','Surrended')

#con este dataframe proceso las edades de todos
RaEd3<-ADP5p11 %>%
  group_by(Age.Intake) %>%
  summarise(Dog.Group=n())
colnames(RaEd3)<-c('Age','Total')

#uno los dos data frame para tener los valores lado a lado
RaEd4<-left_join(RaEd2,RaEd3,by='Age')

#con esta operación obtengo los porcentajes
RaEd5<-RaEd4 %>%
  mutate(Proportion=Surrended/Total)

View(RaEd5)

#Pregunta 2
ADP5p2<-select(filter(AnimalData, Animal.Type=='Dog'),Dog.Group,Outcome.Type,Color)
View(ADP5p2)

#veo las razas que hay
razas<-ADP5p2 %>%
  group_by(Dog.Group) %>%
  summarise(Color=n()) %>%
  arrange(desc(Color))
vec_razas<-c('Terrier','Toy','Sporting')

#veo el tipo de outcomes que hay
outcomes<-ADP5p2 %>%
  group_by(Outcome.Type) %>%
  summarise(Color=n()) %>%
  arrange(desc(Color))
View(outcomes)
vec<-outcomes[,1]
vec_outcomes<-c('Adoption','Transfer','Return to Owner','Humane Euthanasia','Died')

#Veo los colores presentes
color1<-ADP5p2 %>%
  group_by(Color) %>%
  summarise(Outcome.Type=n()) %>%
  arrange(desc(Outcome.Type))
color2<-table(color1)
color1<-data.frame(color1)
vec_color<-color2[,1]

#Terrier
Raza1<-select(filter(ADP5p2,Dog.Group==vec_razas[1]),Outcome.Type,Color) %>%
  group_by(Color)

colorR1<-Raza1 %>%
  group_by(Color) %>%
  summarise(Outcome.Type=n())
colorR1<-data.frame(colorR1)

tab1<-data.frame(with(Raza1,prop.table(table(Raza1),2)))
tab2<-spread(tab1,Outcome.Type,Freq)
tab2<-table(tab2)

tab3<-left_join(tab2,colorR1,'Color')

#Toy
Raza1<-select(filter(ADP5p2,Dog.Group==vec_razas[2]),Outcome.Type,Color) %>%
  group_by(Color)

colorR1<-Raza1 %>%
  group_by(Color) %>%
  summarise(Outcome.Type=n())
colorR1<-data.frame(colorR1)

tab1<-data.frame(with(Raza1,prop.table(table(Raza1),2)))
tab2<-spread(tab1,Outcome.Type,Freq)


tab3<-left_join(tab2,colorR1,'Color')

#Toy
Raza1<-select(filter(ADP5p2,Dog.Group==vec_razas[3]),Outcome.Type,Color) %>%
  group_by(Color)

colorR1<-Raza1 %>%
  group_by(Color) %>%
  summarise(Outcome.Type=n())
colorR1<-data.frame(colorR1)

tab1<-data.frame(with(Raza1,prop.table(table(Raza1),2)))
tab2<-spread(tab1,Outcome.Type,Freq)


tab3<-left_join(tab2,colorR1,'Color')


View(tab2)
View(colorR1)










#Pregunta 3

# que caracteristica tienen los adoptados en menos de 15 días
ADP5p3<-filter(AnimalData,Outcome.Type=='Adoption' & Days.Shelter<15 & Animal.Type=='Dog')

#conocer los numeros globales
dim(filter(AnimalData,Outcome.Type=='Adoption' & Days.Shelter<15 & Animal.Type=='Dog'))
dim(filter(AnimalData,Outcome.Type=='Adoption' & Animal.Type=='Dog'))

#Ver si hay diferencia de sexo
dim(filter(AnimalData,Outcome.Type=='Adoption' & Days.Shelter<15 & Animal.Type=='Dog' & Sex=='Female'))
dim(filter(AnimalData,Outcome.Type=='Adoption' & Animal.Type=='Dog' & Sex=='Female'))

#En edad de ingreso
hist(ADP5p3$Age.Intake,20,include.lowest = TRUE)
length(ADP5p3$Age.Intake[ADP5p3$Age.Intake==1])
length(ADP5p3$Age.Intake)

hist(AnimalData$Age.Intake,20,include.lowest = TRUE)
length(AnimalData$Age.Intake[AnimalData$Age.Intake==1])
length(AnimalData$Age.Intake)

#Condicion
dim(filter(AnimalData,Outcome.Type=='Adoption' & Days.Shelter<15 & Animal.Type=='Dog' & Condition=='Normal'))
dim(filter(AnimalData,Outcome.Type=='Adoption' & Animal.Type=='Dog' & Condition=='Normal'))

#Veamos si el color hace alguna diferencia
ds1<-ADP5p3 %>%
  group_by(Color) %>%
  summarise(Dog.Group=n())
colnames(ds1)<-c('Color','rap')

ds2<-AnimalData %>%
  group_by(Color) %>%
  summarise(Dog.Group=n())
colnames(ds2)<-c('Color','total')

ds3<-left_join(ds1,ds2,by='Color')
ds4<-ds3 %>%
  mutate(proporcion=rap/total)
arrange(ds4,desc(proporcion))
View(ds4)

#La fecha de ingreso aprece interesante, pero ahora no se me ocurre como

#Veamos las diferentes variables, como llego al refugio
intake1<-ADP5p3 %>%
  group_by(Intake.Type) %>%
  summarise(Intake.Date=n())
colnames(intake1)<-c('Intake.Type','Adoptados')

intake2<-filter(AnimalData,Outcome.Type=='Adoption' & Animal.Type=='Dog') %>%
  group_by(Intake.Type) %>%
  summarise(Intake.Date=n())
colnames(intake2)<-c('Intake.Type','Total')

intake3<-left_join(intake1,intake2,by='Intake.Type')
View(intake3)

