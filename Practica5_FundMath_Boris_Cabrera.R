# Práctica 5

# Fundamentos Matemáticas. Profe: Cristina Sanchez
# Estudiante: Boris Cabrera 201710808

# 1. Exite una dependencia entre la raza de perro y la probabilidad de ser abandonado 
#por los dueños?, Y entre la edad?, (factoriza la edad mediante la función cut)

library(dplyr)
ADP5p1<-select(filter(AnimalData,AnimalData$Animal.Type=="Dog" & AnimalData$Intake.Type=="Owner Surrender"),Breed,Age.Intake)
View(ADP5p1)

#Hago un dataset con la información de la raza, la edad de perros abandonados.... preferiría usar 
# Dog.Group porque las variables tienen más miembros y los resultados son más representativos, pero el 
# ejercicio es explicito en ello

#con esta función cuento todos los valores de cada característica
ADP5p1 %>%
  group_by(Breed) %>%
  summarise(Age.Intake=n())

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

ADP5p1 %>%
  group_by(Dog.Group) %>%
  summarise(Aggressive=length(Aggressive[Aggressive=="Y"]),
            Independent=length(Independent[Independent=="Y"]),
            Intelligent=length(Intelligent[Intelligent=="Y"]),
            Loyal=length(Loyal[Loyal=="Y"]),
            Social=length(Social[Social=="Y"]),
            Good.with.Kids=length(Good.with.Kids[Good.with.Kids=="Y"]))


# 2. Del grupo de perros de los que llegaron más animales, existe una dependencia 
# entre la forma en la que se fueron del refugio y su color?

# 3. Que variable ofrece más garantía de que un perro se adoptará en menos de 15 días?