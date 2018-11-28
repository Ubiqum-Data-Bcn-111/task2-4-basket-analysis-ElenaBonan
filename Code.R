#basket analysis #final code
setwd("C:/Users/Elena/Desktop/Git/Nuova cartella/task2-4-basket-analysis-ElenaBonan")
library(arules)
library(arulesViz)
library(readr)
library(stringr)
library(rlist)
file <- read.transactions('ElectronidexTransactions2017.csv', format= 'basket', sep=',', rm.duplicates = TRUE)

Type <- read.csv(file="Type1.csv",header=T, sep=";")
names(Type)[1]<- 'Laptops'
for (i in 1:ncol(Type))
{Type[,i] <- as.character(Type[,i])}

v = c()
for (i in file@itemInfo$labels){
  n = T
  for (j in 1: ncol(Type))
  { 
    if (i %in% Type[,j]) 
    {v = c(v,names(Type[j]))
    n = F
    } 
  }
  if (n) v = c(v, 'NonClass') }

#which (v == 'NonClass')
#file@itemInfo$labels[which (v == 'NonClass')]
v[[10]]= names(Type)[1]
v[[12]] = names(Type)[8]
v[[15]] = names(Type)[1]
v[[102]] = names(Type)[8]
v[[104]] = names(Type)[8]
v[[106]] = names(Type)[8]
v[[113]] = names(Type)[15]


#Add the info about the product 

file@itemInfo$Type = v

# The following format is to check relation using items

file2 = aggregate(file,  file@itemInfo$Type )

# The folllowing format is to check the items with repetitions
# create a data frame

trans <- as(file, "list")
data = list()

for (i in 1:length(trans)){ 
  count = list()
  list1 = v[match(unlist(trans[[i]],use.names=FALSE), unlist(file@itemInfo$labels, use.names=FALSE))]
  for (j in 1:length(names(Type))){
    count[[j]]= length(which(list1== names(Type)[j]))}
  data = list.append(data, count)
}

df <- data.frame(matrix(unlist(data), nrow=9835, byrow=T),stringsAsFactors=FALSE)
names(df)=names(Type)
df$count = df[1]+df[2]+df[3]+df[4]+df[5]+df[6]+df[7]+df[8]+df[9]+df[10]+df[11]+df[12]+df[13]+df[14]+df[15]+df[16]+df[17]
ccc = df$count





#histogram(as.character(df$count[,1]))
barplot(table(df$count[,1]))
summary(df$count)


MyData1 <- subset(df,((df$Laptops + df$Desktop + (df$Monitors * 0.5))>2.5 | df$Laptops>2 |df$Desktop>2 | df$Printers>1 |df$External.Hardrives>2 | 
                        ((df$Computer.Mice+df$Keyboard+ 2*(df$Mouse.and.Keyboard.Combo))>4) | (df$Computer.Headphones+df$Active.Headphones)>3))


#new = file2[c(as.integer(row.names(MyData1)))]
new = file2[c(row.names(MyData1))]
itemFrequencyPlot(new, type= c('absolute'),topN=20, xlab='Item', names=TRUE)


Ruleapriori<- apriori (new, parameter = list(supp = 0.01, conf = 0.70))
ins <- length(inspect(Ruleapriori))
#Improve removing repetitions and items with lift <=1 

rules.sub <- subset(Ruleapriori, lift > 1.1)

plot(rules.sub)

top.support <- sort(rules.sub, decreasing = TRUE, na.last = NA, by = "lift")
inspect(head(top.support, 20))

new2 = file2[-(c(as.integer(row.names(MyData1))))]
image(new2)
itemFrequencyPlot(new2, type= c('absolute'),topN=20, xlab='Item', names=TRUE)
Ruleapriori1<- apriori (new2, parameter = list(supp = 0.01, conf = 0.50))
top.support1 <- sort(Ruleapriori1, decreasing = TRUE, na.last = NA, by = "lift")
inspect(head(top.support1, 50))

# Rules for item forgetting the type
#Business

new3 = file[c(row.names(MyData1))]
Ruleapriori2<- apriori (new3, parameter = list(supp = 0.05, conf = 0.60))
Ruleapriori2 <- subset(Ruleapriori2, lift > 1.1)
top.support2 <- sort(Ruleapriori2, decreasing = TRUE, na.last = NA, by = "lift")
inspect(head(top.support2, 50))

#Custumer 

new4 = file[-(c(as.integer(row.names(MyData1))))]
Ruleapriori3<- apriori (new4, parameter = list(supp = 0.005, conf = 0.10))
Ruleapriori3 <- subset(Ruleapriori3, lift > 1.1)
top.support3 <- sort(Ruleapriori3, decreasing = TRUE, na.last = NA, by = "lift")
inspect(head(top.support3, 50))

# check the percentage for type of product 

c1 = itemFrequency(new, type='relative')
c1 = sort(c, decreasing = TRUE)
c1 %o%100

c2 = itemFrequency(new2, type='relative')
c2 = sort(c2%o%100, decreasing = TRUE)
pie(c2)

#Sales volume

for (i in 1:(ncol(df)-1))
{print(paste( names(df)[i],sum(df[,i])))}


#Some plot regarding the two types of costumers
itemFrequencyPlot(new2, type= c('absolute'),topN=20, xlab='Item', names=TRUE)
itemFrequencyPlot(new3, type= c('absolute'),topN=20, xlab='Item', names=TRUE)
itemFrequencyPlot(new4, type= c('absolute'),topN=20, xlab='Item', names=TRUE)

