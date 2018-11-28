setwd("C:/Users/Elena/Desktop/Git/Nuova cartella/task2-4-basket-analysis-ElenaBonan")
library(arules)
library(arulesViz)
library(readr)
library(stringr)
library(rlist)
file = read.transactions('ElectronidexTransactions2017.csv', format= 'basket', sep=',', rm.duplicates = TRUE)
#file = read.transactions('ElectronidexTransactions2017.csv', sep=',', rm.duplicates = FALSE)
variable.names(file)
frequency(file)

str(file)

inspect(file[1])
inspect(duplicates(file))
str(file)
#inspect(file)
str(file)
length(file)#number of transactions
size(file)
LIST(file)
itemLabels(file)#Name of the items

#itemFrequencyPlot(file, topN=5) 
c = itemFrequency(file, type='absolute')
image(file)
set.seed(123)
image(sample(file,1000))

#Apply the algorithm 

Ruleapriori<- apriori (file, parameter = list(supp = 0.01, conf = 0.50))
ins <- inspect(Ruleapriori)
plot(Ruleapriori)
#Ruleapriori<- apriori (file, parameter = list(supp = 0.05, conf = 0.30))
#ins <- inspect(Ruleapriori)

Ruleapriori<- apriori (file, parameter = list(supp = 0.001, conf = 1))
ins <- inspect(Ruleapriori)
plot(Ruleapriori)

#Things to do 
# see the duplicates
# attach the type of products
# take a subset

#Try to create levels in a data frame

##----------------------
MyData <- read.csv(file="ElectronidexTransactions2017.csv",header=FALSE, sep=",")



attach(MyData)
detach(mydata)


# Try to create levels in a basket format


variable.names(file)[which(str_detect(variable.names(file),"Laptop")| 
                             str_detect(variable.names(file),fixed('book', ignore_case=TRUE)) | 
                             str_detect(variable.names(file),fixed('Acer Aspire', ignore_case=TRUE)))]



#The other option is to take the name from the exel table


#create the levels
file@itemInfo$Type = file@itemInfo$labels 



file@itemInfo$labels

laptops = file@itemInfo$labels[which(
         str_detect(variable.names(file),fixed('Laptops|book', ignore_case=TRUE)) | 
         str_detect(variable.names(file),fixed('Acer Aspire', ignore_case=TRUE)))]
laptops

Type = list()
for (i in file@itemInfo$labels)
{ if ( str_detect(i, fixed('Laptop', ignore_case=TRUE)) | str_detect(i, fixed('book', ignore_case=TRUE))| str_detect(i,fixed('book', ignore_case=TRUE))
    
  
  ) {Type = list.append(Type, 'Laptop')}

  else { Type = list.append(Type,'Nonclass')}}


f =file@itemInfo$labels
Type




str(file)
a = 'ciao'
str_detect(a,'c')



# Add the type of product


Type = c()

for (i in file@itemInfo$labels)
{ if ( str_detect(i, fixed('Laptop', ignore_case=TRUE)) 
       | str_detect(i, fixed('book', ignore_case=TRUE))
       | str_detect(i,fixed('Acer Aspire', ignore_case=TRUE))
) {Type = c(Type,'Laptop')}
  
  if ( str_detect(i, fixed('Desktop', ignore_case=TRUE)) 
       | str_detect(i, fixed('iMac', ignore_case=TRUE))
       
  ) {Type = c(Type,'Desktop')}  
  
  if ( str_detect(i, fixed('Keyboard', ignore_case=TRUE)) 
      
       
  ) {Type = c(Type,'Keyboard')}  
  
  if ( str_detect(i, fixed('Monitor', ignore_case=TRUE)) 
       
       
  ) {Type = c(Type,'Monitor')}  
  
  if ( str_detect(i, fixed('Mouse', ignore_case=TRUE)) 
       | str_detect(i, fixed('3-Button', ignore_case=TRUE))
       
  ) {Type = c(Type,'Mouse')}  
  
  if ( str_detect(i, fixed('Mouse', ignore_case=TRUE)) 
       & str_detect(i, fixed('Keyboard', ignore_case=TRUE))
       
  ) {Type = c(Type,'Mouse and Keyboard Combo')} 
  
  if ( str_detect(i, fixed( 'Dell KM117 Wireless Keyboard', ignore_case=TRUE)) 
       
       
  ) {Type = c(Type,'Mouse and Keyboard Combo')} 
  
  if ( i %in% 
  c('Microsoft Wireless Comfort Keyboard and Mouse',
  'Microsoft Wireless Desktop Keyboard and Mouse'))
  {Type = c(Type,'Mouse and Keyboard Combo')
  print(i)
  }
 
  
  
  if ( str_detect(i, fixed('Headset', ignore_case=TRUE)) 
       | str_detect(i, fixed('Headphones', ignore_case=TRUE))
       
  ) {Type = c(Type,'Headphones')} 
  
  
  
  if ( str_detect(i, fixed('Cable', ignore_case=TRUE)) 
       | str_detect(i, fixed('HDMI Adapter', ignore_case=TRUE))
       
  ) {Type = c(Type,'Computer Cords')} 
  
  if ( str_detect(i, fixed('Hard Drive', ignore_case=TRUE)) 
       
       
  ) {Type = c(Type,'Hard Drive')} 
  
  if ( str_detect(i, fixed('Speaker', ignore_case=TRUE)) 
       | str_detect(i, fixed('DOSS Touch Wireless Bluetooth', ignore_case=TRUE))
       | str_detect(i, fixed('Cyber Acoustics', ignore_case=TRUE))
       
  ) {Type = c(Type,'Speaker')} 
  
  
  if( i %in%
      c( 
         'Epson Printer',
         'HP Wireless Printer',
         'Canon Office Printer',
         'Brother Printer',
         'DYMO Label Manker'
        
        
      ))
  { Type = c(Type,'Printer') }
  
  
  if( i %in%
  c('Apple TV',
  'Google Home',
  'Smart Light Bulb',
  'Fire TV Stick',
  'Roku Express'))
  { Type = c(Type,'Smart Home Device') 
  }
  
  
  if( i %in%
      c('Apple Earpods',
        'Monster Beats By Dr Dre',
        'Otium Wireless Sports Bluetooth Headphones',
        'Panasonic In-Ear Headphone',
        'APIE Bluetooth Headphones',
        'Philips Flexible Earhook Headphones')){ Type = c(Type, 'Active Headphones')}
  
 if (i %in%  
  c('Accessories
  Microsoft Office Home and Student 2016',
  'Computer Game',
  'Belkin Mouse Pad',
  'Large Mouse Pad')){Type = c(Type, 'Accessories')}
  
  if (i %in% 
  c('Printer Ink',
  'Epson Black Ink',
  'HP Black & Tri-color Ink',
  'Canon Ink',
  'Brother Printer Toner',
  'DYMO Labeling Tape'))
  {Type = c(Type, 'Accessories')}
  
  
  
  else 
    
  { Type = c(Type,'Nonclass')}
  }


Type



pt = read.csv(file="ProductType4.csv", sep=',')

#More intelligent way 


Type

file@itemInfo$Type = Type

str(Type)

Type = as.character(Type)
Type


l = list('ciao')
if ('ciao ' in l ) 
  {print(ciao)}

###--------------------
# Table in excel
newLabels<- c("ExternalHD-1TB Portable External Hard Drive","ExternalHD-2TB Portable External Hard Drive","Computer Mice-3Button Mouse","ExternalHD-3TB Portable External Hard Drive","ExternalHD-5TB Desktop Hard Drive","Monitors-AOC Monitor","ActiveHeadphones-APIE Bluetooth Headphone","Monitors-ASUS 2 Monitor","Laptops-ASUS Chromebook","Desktops-ASUS Desktop",               "Monitors-ASUS Monitor","Laptops-Acer Aspire","Desktops-Acer Desktop","Monitors-Acer Monitor","ComputerHeadphones-Ailihen Stereo Headphones","Laptops-Alienware Laptop","ActiveHeadphones-Apple Earpods",               "Laptops-Apple MacBook Air","Laptops-Apple MacBook Pro","Keyboards-Apple Magic Keyboard","SmartHomeDevices-Apple TV","Keyboards-Apple Wired Keyboard","Keyboards-Apple Wireless Keyboard","ComputerCords-Audio Cable",               "Keyboards-Backlit LED Gaming Keyboard","Accessories-Belkin Mouse Pad","Speakers-Bose Companion Speaker","Printers-Brother Printer","Printers-Brother Printer Toner","Desktops-CYBERPOWER Gamer Desktop","Speakers-Cambridge Bluetooth Speaker",               "Cartridge-Canon Ink","Printers-Canon Office Printer","Software-Computer Game","Speakers-Cyber Acoustics","Speakers-DOSS Touch Wireless Bluetooth","Printers-DYMO Label Manker","Cartridge-DYMO Labeling Tape","Desktops-Dell 2 Desktop","Desktops-Dell Desktop",               "MKCombo-Dell KM117 Wireless Keyboard & Mouse","Laptops-Dell Laptop","Monitors-Dell Monitor","Keyboards-Dell Wired Keyboard","MKCombo-EagleTec Wireless Combo Keyboard and Mouse","Laptops-Eluktronics Pro Gaming Laptop","Cartridge-Epson Black Ink","Printers-Epson Printer",               "ComputerCords-Etekcity Power Extension Cord Cable","ComputerCords-Ethernet Cable","Tablets-Fire HD Tablet","SmartHomeDevices-Fire TV Stick","Stands-Full Motion Monitor Mount","Computer Mice-Gaming Mouse Professional","Computer Mice-Generic Black 3-Button","SmartHomeDevices-Google Home",               "ComputerCords-HDMI Adapter","ComputerCords-HDMI Cable 6ft","Cartridge-HP Black & Tri-color Ink","Desktops-HP Desktop","Laptops-HP Laptop","Monitors-HP Monitor","Laptops-HP Notebook Touchscreen Laptop PC","Keyboards-HP USB Keyboard","Computer Mice-HP Wireless Mouse","Printers-HP Wireless Printer",               "Stands-Halter Acrylic Monitor Stand","Stands-Halter Mesh Metal Monitor Stand","Stands-HeightAdjustable Standing Desk","Desktops-Intel Desktop","Speakers-JBL Splashproof Portable Bluetooth Speaker","ComputerHeadphones-Kensington Headphones","Tablets-Kindle","ComputerHeadphones-Koss Home Headphones",               "Monitors-LG Monitor","Laptops-LG Touchscreen Laptop","Accessories-Large Mouse Pad","Desktops-Lenovo Desktop Computer","Computer Mice-Logitech 3-button Mouse","ComputerHeadphones-Logitech ClearChat Headset","MKCombo-Logitech Desktop MK120 Mouse and keyboard Combo","Keyboards-Logitech Keyboard",               "MKCombo-Logitech MK270 Wireless Keyboard and Mouse Combo","MKCombo-Logitech MK360 Wireless Keyboard and Mouse Combo","MKCombo-Logitech MK550 Wireless Wave Keyboard and Mouse Combo","Speakers-Logitech Multimedia Speaker","ComputerHeadphones-Logitech Stereo Headset","Keyboards-Logitech Wireless Keyboard",               "Computer Mice-Logitech Wireless Mouse","Speakers-Mackie CR Speaker","Computer Mice-Microsoft Basic Optical Mouse","ComputerHeadphones-Microsoft Headset","Software-Microsoft Office Home and Student 2016","MKCombo-Microsoft Wireless Comfort Keyboard and Mouse","MKCombo-Microsoft Wireless Desktop Keyboard and Mouse",               "ActiveHeadphones-Monster Beats By Dr Dre","Stands-Multi Media Stand","ActiveHeadphones-Otium Wireless Sports Bluetooth Headphone","ComputerHeadphones-PC Gaming Headset","ActiveHeadphones-Panasonic In-Ear Headphone","ComputerHeadphones-Panasonic On-Ear Stereo Headphones","ActiveHeadphones-Philips Flexible Earhook Headphone",               "Computer Mice-Redragon Gaming Mouse","MKCombo-Rii LED Gaming Keyboard & Mouse Combo","Keyboards-Rii LE
D Keyboard","Speakers-Rokono Mini Speaker","SmartHomeDevices-Roku Express","ComputerCords-Samsung Charging Cable","Tablets-Samsung Galaxy Tablet",
              "Monitors-Samsung Monitor","Monitors-Sceptre Monitor",
              "ExternalHD-Slim 2TB Portable External Hard Drive",      
              "Computer Mice-Slim Wireless Mouse","SmartHomeDevices-Smart Light Bulb","Speakers-Sonos","ComputerCords-USB Cable",
              "ComputerCords-VGA Monitor Cable","Monitors-ViewSonic Monitor","Computer Mice-Wireless Portable Mouse","ComputerHeadphones-XIBERIA Gaming Headset","ComputerHeadphones-Zombie Gaming Headset","Desktops-iMac","Tablets-iPad","Tablets-iPadPro",       
              "ComputerCords-iPhone Charger Cable")

categories<-as.factor(sub("\\-.*", "", newLabels)) 
categories
file@itemInfo$Type = categories
str(file)
file@itemInfo$Type
list(inspect(file[1]))


for (i in inspect(file[1])){print(i)}


aggregate(file,  file@itemInfo$Type )

# Check the rules for product type 

file2 = aggregate(file,  file@itemInfo$Type )

Ruleapriori<- apriori (file2, parameter = list(supp = 0.10, conf = 0.60))
ins <- inspect(Ruleapriori)
plot(Ruleapriori)

c = itemFrequency(file2, type='absolute')
print(c)

sort(c, decreasing = TRUE)

#####################################

setwd("C:/Users/Elena/Desktop/Git/Nuova cartella/task2-4-basket-analysis-ElenaBonan")
library(arules)
library(arulesViz)
library(readr)
library(stringr)
library(rlist)
file <- read.transactions('ElectronidexTransactions2017.csv', format= 'basket', sep=',', rm.duplicates = TRUE)
summary(file)
head(file)
newLabels<- c("ExternalHD-1TB Portable External Hard Drive","ExternalHD-2TB Portable External Hard Drive","Computer Mice-3Button Mouse","ExternalHD-3TB Portable External Hard Drive","ExternalHD-5TB Desktop Hard Drive","Monitors-AOC Monitor","ActiveHeadphones-APIE Bluetooth Headphone","Monitors-ASUS 2 Monitor","Laptops-ASUS Chromebook","Desktops-ASUS Desktop",               "Monitors-ASUS Monitor","Laptops-Acer Aspire","Desktops-Acer Desktop","Monitors-Acer Monitor","ComputerHeadphones-Ailihen Stereo Headphones","Laptops-Alienware Laptop","ActiveHeadphones-Apple Earpods",               "Laptops-Apple MacBook Air","Laptops-Apple MacBook Pro","Keyboards-Apple Magic Keyboard","SmartHomeDevices-Apple TV","Keyboards-Apple Wired Keyboard","Keyboards-Apple Wireless Keyboard","ComputerCords-Audio Cable",               "Keyboards-Backlit LED Gaming Keyboard","Accessories-Belkin Mouse Pad","Speakers-Bose Companion Speaker","Printers-Brother Printer","Printers-Brother Printer Toner","Desktops-CYBERPOWER Gamer Desktop","Speakers-Cambridge Bluetooth Speaker",               "Cartridge-Canon Ink","Printers-Canon Office Printer","Software-Computer Game","Speakers-Cyber Acoustics","Speakers-DOSS Touch Wireless Bluetooth","Printers-DYMO Label Manker","Cartridge-DYMO Labeling Tape","Desktops-Dell 2 Desktop","Desktops-Dell Desktop",               "MKCombo-Dell KM117 Wireless Keyboard & Mouse","Laptops-Dell Laptop","Monitors-Dell Monitor","Keyboards-Dell Wired Keyboard","MKCombo-EagleTec Wireless Combo Keyboard and Mouse","Laptops-Eluktronics Pro Gaming Laptop","Cartridge-Epson Black Ink","Printers-Epson Printer",               "ComputerCords-Etekcity Power Extension Cord Cable","ComputerCords-Ethernet Cable","Tablets-Fire HD Tablet","SmartHomeDevices-Fire TV Stick","Stands-Full Motion Monitor Mount","Computer Mice-Gaming Mouse Professional","Computer Mice-Generic Black 3-Button","SmartHomeDevices-Google Home",               "ComputerCords-HDMI Adapter","ComputerCords-HDMI Cable 6ft","Cartridge-HP Black & Tri-color Ink","Desktops-HP Desktop","Laptops-HP Laptop","Monitors-HP Monitor","Laptops-HP Notebook Touchscreen Laptop PC","Keyboards-HP USB Keyboard","Computer Mice-HP Wireless Mouse","Printers-HP Wireless Printer",               "Stands-Halter Acrylic Monitor Stand","Stands-Halter Mesh Metal Monitor Stand","Stands-HeightAdjustable Standing Desk","Desktops-Intel Desktop","Speakers-JBL Splashproof Portable Bluetooth Speaker","ComputerHeadphones-Kensington Headphones","Tablets-Kindle","ComputerHeadphones-Koss Home Headphones",               "Monitors-LG Monitor","Laptops-LG Touchscreen Laptop","Accessories-Large Mouse Pad","Desktops-Lenovo Desktop Computer","Computer Mice-Logitech 3-button Mouse","ComputerHeadphones-Logitech ClearChat Headset","MKCombo-Logitech Desktop MK120 Mouse and keyboard Combo","Keyboards-Logitech Keyboard",               "MKCombo-Logitech MK270 Wireless Keyboard and Mouse Combo","MKCombo-Logitech MK360 Wireless Keyboard and Mouse Combo","MKCombo-Logitech MK550 Wireless Wave Keyboard and Mouse Combo","Speakers-Logitech Multimedia Speaker","ComputerHeadphones-Logitech Stereo Headset","Keyboards-Logitech Wireless Keyboard",               "Computer Mice-Logitech Wireless Mouse","Speakers-Mackie CR Speaker","Computer Mice-Microsoft Basic Optical Mouse","ComputerHeadphones-Microsoft Headset","Software-Microsoft Office Home and Student 2016","MKCombo-Microsoft Wireless Comfort Keyboard and Mouse","MKCombo-Microsoft Wireless Desktop Keyboard and Mouse",               "ActiveHeadphones-Monster Beats By Dr Dre","Stands-Multi Media Stand","ActiveHeadphones-Otium Wireless Sports Bluetooth Headphone","ComputerHeadphones-PC Gaming Headset","ActiveHeadphones-Panasonic In-Ear Headphone","ComputerHeadphones-Panasonic On-Ear Stereo Headphones","ActiveHeadphones-Philips Flexible Earhook Headphone",               "Computer Mice-Redragon Gaming Mouse","MKCombo-Rii LED Gaming Keyboard & Mouse Combo","Keyboards-Rii LE
D Keyboard","Speakers-Rokono Mini Speaker","SmartHomeDevices-Roku Express","ComputerCords-Samsung Charging Cable","Tablets-Samsung Galaxy Tablet",
              "Monitors-Samsung Monitor","Monitors-Sceptre Monitor",
              "ExternalHD-Slim 2TB Portable External Hard Drive",      
              "Computer Mice-Slim Wireless Mouse","SmartHomeDevices-Smart Light Bulb","Speakers-Sonos","ComputerCords-USB Cable",
              "ComputerCords-VGA Monitor Cable","Monitors-ViewSonic Monitor","Computer Mice-Wireless Portable Mouse","ComputerHeadphones-XIBERIA Gaming Headset","ComputerHeadphones-Zombie Gaming Headset","Desktops-iMac","Tablets-iPad","Tablets-iPadPro",       
              "ComputerCords-iPhone Charger Cable")

categories<-as.factor(sub("\\-.*", "", newLabels)) 
cat <- (sub("\\-.*", "", newLabels)) 
file@itemInfo$Type = categories
fileCat <- file
fileCat@itemInfo$labels <- cat


file2 = aggregate(file,  file@itemInfo$Type )

# Buisness index 


prova = as.data.frame(matrix(data))
dd  <-  as.data.frame(matrix(unlist(data), nrow=length(unlist(data[[1]]))))
#Prova for list

match(unlist(trans[[1]]), unlist(file@itemInfo$labels, use.names=FALSE))

v[match(unlist(trans[[1]]), unlist(file@itemInfo$labels, use.names=FALSE))]



