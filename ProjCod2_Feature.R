# Data Mining Project part2

# Conversion of Top 100 words to features by adding synonyms of each word 
#--------------------------------------------------------------------------

source("ProjectCode_spamword.R")

# List of Libraries Used
library(quanteda)
library(wordnet)

# The top 100 features
top.100.spam.words

# calculating the synmonyms for each word
word <- ""
for(i in 1:100){
  word <- c(word, list(c(synonyms(top.100.spam.words[i], "VERB"), synonyms(top.100.spam.words[i], "NOUN"), top.100.spam.words[i])))
}
# Creating a dictionary
myDict <- dictionary(list(feature1=word[[2]], feature2=word[[3]], feature3=word[[4]], feature4=word[[5]], feature5=word[[6]],
                          feature6=word[[7]], feature7=word[[8]], feature8=word[[9]], feature9=word[[10]], feature10=word[[11]],
                          feature11=word[[12]], feature12=word[[13]], feature13=word[[14]], feature14=word[[15]], feature15=word[[16]],
                          feature16=word[[17]], feature17=word[[18]], feature18=word[[19]], feature19=word[[20]], feature20=word[[21]],
                          feature21=word[[22]], feature22=word[[23]], feature23=word[[24]], feature24=word[[25]], feature25=word[[26]],
                          feature26=word[[27]], feature27=word[[28]], feature28=word[[29]], feature29=word[[30]], feature30=word[[31]],
                          feature31=word[[32]], feature32=word[[33]], feature33=word[[34]], feature34=word[[35]], feature35=word[[36]],
                          feature36=word[[37]], feature37=word[[38]], feature38=word[[39]], feature39=word[[40]], feature40=word[[41]],
                          feature41=word[[42]], feature42=word[[43]], feature43=word[[44]], feature44=word[[45]], feature45=word[[46]],
                          feature46=word[[47]], feature47=word[[48]], feature48=word[[49]], feature49=word[[50]], feature50=word[[51]],
                          feature51=word[[52]], feature52=word[[53]], feature53=word[[54]], feature54=word[[55]], feature55=word[[56]],
                          feature56=word[[57]], feature57=word[[58]], feature58=word[[59]], feature59=word[[60]], feature60=word[[61]],
                          feature61=word[[62]], feature62=word[[63]], feature63=word[[64]], feature64=word[[65]], feature65=word[[66]],
                          feature66=word[[67]], feature67=word[[68]], feature68=word[[69]], feature69=word[[70]], feature70=word[[71]],
                          feature71=word[[72]], feature72=word[[73]], feature73=word[[74]], feature74=word[[75]], feature75=word[[76]],
                          feature76=word[[77]], feature77=word[[78]], feature78=word[[79]], feature79=word[[80]], feature80=word[[81]],
                          feature81=word[[82]], feature82=word[[83]], feature83=word[[84]], feature84=word[[85]], feature85=word[[86]],
                          feature86=word[[87]], feature87=word[[88]], feature88=word[[89]], feature89=word[[90]], feature90=word[[91]],
                          feature91=word[[92]], feature92=word[[93]], feature93=word[[94]], feature94=word[[95]], feature95=word[[96]],
                          feature96=word[[97]], feature97=word[[98]], feature98=word[[99]], feature99=word[[100]], feature100=word[[101]]
                          ))

#--------------------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------------------

