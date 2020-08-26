Sys.setlocale("LC_CTYPE", "ru_RU") #to be able to read this code with Cyrillic 

setwd("") #folder with database

pb <- read.csv("pb_all.csv", header=F)

head(pb) #check the original file structure

number_of_q    <- pb[1,]
name_of_column <- pb[2,]
name_of_column[4]
number_of_q 

head(pb)

#delete 1-3 row
pb <- pb[-(1:3),]
head(pb)

#------- #analysis
#----- #sampling
  
dim(pb)
names(pb)
number_of_q

table(pb$V16)

pb_yes <- pb[pb$V16=="Так",] #we need respondents who answer "yes" for "Do you have experience in voting for Kyiv participatory budgeting"
dim(pb_yes) #total 196 valid answers = this is our final sample

#---- #creating two samples: voters and teams (project leaders + team members), as only project leaders were not enough, so we marge these respondents
  
table(pb_yes$V18) #question about identification the role in the last participatory budgeting voting

pb_yes$type <- NA
pb_yes$type[pb_yes$V18=="Я виборець, тільки голосував/ла за проекти"] <- "voter" #I'm a voter, I've been only voting for projects
pb_yes$type[pb_yes$V18=="Я лідер/ка проекту"] <- "team" #I'm a leader of the project
pb_yes$type[pb_yes$V18=="Я учасник/ця команди проекту"] <- "team" #I'm a team member

table(pb_yes$V18,pb_yes$type)
table(pb_yes$type)  #team = 75 (55 team members + 20 project leaders) and voters = 116


#Decriptive stat
pb_yes

table(pb_yes$V60)
round(prop.table(table(pb_yes$V60)),2)*100

table(pb_yes$V61)
round(prop.table(table(pb_yes$V61)),2)*100

table(pb_yes$V62)
round(prop.table(table(pb_yes$V62)),2)*100

table(pb_yes$V63)
round(prop.table(table(pb_yes$V63)),2)*100

table(pb_yes$V64)
round(prop.table(table(pb_yes$V64)),2)*100


#---- #checking the strategy on trust to stanger/social trust between team and voters
  
number_of_q
table(pb_yes$V26) #16 missing values

pb_yes$trust_social <- NA
pb_yes$trust_social[pb_yes$V26=="У більшості випадків ви повинні бути обережні з іншими людьми, людям не можна повністю довіряти"] <- 1
pb_yes$trust_social[pb_yes$V26=="З більшістю людей ви повинні бути обережними"] <- 2
pb_yes$trust_social[pb_yes$V26=="Людям, як правило, можна довіряти"] <- 3
pb_yes$trust_social[pb_yes$V26=="Людям майже завжди можна довіряти"] <- 3

table(pb_yes$trust_social)

table(pb_yes$type,pb_yes$trust_social)


prop.table(table(pb_yes$type,pb_yes$trust_social),1)*100


chisq.test(table(pb_yes$type,pb_yes$trust_social)) #p-value = 0.00304 

#Team members trust to random people more

##checking the strategy on trust to familiar people/family
  
number_of_q
table(pb_yes$V30)

pb_yes$trust_family <- NA
pb_yes$trust_family[pb_yes$V30=="Зовсім не довіряю"] <- 1
pb_yes$trust_family[pb_yes$V30=="Скоріше не довіряю"] <- 1
pb_yes$trust_family[pb_yes$V30=="Скоріше довіряю"] <- 1
pb_yes$trust_family[pb_yes$V30=="Повністю довіряю"] <- 2

table(pb_yes$trust_family)
table(pb_yes$type,pb_yes$trust_family)
prop.table(table(pb_yes$type,pb_yes$trust_family),1)*100

chisq.test(table(pb_yes$type,pb_yes$trust_family)) #p-value = 0.1218

#All respondents love families


##trust to different social institutions
  
number_of_q
table(pb_yes$V31)

institutions <- pb_yes[,c(31:44)]
names(institutions)

institutions[institutions=="Зовсім не довіряю"] <- 1
institutions[institutions=="Скоріше не довіряю"] <- 2
institutions[institutions=="Важко відповісти чи довіряю, чи ні"] <- 3
institutions[institutions=="Скоріше довіряю"] <- 4
institutions[institutions=="Повністю довіряю"] <- 5

class(institutions)
str(institutions)

institutions <- as.data.frame(apply(institutions,2,as.numeric))

name_of_column

nms <- c("President", "VRY", "KabMin", "Police", "Courts", "AFU", "Nat_party", "Loc_party", "Loc_Kyiv", "Loc_dep", "Loc_selfgov", "NGO", "Nat_media", "Loc_media")
colnames(institutions) <- nms

colnames(institutions)

#bloxpot to check differences in trust between voters and team
dim(institutions)
for(i in 1:14){
  boxplot(institutions[,i]~pb_yes$type, ylab = colnames(institutions)[i])
}

rslt <- NA
for(i in 1:14){
  rslt <- t.test(institutions[,i]~pb_yes$type)
  print(rslt$p.value) #8 and 10 are significant 0.05274413 and 0.034286
}

library(psych)
alpha(institutions) #0.86 = overall reliability of the scale is high

## check only political institutions on national, local and governmental levels

nats <- institutions[,c(1:7,12,13)] 
locs <- institutions[,-c(1:7,12,13)] #only Loc_party Loc_Kyiv Loc_dep Loc_selfgov
govs <- institutions[,-c(12:14)] #no NGO Nat_media Loc_media
dim(nats)
dim(locs)
dim(govs)

alpha(nats) #0.78
alpha(locs) #0.79
alpha(govs) #0.86

govs_mean <- apply(govs,1,mean,na.rm=T)
nats_mean <- apply(nats,1,mean,na.rm=T)
locs_mean <- apply(locs,1,mean,na.rm=T)

summary(govs_mean)
summary(nats_mean)
summary(locs_mean)

hist(govs_mean)
hist(nats_mean)
hist(locs_mean)

boxplot(govs_mean)
boxplot(nats_mean)
boxplot(locs_mean)



##adding new variables
  
#teammember <-  ifelse(pb_yes$type == "team", 1, 0)
women <-  ifelse(pb_yes$V60 == "Жінка", 1, 0)
eduhigh <-  ifelse(pb_yes$V64 == "Вища", 1, 0)
kyivcit <- ifelse(pb_yes$V62 == "Я постійно проживаю в Києві", 1, 0)
enterpr_occ <- ifelse (pb_yes$V63 == "Приватний підприємець/самозайнята особа", 1, 0)
age1825 <- ifelse(pb_yes$V61 == "18-25 років", 1, 0)
age1835 <- ifelse(pb_yes$V61 == "26-35 років", 1, 0)
ageg    <- pb_yes$V61 


#add membership in organizations

table(pb_yes$V46) #20 missing
grep("ні, я не є членом жодного з громадських об'єднань", pb_yes$V46)
nomember <- grep("ні, я не є членом жодного з громадських об'єднань", pb_yes$V46)

pb_yes$V46[4]
pb_yes$V46[nomember]

aremembers <- pb_yes$V46
aremembers[nomember] <- "nonmember"


aremembers <- ifelse(aremembers == "nonmember", 0, 1) #nonmember=0, member=1


##regression: activism to local parties + soc-dem
  
model1 <- lm(institutions$Loc_party~pb_yes$type)
summary(model1)

#add soc-dem
model2 <- lm(institutions$Loc_party~pb_yes$type + women + eduhigh + kyivcit + enterpr_occ + age1825)
summary (model2)


model3 <- lm(institutions$Loc_party~pb_yes$trust_social) #approve #2
summary (model3)

model178 <- lm(institutions$Loc_party~aremembers)
summary(model178)


#--- #check with trust from concept
pb_yes$V27 #Як би Ви охарактеризували ваш рівень довіри до процесу відбору та оцінки проєктів ГБ зі сторони міської влади?
table(pb_yes$V27)
pb_yes$V28 #Q17 - Як би Ви охарактеризували ваш рівень довіри до Громадської бюджетної комісії?
pb_yes$V29 #Q18 - Як би Ви охарактеризували ваш рівень довіри до онлайн системи голосування за проєкти?

model4 <- lm(institutions$Loc_party~pb_yes$type+pb_yes$V27 + pb_yes$V28 + pb_yes$V29)
summary (model4)

pb_yes$V45 #Q20 - На вашу думку, Ви активно залучені до громадської діяльності?
pb_yes$V47 #Q22 - Чи займались Ви волонтерською діяльністю (безоплатною роботою задля суспільних/громадських потреб) протягом останнього року?
pb_yes$V48 #Q23 - Чи надавали Ви протягом останнього року якусь благодійну грошову або матеріальну допомогу (наприклад, одяг чи їжу) людям чи громадським організаціям, які вирішують певні проблеми?
pb_yes$V49 #Q24 - Чи є у вас досвід у локальній політичній або громадській діяльності у Києві протягом останнього року?

volunteer <-  ifelse(pb_yes$V47 == "Так", 1, 0)
charity <-  ifelse(pb_yes$V48 == "Так", 1, 0)


model5 <- lm(institutions$Loc_party~pb_yes$type+pb_yes$V45+volunteer+charity+pb_yes$V49)
summary (model5)

pb_yes$V50 #Q25 - Якою мірою Ви відчуваєте свою відповідальність за те, що відбувається в Києві?

model6 <- lm(institutions$Loc_party~pb_yes$type+pb_yes$V49+pb_yes$V50)
summary (model6)

pb_yes$V19 #Q6 - За скільки проєктів громадського бюджету Ви голосували востаннє?
pb_yes$V20 #Q7 - На момент голосування, чи знали Ви особисто когось із членів команди проектів (за які голосували)?

model7 <- lm(institutions$Loc_party~pb_yes$type+pb_yes$V19 +pb_yes$V20)
summary (model7)

pb_yes$V22 #Q9 - Чи просили Ви когось голосувати за проєкти ГБ?
pb_yes$V23 #Q10 - Чи брали участь в публічних онлайн дискусіях щодо будь-яких проєктів ГБ?
pb_yes$V24 #Q11 - Чи є Ви учасником/цею Фейсбук-групи Київського ГБ?
pb_yes$V25 #Q12 - Чи Ви спеціально слідкуєте за реалізацію проєктів, за які голосували?

model8 <- lm(institutions$Loc_party~pb_yes$type+pb_yes$V22+ pb_yes$V23+pb_yes$V24+pb_yes$V25)
summary (model8)

model99 <- lm(institutions$Loc_party~pb_yes$type+pb_yes$V24)
summary (model99)

#MODELS WITH Loc_dep

model61 <- lm(institutions$Loc_dep~pb_yes$type+women + eduhigh + kyivcit + enterpr_occ + age1825 + aremembers)
summary (model61)

#MODELS WITH pb_yes$trust_social

model89 <-lm(pb_yes$trust_social~pb_yes$type+pb_yes$V20)
summary (model89)

############### Evaluation of concept models
### #E development
#V52 - dopomogaju gromadi
#V53 - rozumiu derj upravlinna
#V56 - slidkuju zs KMDA
#V51 - otsinit rol grom aktyvistok

table(pb_yes$V52)
table(pb_yes$V53)
table(pb_yes$V56)
table(pb_yes$V51)
#
edevel <- data.frame(v52=pb_yes$V52,
                     v53=pb_yes$V53,
                     v56=pb_yes$V56)
#
edevel[edevel=="Зовсім не погоджуюсь"] <- 1
edevel[edevel=="Скоріше не погоджуюсь"] <- 2
edevel[edevel=="Важко відповісти"] <- 3
edevel[edevel=="Скоріше погоджуюсь"] <- 4
edevel[edevel=="Повністю погоджуюсь"] <- 5

class(edevel)
str(edevel)

edevel$v51 <- pb_yes$V51

edevel <- as.data.frame(apply(edevel,2,as.numeric))
str(edevel)

library(psych)
alpha(edevel) #0.62 

####Gove trans
#Q27_6 27_7 27_4 27_3 27_2
#V57, V58, V55, V54, V53

govtrans <- pb_yes[,c(57,58,55,54)]


govtrans[govtrans=="Зовсім не погоджуюсь"] <- 1
govtrans[govtrans=="Скоріше не погоджуюсь"] <- 2
govtrans[govtrans=="Важко відповісти"] <- 3
govtrans[govtrans=="Скоріше погоджуюсь"] <- 4
govtrans[govtrans=="Повністю погоджуюсь"] <- 5


govtrans <- as.data.frame(apply(govtrans,2,as.numeric))
str(govtrans)
alpha(govtrans) #0.73

govtrans_mean <- apply(govtrans,1,mean, na.rm=T)

modelKLUA <- lm(institutions$Loc_party~govtrans_mean)
summary(modelKLUA) 

modelGOVTRANSTS <- lm(govtrans_mean~pb_yes$trust_social)
summary(modelGOVTRANSTS)

modelGOVTRANSVOT <- lm(govtrans_mean~pb_yes$type)
summary(modelGOVTRANSVOT) #unvalid p-value: 0.4484



modelKLUA22 <-  lm(institutions$Loc_party~govtrans_mean + women + eduhigh + kyivcit + enterpr_occ + age1825)
summary(modelKLUA22)

modelKLUA2 <-  lm(institutions$Loc_party~govtrans_mean + women + eduhigh + kyivcit + enterpr_occ + age1825 + aremembers)
summary(modelKLUA2) #Adjusted R-squared:  0.07969 higher with soc-dem

modelKLUA23 <-  lm(institutions$Loc_party~govtrans_mean + women + eduhigh + kyivcit + enterpr_occ + age1825 + pb_yes$type)
summary(modelKLUA23)

#####SAT with e-part
#Q14,17,18
#V27, 28, 29

epart <- pb_yes[,c(27:29)]

epart[epart=="Зовсім не довіряю"] <- 1
epart[epart=="Скоріше не довіряю"] <- 2
epart[epart=="Важко відповісти чи довіряю, чи ні"] <- 3
epart[epart=="Скоріше довіряю"] <- 4
epart[epart=="Повністю довіряю"] <- 5


epart <- as.data.frame(apply(epart,2,as.numeric))
str(epart)

alpha(epart)  #061  #Satisfaction with e-participation is not a construct


### new index - activism

#Q6, 9, 10, 20, 21, 22, 23, 24
#V19, V22, V45, (V46), V47-V49
number_of_q
activism <- pb_yes[,c(19,22,45,47,48,49)]
head(activism)

table(activism$V19)
table(activism$V22)
table(activism$V45)
table(activism$V47)
table(activism$V49)
table(activism$V48)


activism$type1[pb_yes$V19=="Я не пам'ятаю"] <- "Ні" 
activism$type1[pb_yes$V19=="1"] <- "Ні" 
activism$type1[pb_yes$V19=="2"] <- "Ні" 
activism$type1[pb_yes$V19=="3"] <- "Ні" 
activism$type1[pb_yes$V19=="4"] <- "Ні" 
activism$type1[pb_yes$V19=="5"] <- "Так" 

table(pb_yes$V19,activism$type1)

#
activism$type2 <- activism$V22

#
activism$type3 <- NA
activism$type3[pb_yes$V45=="Так"] <- "Так" 
activism$type3[pb_yes$V45=="Ні"] <- "Ні" 
activism$type3[pb_yes$V45=="Складно сказати"] <- "Ні" 

table(pb_yes$V45,activism$type3)

#
activism$type4 <- NA
activism$type4[pb_yes$V47=="Так"] <- "Так" 
activism$type4[pb_yes$V47=="Ні"] <- "Ні" 
activism$type4[pb_yes$V47=="Складно сказати"] <- "Ні" 

table(pb_yes$V47,activism$type4)

#
table(pb_yes$V48)
activism$type5 <- NA
activism$type5[pb_yes$V48=="Так"] <- "Так" 
activism$type5[pb_yes$V48=="Ні"] <- "Ні" 
activism$type5[pb_yes$V48=="Складно сказати"] <- "Ні" 

table(pb_yes$V48,activism$type5)

#
activism$type6[pb_yes$V49=="Складно сказати/не знаю"] <- "Ні" 
activism$type6[pb_yes$V49=="У мене немає такого досвіду"] <- "Ні" 
activism$type6[pb_yes$V49=="Маю малий досвід"] <- "Так" 
activism$type6[pb_yes$V49=="У мене є значний досвід"] <- "Так" 
activism$type6[pb_yes$V49=="Я повністю залучений/а до політичній/громадській діяльності міста"] <- "Так" 

table(pb_yes$V49,activism$type6)
#
head(activism)
activism_f <- activism[,c(7:12)]
head(activism_f)

activism_f[activism_f=="Так"] <- 1
activism_f[activism_f=="Ні"] <- 0

activism_f <- as.data.frame(apply(activism_f,2,as.numeric))
str(activism_f)


alpha(activism_f) #0.71 

names(activism_f)
activism_mean <- apply(activism_f,1,mean, na.rm=T)
hist(activism_mean)


table(activism_f,pb_yes$type)

###activism model _ regression analysis
modelACTGOV <- lm(govs_mean~activism_mean)
summary(modelACTGOV)

modelACTGOVSD <- lm(govs_mean~activism_mean + women + eduhigh + kyivcit + enterpr_occ + age1825)
summary(modelACTGOVSD)


modelACTLOC <- lm(institutions$Loc_party~activism_mean)
summary(modelACTLOC)

modelACTLOCSD <- lm(institutions$Loc_party~activism_mean + women + eduhigh + kyivcit + enterpr_occ + age1825)
summary(modelACTLOCSD)


modelACTT <- lm(pb_yes$trust_social~activism_mean)
summary(modelACTT)

modelACTTSD <- lm(pb_yes$trust_social~activism_mean + women + eduhigh + kyivcit + enterpr_occ + age1825)
summary(modelACTTSD)

##crosstabulation

table(pb_yes$V60,pb_yes$type)
table(pb_yes$V64,pb_yes$type)
table(pb_yes$V24,pb_yes$type) #Facebook membership


model998 <- lm(pb_yes$trust_social~pb_yes$type+pb_yes$V24)
summary(model998) #valid

model998A <- lm(pb_yes$trust_social~pb_yes$V24)
summary(model998A) #valid

model998B <- lm(pb_yes$trust_social~pb_yes$V24 + pb_yes$type)
summary(model998B) #valid

model998D <- lm(institutions$Loc_party~pb_yes$V24)
summary(model998D) 

model9998 <- lm(institutions$Loc_party~pb_yes$type+pb_yes$V24)
summary(model9998)
