install.packages("ineq")
library(ineq)

#Ich nehme im Folgenden die ineq-Gini-Formel. Es besteht dort keine Bedingung, dass es insgesamt 100 sein müssen o.ä.

#Treatment Group
x  <- c(20, 20, 20, 20, 20) # Das ist mein Input!
x1 <- c(25, 20, 15, 21, 19) 
x2 <- c(20, 20, 20, 22, 18) 
x3 <- c(20, 20, 20, 23, 17) 
x4 <- c(20, 20, 20, 24, 16) 
x5 <- c(20, 20, 20, 25, 15) 



x.1 <- ineq((x), type="Gini") #Hiermit rechne ich den Gini coefficient aus!
print(x.1)

x1.1 <- ineq((x1), type="Gini") 
print(x1.1)

x2.1 <- ineq((x2), type="Gini") #Hiermit rechne ich ne Gini coefficient aus!+
print(x2.1)

x3.1 <- ineq((x3), type="Gini") #Hiermit rechne ich ne Gini coefficient aus!+
print(x3.1)


x4.1 <- ineq((x4), type="Gini") #Hiermit rechne ich ne Gini coefficient aus!+
print(x4.1)

x5.1 <- ineq((x5), type="Gini") #Hiermit rechne ich ne Gini coefficient aus!+
print(x5.1)






#Control Group
y  <- c(80, 10, 6, 4, 0) # Das ist mein Input!
y1 <- c(70, 20, 7, 3, 0) 
y2 <- c(60, 20, 10, 8, 2) 
y3 <- c(50, 30, 10, 8, 2) 
y4 <- c(50, 25, 15, 7, 3) 
y5 <- c(50, 24, 16, 7, 3) 


y.1 <- ineq((y), type="Gini") #Hiermit rechne ich ne Gini coefficient aus!
print(y.1)

y1.1 <- ineq((y1), type="Gini") 
print(y1.1)

y2.1 <- ineq((y2), type="Gini") 
print(y2.1)

y3.1 <- ineq((y3), type="Gini") 
print(y3.1)


y4.1 <- ineq((y4), type="Gini") 
print(y4.1)

y5.1 <- ineq((y5), type="Gini") 
print(y5.1)





#ANOVA
# http://www.sthda.com/english/wiki/one-way-anova-test-in-r#what-is-one-way-anova-test

cdata <- c(y.1, y1.1, y2.1, y3.1, y4.1, y5.1)
cname <- c ("C","C","C","C","C","C")

tdata <- c(x.1, x1.1, x2.1, x3.1, x4.1, x5.1)
tname <- c("T","T","T","T","T","T")

# Ich muss programmieren, dass die y-Werte der Kontrollgruppe zugeordnet und die x-Werte der Treatmentgruppe zugeordnet werden. 
#Dataframe? Matrix?  1:(Gruppe, Wert), 2:(Gruppe, Wert)

data.frame_c <- data.frame(gini = cdata, group = cname)
data.frame_t <- data.frame(gini = tdata, group = tname)

data.frame_all <- rbind(data.frame_c, data.frame_t)

print(data.frame_all)  # so konnte ich einen DataFrame mit 12 Zeilen erstellen. Jetzt müsste ich die ANOVA anhand der Gruppen durchführen können


boxplot(gini ~ group, data = data.frame_all,
        xlab = "Group", ylab = "Gini Coefficient",
        frame = FALSE)




# Compute the analysis of variance
aov <- aov(gini ~ group, data = data.frame_all)
# Summary of the analysis
summary(aov)


#TO
# rechnet mir R hier die richtigen Gini Werte aus?
# stimmt alles bei der ANOVA?




#Preferences for redistributive policies
#Questions:

#a) Differences in income in <country> are too large. 				

#b) It is the responsibility of the government to reduce the differences 
#in income between people with high incomes and those with low incomes.

#c) The government should provide a decent standard of living for the unemployed.

#d) The government should spend less on benefits for the poor. (INVERTIERT)


#5-Point-Likert Scale 

#1 Strongly Agree
#2 Agree
#3 Neutral
#4 Disagree
#5 Strongly Disagree

# -> Je näher der Durchschnittswert an 1 rückt, desto positiver ist die Einstellung des Probanden
#    hinsichtlich Umverteilungsmaßnahmen zu betrachten.


# Es gibt pro Proban 4 Werte (a, b, c, d). Von denen ist der Wert d invertiert.
# d.h aus einer 1 muss eine 5 werden.
# 5 -> 1
# 4 -> 2
# 3 -> 3
# 2 -> 4
# 1 -> 5         6-x= verwendeter Wert

#Treatment Group
alex <- c(5,5,5,1)
damian <- c(4,4,4,2)
luci <- c(5,3,4,1)
lorenz <- c(4,5,5,2)
nick <- c(5,3,4,1)
brudi <- c(4,4,5,1)


tdata_frame <- data.frame(alex, damian, luci, lorenz, nick, brudi)

tdata_frame <- t(tdata_frame)

colnames(tdata_frame) <-col_name

print(tdata_frame)

#Neue Spalte hinzufügen

tdata_frame <- cbind(tdata_frame, group = c("T","T","T","T","T","T"))


#TO DO 
#Daten zu Frage d invertieren!

#tdata_frame[1:6,4] -> 6- tdata_frame[1:6,4]

#tdata_frame$d -> 6 - tdata_frame$d

df <-as.data.frame(tdata_frame)

is.data.frame(df)

df$d <- c(1,2,1,2,1,1)

df$d <- 6 - df$d



#Cotrol Group
agnes <- c(1,1,1,5)
patrick <- c(2,2,2,4)
antonia <- c(1,2,1,4)
doris <- c(2,1,1,5)
desire <- c(1,2,3,5)
peter <- c(2,1,2,4)

cnamep <- c("C","C","C","C","C")

dfc <- rbind(agnes, patrick, antonia, doris, desire, peter)

colnames(dfc) <- c("a","b","c","d")

is.data.frame(dfc)

dfc <- as.data.frame(dfc)

print(dfc)

dfc <- cbind(dfc, group = c("C","C","C","C","C","C"))

dfc$d <- 6 - dfc$d

typeof(dfc[5:3])



#Beide DataFrames verbinden

df_all <- rbind(dfc, df)


#Durchschnittswerte bilden

class(df_all[1,1])

as.numeric(df_all[0, 0])

df_all_ohneGruppe <- df_all[,-5]

df_all_ohneGruppe <- lapply(df_all_ohneGruppe,as.numeric)  #so wandelt man alles in numeric um!!! 

mean(df_all_ohneGruppe)

#TO DO
# alle Zahlen in integer umwandeln!!! Sonst läuft der Laden nicht.








#ANOVA durchführen
# Compute the analysis of variance
#aov2 <- aov(gini ~ group, data = df_all)
# Summary of the analysis
#summary(aov2)

