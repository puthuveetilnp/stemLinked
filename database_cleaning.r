library("tidyverse")
library(skimr)
coach<-read_excel("~/Downloads/fll-data.xlsx",sheet=2)
student<-read_excel("~/Downloads/fll-data.xlsx")
team<-read_excel("~/Downloads/fll-data.xlsx",sheet=3)

header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

coach<-header.true(coach)

student<-header.true(student)

team<-header.true(team)


skim(coach)
replace_phone<- function(dataset){
  dataset %>% 
    mutate(Phone=str_replace_all(Phone,"\\)|\\-|\\(|\\ ",""))

}

coach<-replace_phone(coach)
student<-replace_phone(student)

team$meet_day<-rbinom(250, 1, 0.5)
team$meet_time<-rbinom(250, 1, 0.5)
team$accepting<-rbinom(250, 1, 0.5)
team<- team %>%  
  mutate(meet_day=ifelse(meet_day==0,"weekday","weekend"),
         meet_time=ifelse(meet_time==0  | meet_day=="weekday","afternoon","morning"),
         accepting=ifelse(accepting==0,"yes","no"))

addQutations <-function(dataset){
  dataset$`First Name`<-paste0('"',dataset$`First Name`,'"')
  dataset$`Last Name`<- paste0('"',dataset$`Last Name`,'"')
  dataset$Address<-  paste0('"',dataset$Address,'"') 
  dataset$Pronouns<-paste0('"',dataset$Pronouns,'"') 
  dataset$Phone<-paste0('"',dataset$Phone,'"')
  dataset$Email<-paste0('"',dataset$Email,'"') 
  return( dataset)
}
coach<-addQutations(coach)
student<-addQutations(student)


student<-student %>% 
  mutate(whole_sentence=str_c(`First Name`,`Last Name`, Age, Address, Radius, Pronouns, Phone, Email,sep=","))


noquote(student$whole_sentence)
sentence<-"Create table Student( First_Name VARCHAR(255), Last_Name VARCHAR(255), Age INT, Address VARCHAR(255), Radius INT, Pronouns VARCHAR(255), Phone VARCHAR(255), Email VARCHAR(255), Student_ID INT NOT NULL AUTO_INCREMENT, PRIMARY KEY(Student_ID));"
sentence2<-"INSERT INTO Student(First_Name, Last_Name, Age, Address, Radius, Pronouns, Phone, Email)values ("
p<-paste0(sentence2,(student$whole_sentence),");")
writeLines(p,"output.txt")
str_c(student$whole_sentence,sep=",")





coach<-coach %>% 
  mutate(whole_sentence=str_c(`First Name`,`Last Name`, Age, Address, Radius, Pronouns, Phone, Email,sep=","))


noquote(coach$whole_sentence)
sentence<-"Create table coach( First_Name VARCHAR(255), Last_Name VARCHAR(255), Age INT, Address VARCHAR(255), Radius INT, Pronouns VARCHAR(255), Phone VARCHAR(255), Email VARCHAR(255), coach_ID INT NOT NULL AUTO_INCREMENT, PRIMARY KEY(coach_ID));"
sentence2<-"INSERT INTO coach(First_Name, Last_Name, Age, Address, Radius, Pronouns, Phone, Email)values ("
p<-paste0(sentence2,(coach$whole_sentence),");")
writeLines(p,"output.txt")
str_c(coach$whole_sentence,sep=",")

sentence<-"Create table team(Team_Name VARCHAR(255), Zip_Code VARCHAR(255), Team_Number VARCHAR(255) NOT NULL, Organization VARCHAR(255), Meet_Day VARCHAR(255), Meet_Time VARCHAR(255), Accepting VARCHAR(255), PRIMARY KEY(Team_Number));"

team$`Team Name`<-paste0('"',team$`Team Name`,'"')
team$`Zip Code`<- paste0('"',team$`Zip Code`,'"')
team$`Team Number`<-  paste0('"',team$`Team Number`,'"') 
team$Organization<-paste0('"',team$Organization,'"') 
team$meet_day<-paste0('"',team$meet_day,'"')
team$meet_time<-paste0('"',team$meet_time,'"') 
team$accepting<-paste0('"',team$accepting,'"') 


team<-team %>% 
  mutate(whole_sentence=str_c(`Team Name`,`Zip Code`,`Team Number`, Organization, meet_day, meet_time, accepting,sep=","))
sentence2<-"INSERT INTO team(Team_Name, Zip_Code, Team_Number, Organization, Meet_Day, Meet_Time, Accepting)values ("
p<-paste0(sentence2,(team$whole_sentence),");")
writeLines(p,"output.txt")


