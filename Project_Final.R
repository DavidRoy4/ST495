library(tidyverse)
library(readxl)

fileList <- list.files(path="C:\\Users\\Sherlock\\Documents\\Fall 2022\\ST495\\Project", pattern=".xlsx")
sapply(fileList, read_xlsx)


setwd("C:\\Users\\Sherlock\\Documents\\Fall 2022\\ST495\\Project")
fnames <- list.files()
data <- lapply(fnames, read_xlsx)

Error_Master <- data[[1]]
Error_Master$DEVID <- as.numeric(Error_Master$DEVID)
Error_Master <- Error_Master[rowSums(is.na(Error_Master)) != ncol(Error_Master),]

Approvals <- data[[2]]

Users <- data[[11]]
Users <- Users %>% rename("APPROVED_BY" = "ID")

Combo <- left_join(Approvals, Users, by = "APPROVED_BY")
Combo <- Combo[rowSums(is.na(Combo)) != ncol(Combo),]
Combo <- Combo[1:156,]


Tasks <- data[[10]]
Tasks <- Tasks %>% rename("DEVID" = "ARTIFACTID")


Combined <- left_join(Error_Master, Tasks[,c("MOREINFO", "USERID", "GROUPID", "ACTION", "STATUS", "DEVID")], by = "DEVID")


#THIS EQUALS ERRORMASTER, APPROVALS, USERS, TASKS
Combined <- left_join(Combined, Combo, by = "DEVID")
Combined <- Combined[rowSums(is.na(Combined)) != ncol(Combined),]


Corrections <- data[[4]]

Node <- data[[5]]
Node <- Node %>% rename("ACTION_ID" = "ID")

#THIS IS CORRECTIONS AND CORRECTION NODE
Combined2 <- left_join(Corrections, Node, by = "ACTION_ID")


Codes <- Codes %>% mutate(CATEGORY2_ID =  ID, 
                          DESCRIPTION2 = DESCRIPTION, 
                          ATTRIBUTE_ID =  ID, 
                          DESCRIPTION3 = DESCRIPTION)
Codes <- Codes %>% rename("CATEGORY1_ID" = "ID")

Roles <- data[[9]]
Roles <- Roles[2:13,]

Roles2 <- Roles %>% mutate(ROLE1 =  ID, 
                           R_DESCRIPTION1 = DESCRIPTION, 
                           ROLE2 =  ID, 
                           R_DESCRIPTION2 = DESCRIPTION,
                           ROLE3 =  ID, 
                           R_DESCRIPTION3 = DESCRIPTION)

Error_Lookup <- data[[8]]
Error2 <- Error_Lookup %>% mutate(ROLE1 = substring(`ROLE_CODES`, 1, 1), 
                                  ROLE2 = substring(`ROLE_CODES`, 2, 2), 
                                  ROLE3 = substring(`ROLE_CODES`, 3, 3))



#THIS EQUALS ERROR LOOKUP, ROLES, CODES
Error2 <- left_join(Error2, Roles2[,c("ROLE1", "R_DESCRIPTION1")], by = "ROLE1")
Error2 <- left_join(Error2, Roles2[,c("ROLE2", "R_DESCRIPTION2")], by = "ROLE2")
Error2 <- left_join(Error2, Roles2[,c("ROLE3", "R_DESCRIPTION3")], by = "ROLE3")

Error2 <- left_join(Error2, Codes[,c("CATEGORY1_ID", "DESCRIPTION")], by = "CATEGORY1_ID")
Error2 <- left_join(Error2, Codes[,c("CATEGORY2_ID", "DESCRIPTION2")], by = "CATEGORY2_ID")
Error2 <- left_join(Error2, Codes[,c("ATTRIBUTE_ID", "DESCRIPTION3")], by = "ATTRIBUTE_ID")

#DROP VARIABLES 
Error2 <- Error2 %>% select(-c("ROLE1", "ROLE2", "ROLE3", "OCCURRENCE", "DETECTION"))

