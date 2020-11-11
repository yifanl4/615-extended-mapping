#creating new csv file for d4
d1 = read.csv("PublicAssistanceFundedProjectsDetails.csv")
d1 = separate(col = declarationDate, into = c("declarationYear","others"), sep = "-", remove = FALSE, data = d1)
d2 = select(d1, c(3,5,8,9,13,15,17,18,19,20))
d3 = d2 %>% filter(d2$declarationYear %in% c(2009:2018))
d4 = filter(d3, incidentType=="Hurricane")
write.table(d4, "C:/Users/Yifan Liu/MA615/extended_mapping/d4.csv", col.names = TRUE, row.names = FALSE, sep = ",")
