library(readr)

setwd('/project/graziul/ra/xuranzeng')
df<-read_csv("311_Service_Requests_-_Request_Types.csv")

request<-read_csv("311_Service_Requests.csv")
request<-subset(request,STATUS!="Canceled") # delete canceled row
request<-subset(request,DUPLICATE==FALSE) # delete canceled row
request<-request[!with(request,is.na(POLICE_DISTRICT)& is.na(POLICE_BEAT)),]


remove<- c("SR_NUMBER","CITY","DUPLICATE","COMMUNITY_AREA","OWNER_DEPARTMENT",
           "STATUS","LAST_MODIFIED_DATE","CLOSED_DATE","STREET_ADDRESS","STATE",
           "STREET_NUMBER","STREET_DIRECTION","STREET_NAME" ,"STREET_TYPE",
           "LEGACY_RECORD","LEGACY_SR_NUMBER","PARENT_SR_NUMBER","WARD","
           ELECTRICAL_DISTRICT","ELECTRICITY_GRID","POLICE_SECTOR","PRECINCT",
           "SANITATION_DIVISION_DAYS","X_COORDINATE","Y_COORDINATE","LATITUDE",
           "LONGITUDE","LOCATION","ELECTRICAL_DISTRICT","CREATED_HOUR",
           "CREATED_DAY_OF_WEEK","CREATED_MONTH" )
request<-request[ , -which(names(request) %in% remove)] # delete irrelevant column

write.csv(request,"request_simplified.csv")
