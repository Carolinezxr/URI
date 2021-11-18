# create zone files
create_zone_files<-function(transcript){
  zone_list<-unique(transcript$zone)
  path_list<-paste("/project/graziul/ra/team_ser/data",zone_list,sep="/")
  for (i in 1:length(path_list)){
    dir.create(path_list[i])
  }
}

#create day files (under zone files)
create_day_files<-function(transcript){
  zone_list<-unique(transcript$zone)
  for (i in 1:length(zone_list)){
    transcript1<-subset(transcript,zone==zone_list[i])
    transcript1$date<-format(as.Date(substr(transcript1$file,1,8),'%Y%m%d'),format="%Y_%m_%d")
    day_list<-unique(transcript1$date)
    path_list<-paste("/project/graziul/ra/team_ser/data",zone_list[i],day_list,sep="/")
    for (j in 1:length(path_list)){
      dir.create(path_list[j])
    }
  }
}

# create mp3 files (careful, manual)
i=1
transcript1<-subset(transcript,zone==zone_list[i])
transcript1$date<-format(as.Date(substr(transcript1$file,1,8),'%Y%m%d'),format="%Y_%m_%d")
for (i in 1:nrow(transcript1)){
  reg<-regexpr(transcript1[i,'feed'],transcript1[i,'file'])
  transcript1[i,'mp3name']<-substr(transcript1[i,'file'],1,(reg[1]+4))
}
i=1
day_list<-unique(transcript1$date)
zone_list[i]
for (j in 1:length(day_list)){
  transcript11<-subset(transcript1,date==day_list[j])
  mp3name<-transcript11$mp3name
  mp3name<-unique(mp3name)
  path_list<-paste("/project/graziul/ra/team_ser/data","Zone1",day_list[j],mp3name,sep="/")
  for (i in 1:length(path_list)){
    dir.create(path_list[i])
  }
}
