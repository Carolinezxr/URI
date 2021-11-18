library(tuneR)
library(seewave)
library(lubridate)
library(hms)

transcript<-read.csv("/project/graziul/transcripts/transcripts2021_09_03.csv")

get_mp3_name<-function(i,transcript){
  wavefile<-transcript[i,'file']
  feedname<-transcript[i,'feed']
  reg<-regexpr(feedname,wavefile)
  mp3name<-substr(wavefile,1,(reg[1]+4))
  return(mp3name)
}

# from transcript file extract mp3 path (main path+zone+day+mp3name)
mp3path<-function(i,transcript){
  zonename<-transcript[i,'zone']
  wavefile<-transcript[i,'file']
  dayname<-format(as.Date(substr(wavefile,1,8),'%Y%m%d'),format="%Y_%m_%d")
  mp3name<-get_mp3_name(i,transcript)
  mp3name<-paste(mp3name,".mp3",sep="")
  mp3path<-paste("/project/graziul/data",zonename,dayname,mp3name,sep="/")
  return(mp3path)
}

wav_save_path<-function(i,transcript){
  wavefile<-transcript[i,'file']
  mp3name<-get_mp3_name(i,transcript)
  timename<-gsub('[.]','',transcript[i,'start'])
  wav_save<-paste(mp3name,timename,sep="-")
  wav_name<-paste(wav_save,".wav",sep="")
  zonename<-transcript[i,'zone']
  dayname<-format(as.Date(substr(wavefile,1,8),'%Y%m%d'),format="%Y_%m_%d")
  wav_save_path<-paste("/project/graziul/ra/team_ser/data",zonename,dayname,mp3name,wav_name,sep="/")
  return(wav_save_path)
}

# # test
# get_mp3_name(18,transcript)
# mp3path(18,transcript)
# wav_save_path(18,transcript)

get_nosilence_slice<-function(i,transcript){
  mp3_path<-mp3path(i,transcript)
  mp3<-readMP3(mp3_path)
  starttime<-as_hms(parse_date_time(transcript[i,'start'],"%H.%M.%S"))
  endtime<-as_hms(parse_date_time(transcript[i,'end'],"%H.%M.%S"))
  startsecond<-minute(starttime)*60+second(starttime)
  endsecond<-minute(endtime)*60+second(endtime)
  audio_nosilence<-extractWave(mp3,from = minute(starttime)*60+second(starttime), to = minute(endtime)*60+second(endtime), xunit="time")
  wav_save_path1<-wav_save_path(i,transcript)
  writeWave(audio_nosilence,wav_save_path1)
  return(audio_nosilence)
}

nosilence_slice_dictionary<-transcript[,c('feed','zone','year','month','day','time','start','end','transcription','length_s')]
for (i in 301:1000){
  get_nosilence_slice(i,transcript)
  nosilence_slice_dictionary[i,'mp3path']<-mp3path(i,transcript)
  nosilence_slice_dictionary[i,'wav_save_path']<-wav_save_path(i,transcript)
  filename<-get_mp3_name(i,transcript)
  zone<-transcript[i,'zone']
  cat("slicing:",i,"file",filename, zone,"\n")
}
write.csv(nosilence_slice_dictionary,"/project/graziul/ra/team_ser/nosilence_slice_dictionary.csv")
