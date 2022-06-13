rm(list=ls())
library(tidyverse)
library(ggplot2)

df<-read.csv("/Users/lingyunfan/all_repos/nyc_highschools/NYC_High_Schools/data/2021_DOE_High_School_Directory.csv")

##############preprocessing###############3
colnames(df)
variables<-c("dbn","school_name","diadetails","location","website","total_students","addtl_info1","school_accessibility_description","url")
sum(is.na(df$program4))


prgr<-cbind(df %>% dplyr:: select(starts_with("program")),df["dbn"])%>%gather(program,program_name,-dbn)%>%arrange(dbn,program)
prgr<-prgr%>%cbind(cbind(df %>% dplyr:: select(starts_with("grade9geapplicantsperseat")),df["dbn"])%>%gather(program,grade9geapplicantsperseat,-dbn)%>%arrange(dbn,program)%>%select(-program,-dbn))
prgr<-prgr%>%cbind(cbind(df %>% dplyr:: select(starts_with("grade9swdapplicantsperseat")),df["dbn"])%>%gather(program,grade9swdapplicantsperseat,-dbn)%>%arrange(dbn,program)%>%select(-program,-dbn))
prgr<-prgr%>%cbind(cbind(df %>% dplyr:: select(starts_with("eligibility")),df["dbn"])%>%gather(program,eligibility,-dbn)%>%arrange(dbn,program)%>%select(-program,-dbn))
prgr<-prgr%>%cbind(cbind(df %>% dplyr:: select(starts_with("prgdesc")),df["dbn"])%>%gather(program,prgdesc,-dbn)%>%arrange(dbn,program)%>%select(-program,-dbn))


prio<-cbind(df %>% dplyr:: select(starts_with("admissionspriority")),df["dbn"])%>%
  gather(key, value,-dbn)%>%
  extract(key, c("priority", "number"), "(admissionspriority.)(.*)")%>%
    tidyr::spread(priority,value)%>%arrange(dbn,number)%>%select(-number,-dbn)

prgr<-prgr%>%cbind(prio)


rqrm<-cbind(df %>% dplyr:: select(starts_with("requirement")),df["dbn"])%>%
  gather(key, value,-dbn)%>%
  extract(key, c("requirement", "number"), "(requirement\\_.)\\_(.*)")%>%
    spread(requirement,value)%>%arrange(dbn,number)%>%select(-number,-dbn)


prgr<-prgr%>%cbind(rqrm)

df2<-prgr%>%left_join(df[,variables],by="dbn")

df2[df2 == ""] <- NA
to_check<-df2[is.na(df2$program_name),]
to_check[,3:16]%>%
  summarise_all(funs(sum(is.na(.))))
df2[which((df2$dbn=="17K531")&(df2$program=="program1")),4:16]=df2[which((df2$dbn=="17K531")&(df2$program=="program2")),4:16]#to address the only anomaly 
df2<-df2[!is.na(df2$program_name),]

schools<-df2%>%group_by(dbn)%>%summarise(n_prgm=n())
schools<-schools%>%left_join(df[,variables],by="dbn")
table(schools$n_prgm)


schools$coor<-str_match(schools$location, "\\((.*)\\)")[,2]
library(stringr)
schools[c('lat', 'long')] <- str_split_fixed(schools$coor, ",", 2)
schools<-schools%>% 
  mutate_at(c(12:13), as.numeric)
############mapping#############
library(ggmap)
library(RJSONIO)
library(ggthemes)
setwd("/Users/lingyunfan/all_repos/nyc_highschools/NYC_High_Schools/data")
source("google_map_style.R")
style_string<-create_style_string(style_list)
register_google(key = "AIzaSyDM2SurFMVuqbdOh9GR9CMMlURLMgTiPuA", write = TRUE)
map_nyc<- get_googlemap(center =c(lon=-73.957298,lat=40.742118),zoom = 11,style=style_string)

schools$n=schools$n_prgm/20
ggmap(map_nyc,extent="normal",maprange=FALSE) +
  geom_point(data=schools, aes(x=long,y=lat,size=n,color=school_accessibility_description), alpha=1)+
  theme_map()+
  guides(size="none")+
  #theme(legend.position = "none")+
  labs(color="school accessibility")+
  coord_map(projection = "mercator",
            xlim=c(attr(map_nyc, "bb")$ll.lon+0.05,
                   attr(map_nyc, "bb")$ur.lon-0.05),
            ylim=c(attr(map_nyc, "bb")$ll.lat+0.05,
                   attr(map_nyc, "bb")$ur.lat-0.05)) 

write_csv(schools,"/Users/lingyunfan/all_repos/nyc_highschools/NYC_High_Schools/data/schools.csv")



library(leaflet)
library(RColorBrewer)
library(leaflet.extras)
pal = colorFactor("Set1", domain = schools$school_accessibility_description)
color_offsel1 = pal(schools$school_accessibility_description)
content <- paste("School:",schools$school_name,"<br/>",
                 "Number of Programs",schools$n_prgm,"<br/>",
                 "Number of Students:",schools$total_students,"<br/>",
                 "Accessibility:",schools$school_accessibility_description,"<br/>",
                 "Website:",schools$website,"<br/>")

leaflet(schools) %>%
  setView(lng=-73.957298,lat=40.742118,zoom = 12 )%>%
  addProviderTiles("Stamen.TonerLite") %>%
  addCircleMarkers(~long,~lat,color=color_offsel1,radius=6,
             popup =content,stroke = FALSE,fillOpacity = 0.5,
             clusterOptions = markerClusterOptions(maxClusterRadius = 30))%>%
  addLegend(pal = pal, values = ~schools$school_accessibility_description, title = "School Accessibility")%>%
  addSearchGoogle(apikey = Sys.getenv("AIzaSyBW-xqfaBSA41jVVGVZBqqEqUxPRmRPZ8g"),
                  options = searchOptions(autoCollapse =FALSE, minLength = 2))
  
  
  leaflet.extras::addSearchOSM(options = searchOptions(hideMarkerOnCollapse = TRUE,collapse=FALSE,
                                                       textPlaceholder = "Search for Any Location",zoom=15, position = "topleft"),
                               fitBounds=TRUE)

library(htmlwidgets)
saveWidget(map1, file="first_map.html")

save(df,df2,schools, file="workspace.RData")

###########explore the admission info#############


table(sub(" \\-.*", "", c(df2$requirement_1,df2$requirement_2,df2$requirement_3,df2$requirement_4,df2$requirement_5)))
view(as.data.frame.matrix(table(sub(" \\-.*", "",df2$requirement_1),sub(" \\-.*", "",df2$requirement_2), useNA = "ifany")))

df2$rqr1<-sub(" \\-.*", "",df2$requirement_1)
df2$rqr2<-sub(" \\-.*", "",df2$requirement_2)
df2$rqr3<-sub(" \\-.*", "",df2$requirement_3)
df2$rqr4<-sub(" \\-.*", "",df2$requirement_4)
df2$rqr5<-sub(" \\-.*", "",df2$requirement_5)

three_items<-c("2018-19 ELA/Math State Test Scores","2018-19 Final Grades","2019-20 Marking Period Grades")


fun.contains <- function(b, s){
  all(s %in% b) && length(s[duplicated(s)]) <= length(b[duplicated(b)]) &&
    (if(length(s[duplicated(s)])>0) fun.contains(b[duplicated(b)],s[duplicated(s)]) else 1 )
}

df2<-df2%>%mutate(rqr_set= mapply(c, rqr1,rqr2,rqr3,rqr4,rqr5, SIMPLIFY = F))


for(i in 1:nrow(df2)) {       # for-loop over rows
  if(fun.contains(c(df2$rqr1[i],df2$rqr2[i],df2$rqr3[i],df2$rqr4[i],df2$rqr5[i]),three_items)){
    df2[i,"three_rqr"]=TRUE
  }
}

table(df2$three_rqr)


view(as.data.frame.matrix(table(df2$admissionspriority1,df2$admissionspriority2,useNA = "ifany")))

view(table(df2$admissionspriority1))
view(table(df2$admissionspriority2))
view(table(df2$admissionspriority3))

#recoding the priority 1
df2$top_priority=ifelse(df2$admissionspriority1=="Open to New York City residents","to NYC residents","Other")
df2$top_priority=ifelse(df2$admissionspriority1=="Priority to continuing 8th graders","to continuing 8th graders",df2$top_priority)
df2$top_priority=ifelse(df2$admissionspriority1=="Priority to Bronx students or residents","to Bronx residents",df2$top_priority)
df2$top_priority=ifelse(df2$admissionspriority1=="Priority to Brooklyn students or residents","to Brooklyn residents",df2$top_priority)
df2$top_priority=ifelse(df2$admissionspriority1=="Priority to Brooklyn students or residents for up to 50% of the seats","to Brooklyn residents",df2$top_priority)
df2$top_priority=ifelse(df2$admissionspriority1=="Priority to Manhattan students or residents","to Manhattan residents",df2$top_priority)
df2$top_priority=ifelse(df2$admissionspriority1=="Priority to Queens students or residents","to Queens residents",df2$top_priority)
df2$top_priority=ifelse(df2$admissionspriority1=="Priority to Staten Island students or residents","to Staten Island residents",df2$top_priority)
df2$top_priority=ifelse(is.na(df2$top_priority),"No Priority",df2$top_priority)
view(table(df2$top_priority))




view(table(df2$addtl_info))
view(table(df2$eligibility))
view(table(df2$diadetails))

df<-df%>%mutate(prgm_set= mapply(c,program1,program2,program3,
                                 program4,program5,program6,
                                 program7,program8,program9,
                                 program10,program11,program12,SIMPLIFY = F))
schools<-schools%>%arrange(dbn)
df<-df%>%arrange(dbn)
schools$prgm_set<-df$prgm_set
schools$prgm_set<-lapply(schools$prgm_set,function(x) paste(x, collapse=', ' ))
schools$prgm_set<-lapply(schools$prgm_set,unlist)
to_check<-unnest(schools, prgm_set)
to_check$prgm_set<-gsub(" ,", ",", to_check$prgm_set, fixed=TRUE)
to_check$prgm_set<-trimws(to_check$prgm_set)
to_check$prgm_set<-sub('[[:punct:]]+$', '', to_check$prgm_set)



write_csv(to_check,"/Users/lingyunfan/all_repos/nyc_highschools/NYC_High_Schools/data/schools.csv")


df2$ge_per_seat<-as.numeric(df2$grade9geapplicantsperseat)
max(df2$ge_per_seat,na.rm = TRUE)



write_csv(df2[,-30],"/Users/lingyunfan/all_repos/nyc_highschools/NYC_High_Schools/data/restructured.csv")



table(df2$school_accessibility_description)




df2<-read.csv("/Users/lingyunfan/all_repos/nyc_highschools/NYC_High_Schools/data/restructured_programs.csv")
schools<-read.csv("/Users/lingyunfan/all_repos/nyc_highschools/NYC_High_Schools/data/schools.csv")
view(table(df2$diadetails))

lis<-list(data=df,st="FDf")
class(lis$data)
view(table(df2$eligibility,useNA="always"))
?table

schools<-schools%>%group_by(long,lat)%>%mutate(counter = row_number(dbn)-1)
subs<-schools[,c("lat","long","dbn","counter")]
subs$lat=subs$lat+0.0001*subs$counter
subs$long=subs$long+0.0001*subs$counter


df2<-df2[,-33]
df2<-df2[,-34]
df2<-df2%>%left_join(subs,by="dbn")




write_csv(df2,"/Users/lingyunfan/all_repos/nyc_highschools/NYC_High_Schools/data/restructured_programs.csv")
df2<-read.csv("/Users/lingyunfan/all_repos/nyc_highschools/NYC_High_Schools/data/restructured_programs.csv")
#many schools lack ge_per_seat info, so their points are not shown on the map

min(df2$ge_per_seat,na.rm=TRUE)
df2$eligibility<-ifelse(is.na(df2$eligibility),"NA",df2$eligibility)


df<-read.csv("/Users/lingyunfan/all_repos/nyc_highschools/NYC_High_Schools/data/2021_DOE_High_School_Directory.csv")
view(table(df$ell_programs))
df$lang1=ifelse(grepl("Chinese", df$ell_programs, fixed = TRUE),TRUE,FALSE)
df$lang1=ifelse(df$lang1,"Chinese",NA)
df$lang2=ifelse(grepl("Spanish", df$ell_programs, fixed = TRUE),TRUE,FALSE)
df$lang2=ifelse(df$lang2,"Spanish",NA)
df$lang3=ifelse(grepl("Arabic", df$ell_programs, fixed = TRUE),TRUE,FALSE)
df$lang3=ifelse(df$lang3,"Arabic",NA)
df$lang4=ifelse(grepl("French", df$ell_programs, fixed = TRUE),TRUE,FALSE)
df$lang4=ifelse(df$lang4,"French",NA)
df$lang5=ifelse(grepl("Bengali", df$ell_programs, fixed = TRUE),TRUE,FALSE)
df$lang5=ifelse(df$lang5,"Bengali",NA)
df$lang6=ifelse(grepl("Haitian Creole", df$ell_programs, fixed = TRUE),TRUE,FALSE)
df$lang6=ifelse(df$lang6,"Haitian Creole",NA)

paste_noNA <- function(x,sep=", ") {
  gsub(", " ,sep, toString(x[!is.na(x) & x!="" & x!="NA"] ) ) }
df$ell <- apply( df[ , c(450:455) ] , 1 , paste_noNA , sep=", ")
df$ell <- ifelse(df$ell=="",NA,df$ell)
table(df$ell)

to_check<-df[,c("dbn","ell")]
df2<-df2%>%left_join(to_check)
table(df2$ell)
library(tidyverse)
df2$ell<-ifelse(is.na(df2$ell),"No ELL Programs",df2$ell)
write_csv(df2,"/Users/lingyunfan/all_repos/nyc_highschools/NYC_High_Schools/data/restructured_programs.csv")
write_csv(df,"/Users/lingyunfan/all_repos/nyc_highschools/NYC_High_Schools/data/2021_DOE_High_School_Directory.csv")


prgr<-cbind(df %>% dplyr:: select(starts_with("interest")),df["dbn"])%>%gather(program,program_name,-dbn)%>%arrange(dbn,program)


#add more variables
prgr$program <- gsub("interest", "program", prgr$program)
colnames(prgr)[3]<-"interest"

df2<-df2%>%left_join(prgr,by=c("dbn","program"))
view(as.data.frame(table(df2$interest))%>%arrange(-Freq))



prgr<-cbind(df %>% dplyr:: select(starts_with("code")),df["dbn"])%>%gather(program,program_name,-dbn)%>%arrange(dbn,program)
prgr$program <- gsub("code", "program", prgr$program)
colnames(prgr)[3]<-"code"

df2<-df2%>%left_join(prgr,by=c("dbn","program"))
n_distinct(df2$code)
df2$id<-paste(df2$code,df2$program_name)



df2<-df2%>%left_join(df[,c("dbn","extracurricular_activities")],by="dbn")
df2<-df2%>%left_join(df[,c("dbn","school_sports")],by="dbn")
df2<-df2%>%left_join(df[,c("dbn","language_classes")],by="dbn")
df2<-df2%>%left_join(df[,c("dbn","language_classes")],by="dbn")
df2<-df2%>%left_join(df[,c("dbn","neighborhood")],by="dbn")



df2$info <- apply(df2[,c(1,17,37,40:43)] , 1 , paste_noNA , sep="," )


write_csv(df2,"/Users/lingyunfan/all_repos/nyc_highschools/NYC_High_Schools/data/restructured_programs.csv")
write_csv(df,"/Users/lingyunfan/all_repos/nyc_highschools/NYC_High_Schools/data/2021_DOE_High_School_Directory.csv")

df2$school_accessibility_description<-ifelse(is.na(df2$school_accessibility_description),"Unknown",df2$school_accessibility_description)
unique(df2$school_accessibility_description)

library(tidyverse)

view(table(df2$eligibility))
df2$lunch<-grepl("lunch", df2$diadetails, fixed = TRUE)
table(df2$lunch)
nrow(df2[df2$lunch,])


###add more variables

table(df$international)
table(df$specialized)
table(df$girls)
table(df$boys)
table(df$transfer)
table(df$transfer)

library(tidyverse)
df2<-df2%>%left_join(df[,c("dbn","girls")],by="dbn")
df2<-df2%>%left_join(df[,c("dbn","boys")],by="dbn")
df2<-df2%>%left_join(df[,c("dbn","international")],by="dbn")
df2<-df2%>%left_join(df[,c("dbn","specialized")],by="dbn")
df2<-df2%>%left_join(df[,c("dbn","pbat")],by="dbn")
df2<-df2%>%left_join(df[,c("dbn","ptech")],by="dbn")
df2<-df2%>%left_join(df[,c("dbn","transfer")],by="dbn")
df2<-df2%>%left_join(df[,c("dbn","earlycollege")],by="dbn")




##############to incorporate sport info

library(tidyverse)
df$sport<-paste0(df$psal_sports_boys,df$psal_sports_girls,df$psal_sports_coed,df$school_sports,sep=", ")
view(df$sport_ls)

df$sport <- apply(df[,c(40:43)] , 1 , paste_noNA , sep="," )

df2<-df2%>%left_join(df[,c("dbn","sport")],by="dbn")


df$sport_ls<-lapply(df$sport,function(x) as.list(strsplit(x, ",")[[1]]))

df2$sport_ls<-lapply(df2$sport,function(x) as.list(strsplit(x, ",")[[1]]))
df2[unlist(lapply(df2$sport_ls,function(x) contain_yes(x,c("Indoor Track")))),]


sports<-as.data.frame(table(unlist(df$sport_ls)))%>%arrange(-Freq)%>%head(26)



#wheter list1 contain list2
contain_yes <- function(list1,list2) {
  result<-TRUE
  for(i in list2){
    if(!i %in% list1){result=FALSE}
  }
  return(result)
}





write_csv(sports,"/Users/lingyunfan/all_repos/nyc_highschools/NYC_High_Schools/data/sports.csv")
write_csv(df2,"/Users/lingyunfan/all_repos/nyc_highschools/NYC_High_Schools/data/restructured_programs.csv")


colnames(df2)
df2<-df2%>%left_join(df[,c("dbn","academicopportunities1")],by="dbn")


offer<-cbind(df %>% dplyr:: select(starts_with("offer_rate")),df["dbn"])%>%
  gather(key, value,-dbn)%>%
  extract(key, c("offer", "number"), "(offer_rate.)\\_(.*)")%>%
  tidyr::spread(offer,value)%>%arrange(dbn,number)

df2$number<-as.numeric(gsub("program(.*)","\\1", df2$program))
offer$number<-as.numeric(offer$number)
df2<-df2%>%left_join(offer[,1:3],by=c("dbn","number"))

df2<-df2%>%left_join(df[,c("dbn","graduation_rate","overview_paragraph")],by="dbn")
write_csv(df2[,-55],"/Users/lingyunfan/all_repos/nyc_highschools/NYC_High_Schools/data/restructured_programs.csv")


sports$Var1
