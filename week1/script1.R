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
write_csv(df2,"/Users/lingyunfan/all_repos/nyc_highschools/NYC_High_Schools/data/restructured.csv")

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




library(leaflet)
library(RColorBrewer)
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
view(table(df2$top_priority))




view(table(df2$addtl_info))
view(table(df2$eligibility))


