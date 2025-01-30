library(tidyverse)
library(rjson)

# read all 1st tour data from resultats
(files <- dir('resultats',
              pattern='csv',
              full.names = TRUE))
(files <- str_subset(files,'t1_'))

df_agg <- lapply(str_subset(files,'-ag'),function(x) {
  print(x)
  readr::read_csv(x)%>% 
    select(code_insee = `Code département`,
           departement = Département,
           votes = Exprimés,
           FN = `Front national`) %>% 
    mutate(annee = str_extract(x,"[0-9]{4}"),
           across(all_of(c("code_insee","departement")),
                  factor),
           across(all_of(c("votes","FN")),
                  as.numeric)) %>% 
    arrange(code_insee,votes)
})
df_agg <- bind_rows(df_agg)

df_full <- lapply(str_subset(files,'(-ag)|(-moderne)',negate = TRUE),function(x) {
  print(x)
  readr::read_csv(x) %>% 
    select(code_insee = matches("Code (du ){0,1}département"),
           code_canton = matches("Code (du ){0,1}canton"),
           departement = matches("^(Libellé du ){0,1}[Dd]épartement"),
           votes = Exprimés,
           matches("(Nuance)|(Voix)")) %>% 
    gather(var,val,-c(code_insee, departement, code_canton, votes)) %>% 
    mutate(i = str_extract(var,"[0-9]{1,}"),
           var = str_replace_all(var,"[0-9]",""),
           var = str_replace_all(var,"\\.",""),
           var = str_replace_all(var,"Liste",""),
           var = str_trim(var)) %>% 
    spread(var,val) %>% 
    filter(Nuance=="FN") %>% 
    select(code_insee,
           departement,
           votes,
           FN = Voix) %>% 
    mutate(annee = str_extract(x,"[0-9]{4}"),
           code_insee = str_pad(code_insee,
                                width = 2,
                                side = "left",
                                pad = "0"),
           across(all_of(c("code_insee","departement")),
                  factor),
           across(all_of(c("votes","FN")),
                  as.numeric)) %>% 
    arrange(code_insee,votes)
})
df_full <- bind_rows(df_full)

df_moderne <- lapply(str_subset(files,"-moderne"),function(x) {
  print(x)
  readr::read_csv(x) %>% 
    select(code_insee = matches("Code (du ){0,1}département"),
           code_canton = matches("Code (du ){0,1}canton"),
           departement = matches("^(Libellé du ){0,1}[Dd]épartement"),
           votes = Exprimés,
           matches("(^Nuance)|(^Voix)")) %>% 
    gather(var,val,-any_of(c("code_insee", 
                             "departement", 
                             "code_canton", 
                             "votes"))) %>% 
    mutate(i = str_extract(var,"[0-9]{1,}"),
           i = as.numeric(i),
           var = str_replace_all(var,"[0-9]",""),
           var = str_replace_all(var,"\\.",""),
           var = str_replace_all(var,"Liste",""),
           var = str_trim(var)) %>% 
    group_by(pick(any_of(c("code_insee","code_canton")))) %>%
    arrange(i) %>% 
    mutate(i = rep(seq(n()/2),each = 2)) %>% 
    ungroup() %>% 
    spread(var,val) %>% 
    filter(str_detect(Nuance,"[FR]N")) %>% 
    select(code_insee,
           departement,
           votes,
           FN = Voix) %>% 
    mutate(annee = str_extract(x,"[0-9]{4}"),
           code_insee = str_pad(code_insee,
                                width = 2,
                                side = "left",
                                pad = "0"),
           departement = str_to_upper(departement),
           across(all_of(c("code_insee","departement")),
                  factor),
           across(all_of(c("votes","FN")),
                  as.numeric)) %>% 
    arrange(code_insee,votes)
})
df_moderne <- bind_rows(df_moderne)

dta <- bind_rows(list(df_agg,
                      df_full,
                      df_moderne)) %>% 
  filter(!is.na(FN),
         !is.na(votes)) %>% 
  group_by(annee,code_insee) %>% 
  summarize(FN = sum(FN)/sum(votes))

dta <- droplevels(dta)

final <- split(dta,dta$annee)
final <- lapply(final,function(df) with(df,setNames(FN,code_insee)) )

cat(toJSON(final),file=file.path('..','web','resultats.json'))

boxplot(FN~annee,data=dta)
with(dta,range(FN))
