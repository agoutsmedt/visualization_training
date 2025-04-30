data_ispole%>%
  mutate(
   book=ifelse(type_de_publication=="Monographie (Book)",1,0),
   chapter= ifelse(type_de_publication== "Contribution à ouvrage collectif (Book Chapter)",1,0),
    article=ifelse(type_de_publication=="Article de périodique (Journal article)",1,0),
   conference_paper=ifelse(type_de_publication== "Communication à un colloque (Conference Paper)",1,0)
    # type_de_publication== "Document de travail (Working Paper)"
  )%>%
  mutate(auteur_s=str_trim(auteur_s))%>%
  tidyr::separate_rows(auteur_s,sep = ";")%>%  
  mutate(auteur_s=trimws(auteur_s))%>%
  group_by(auteur_s)%>%
  summarise(
      book=sum(book),
      chapter=sum(chapter),
      article=sum(article),
      conference_paper=sum(conference_paper))%>%
  ungroup()%>%
  mutate(total = rowSums(across(c(book,chapter,article,conference_paper))))%>%
  filter(total>0)->df#%>%
  #filter(conference_paper<100&book<15)->df
  select(-total,-auteur_s)->df
  df%>%
filter(grepl("laloux",tolower(auteur_s)))
library(patchwork)
df%>%filter(book>10)
df%>%
ggplot(aes(x=conference_paper,y=book))+
  geom_count(shape=22,fill="#ED7D31",show.legend = F,,alpha=0.75)+
  geom_smooth(show.legend=F,linetype="dashed",fill="#ED7D31",color="black",method='lm')+
labs(x="Number of Conference Papers",y=NULL,,title="Book")  +
  df%>%
  ggplot(aes(x=conference_paper,y=chapter))+
  geom_count(shape=22,fill="#5B9BD5",show.legend = F,alpha=0.75)+
  geom_smooth(show.legend=F,linetype="dashed",fill="#5B9BD5",color="black",method='lm')+
  labs(x="Number of Conference Papers",y=NULL,title="Book Chapter")+
  df%>%
  ggplot(aes(x=conference_paper,y=article))+
  geom_count(shape=22,fill="#70AD47",show.legend = F,alpha=0.75)+
  geom_smooth(show.legend=F,linetype="dashed",fill="#70AD47",color="black",method='lm')+
  labs(x="Number of Conference Papers",y=NULL,title="Article")->p

p+plot_layout(axis_titles = "collect") +
  plot_annotation(title = "How different research outours are linked with the number of ocnfernece paper")&theme_ipsum(base_family = "Century Gothic")

  
data_ispole%>%
  mutate(
    book=ifelse(type_de_publication=="Monographie (Book)",1,0),
    chapter= ifelse(type_de_publication== "Contribution à ouvrage collectif (Book Chapter)",1,0),
    article=ifelse(type_de_publication=="Article de périodique (Journal article)",1,0))%>%
   
  mutate(auteur_s=str_trim(auteur_s))%>%
  tidyr::separate_rows(auteur_s,sep = ";")%>%  
  mutate(auteur_s=trimws(auteur_s))%>%
  group_by(auteur_s,peer_review)%>%
  summarise(
    book=sum(book),
    chapter=sum(chapter),
    article=sum(article)
    )%>%
  ungroup()%>%
  mutate(total = rowSums(across(c(book,chapter,article))))%>%
  filter(total>0)%>%
  filter(book<15)%>%
  inner_join(select(df,conference_paper,auteur_s),by="auteur_s")->df2

df2%>%
  ggplot(aes(x=conference_paper,y=book,fill=peer_review,color=peer_review))+
  geom_count(shape=22,alpha=0.75)+
  geom_smooth(show.legend=F,linetype="dashed",method='lm')+
  labs(x="Number of Conference Papers",y=NULL,title="Book",color=NULL,fill=NULL)  + scale_fill_manual(values=c( "#00AFBB", "#E7B800"))+
  scale_color_manual(values=c( "#00AFBB", "#E7B800"))+
  guides(size = "none")+
  df2%>%
  ggplot(aes(x=conference_paper,y=chapter,fill=peer_review,color=peer_review))+
  geom_count(shape=22,alpha=0.75)+
  geom_smooth(show.legend=F,linetype="dashed",method='lm')+
  labs(x="Number of Conference Papers",y=NULL,title="Book Chapter",color=NULL,fill=NULL)+
  scale_fill_manual(values=c( "#00AFBB", "#E7B800"))+
  scale_color_manual(values=c( "#00AFBB", "#E7B800"))+
  guides(size = "none")+
  df2%>%
  ggplot(aes(x=conference_paper,y=article,fill=peer_review,color=peer_review))+
  geom_count(shape=22,alpha=0.75)+
  geom_smooth(show.legend=F,linetype="dashed",method='lm')+
  labs(x="Number of Conference Papers",y=NULL,title="Article",color=NULL,fill=NULL)+
  scale_fill_manual(values=c( "#00AFBB", "#E7B800"))+
  scale_color_manual(values=c( "#00AFBB", "#E7B800"))+
  guides(size = "none")->p


p+plot_layout(axis_titles = "collect",guides = "collect") +
  plot_annotation(title = "How different research outours are linked with the number of ocnfernece paper")&theme_ipsum(base_family = "Century Gothic")&
  theme(legend.position = "top",legend.name=element_blank())

 
