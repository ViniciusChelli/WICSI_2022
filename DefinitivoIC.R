library(SnowballC)#Porter's word stemming algorithm
library(tm)#text mining
library(NLP) 
library(caret)
library(tidytext)
library(tibble)
library(caret)#classificação e etc.
library(data.table)#extensão das funcionalidades do data frame
library(tidyverse)#ciência de dados
library(wordcloud2)#nuvem de palavras 
library(RColorBrewer)#palheta de cores a mais 
library(textdata)
library(RSentiment) 
library(plotly)#Ferramenta gráfica interativa
library(superml)#CountVectorizer 
library(textstem)
library(syuzhet)
library(tuber)#Realiza a conexão com a API do youtube
setwd("C:/Users/user/Desktop/DocIC")  
app_id="@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@" #Secreto, cada API tem suas chaves
secret_id="@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@" #Secreto, cada API tem suas chaves
yt_oauth(app_id,secret_id,token = '') 
comment=get_all_comments(video_id = "E98hC9e__Xs")
detalhes=get_video_details(video_id = "Ks7mmrer37g",part = "snippet") 
tags=unlist(detalhes$items[[1]]$snippet$tags)
titulo=unlist(detalhes$items[[1]]$snippet$title) 
descricao=unlist(detalhes$items[[1]]$snippet$description) 
Video=data.frame(titulo,descrição,tags,categoria)
nrc=get_sentiment_dictionary("nrc")
#Pegar título, descrição, tags e categoria
#Lexico de sentimentos* - criação com base na fusão do léxico bing com as palavras positivas 
#e negativas do léxico NRC, usando também a função unique() para eliminar linhas repetidas
BINGAmplified=NULL 
NRCAtualized=read.csv("NRC.csv",sep=",",header = T) 
#Função para pré-processar os detalhes e pegarmos os vetores
GetSentiment()
set.seed(1234)# Eliminar a randomicidade nos dados  
#Guia por Tema
Temas=c("Film & Animation","Autos & Vehicles","Music","Pets & Animals","Sports" 
        ,"Travel & Events","Gaming","People & Blogs" 
        ,"Comedy","Entertainment","News & Politics","Howto & Style","Education" 
        ,"Science & Technology","Nonprofits & Activism","Movies") 
id=c(1,2,10,15,17,19,20,22,23,24,25,26,27,28,29,30) 
Guia=data.frame(id,Temas) 
################################ 
#nlp 
Vetor=c(comment$textDisplay)
#remove as tags html 
Vetor=gsub(pattern = "<.*>",replacement = "",x=Vetor) 
#remove os emoticons/emojis
Vetor=iconv(Vetor,"latin1","ASCII",sub = "")
#Cria o Corpus (documento)
Corpus=VCorpus(VectorSource(Vetor),readerControl = list(language="en"))
#remove os números
Corpus.trans=tm_map(Corpus,removeNumbers)
#remove pontuações
Corpus.trans=tm_map(Corpus.trans,removePunctuation)#remove pontuações
#deixa tudo minúsculo 
Corpus.trans=tm_map(Corpus.trans,content_transformer(tolower))
#colocar as palavras num vetor de caracteres 
sw=stop_words#Três lexicos (SnowBall, Onix, SMART) em inglês -> mais correto 
s=sw[,1] 
stopword=unlist(s, recursive = TRUE, use.names = TRUE)#colocar as palavras num vetor de caracteres
#remove as stopwords do Inglês
Corpus.trans=tm_map(Corpus.trans,removeWords,stopword) 
#remove os espaços em branco 
Corpus.trans=tm_map(Corpus.trans,stripWhitespace) 
#Aplica a Lematização das palavras 
Corpus.trans=tm_map(Corpus.trans,lemmatize_strings) 
#Passa para PlainTextDocument - matriz 
Corpus.trans=tm_map(Corpus.trans,PlainTextDocument)
#Passa para tibble
Tema=tidy(Corpus.trans) 
Tema$author=NULL 
Tema$heading=NULL 
Tema$description=NULL 
Tema$origin=NULL
Tema$datetimestamp=NULL 
Tema$language=NULL
Tema$id=NULL
#Tokenization 
Tokens=Tema %>% unnest_tokens(word,text)
#################################### 
#  Análise com o Bing amplificado  #
#################################### 
#Extrai o sentimento

tabelaBing=Tokens %>% 
        inner_join(BINGAmplified) %>% 
        count(sentiment)
V_p=filter(tabelaBing,sentiment=="positive")$n  
V_n=filter(tabelaBing,sentiment=="negative")$n   
if(is_empty(V_p)) 
   V_p=0
if(is_empty(V_n)) 
   V_n=0 
Vetor=c(V_n,V_p,)
 #SentimentoBing=filter(tabelaBing,n==max(tabelaBing$n))[1] 
# SentimentoBing=as.character(SentimentoBing=filter(tabelaBing,n==max(tabelaBing$n))[1])
wordcloud2(tabelaBing,size = 0.8,color = "random-dark",shape = "pentagon",backgroundColor = "black") 
#Wordcloud2 shapes: cardioid,circle,diamond,triangle-forward,triangle,pentagon,star

#Term frequency dos tokens  
freqBing=Tokens %>%count(word,sort = TRUE)#conto o número de incidencias dos tokens
for (i in 1:nrow(freqBing)) {
        freqBing$TF[i]=round(freqBing$n[i]/nrow(freqBing),digits = 4)
}#crio a coluna com o termo TF para cada token 
View(freqBing) 
#Wordcloud das palavras mais recorrentes (leva em conta o valor n) 
wordcloud2(freqBing,size=2,color = "random-dark",shape = "pentagon",backgroundColor = "black")
#Wordcloud das palavras dado suas TFs
freqBingTF=freqBing
freqBingTF$n=NULL 
wordcloud2(freqBingTF,size=1.5,color = "random-dark",shape = "circle",backgroundColor = "black") 
#Análise da frequência dos tokens mais utilizados junto da emoção 
ContadorBing <- Tokens %>%
        inner_join(BINGAmplified) %>%
        count(word, sentiment, sort = TRUE) %>%
        ungroup() 
Linha=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25")
#Contribuição de cada palavra para o sentimento do vídeo n=5%*nrow(contador)
ContadorBing %>% 
  count(sentiment, word, wt = n, sort = TRUE) %>%
  filter(rownames(ContadorBing) %in% Linha) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n))%>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Contribuição para o sentimento")

#Gráfico de Número de palavras por sentimento
ggplot(ContadorBing,aes(x=sentiment,y=n)) + 
        geom_col(fill="dodgerblue") + 
        labs(title = "Vídeo Qualquer",x="Sentimentos",y="Nº de palavras") + 
        theme(axis.text.x = element_text(angle = 90,hjust = 1))
#Gráfico de setores por sentimento nesse vídeo específico
plot_ly(tabelaBing,labels=~sentiment,values=~n,type="pie") %>% 
          layout(title="Sentimentos no vídeo em porcentagem", 
                 yaxis=list(showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE), 
                 xaxis=list(showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE)) 
 
#################################### 
#  Análise com o NRC Atualizado    #
#################################### 
tabelaNRC=Tokens %>% 
inner_join(nrc) %>% 
count(sentiment)
SentimentoNRC=filter(tabelaNRC,n==max(tabelaNRC$n))[1] 
SentimentoNRC=as.character(SentimentoNRC)
SentimentCloud=wordcloud2(tabelaNRC,size = 0.5,color = "random-dark",shape = "pentagon",backgroundColor = "black") 
#Wordcloud2 shapes: cardioid,circle,diamond,triangle-forward,triangle,pentagon,star
sum(tabelaNRC$n)
#Term frequency dos tokens  
freqNRC=Tokens %>%count(word,sort = TRUE)#conto o número de incidencias dos tokens
for (i in 1:nrow(freqNRC)) {
  freqNRC$TF[i]=round(freqNRC$n[i]/nrow(freqNRC),digits = 4)
}#crio a coluna com o termo TF para cada token 
View(freqNRC) 
#Wordcloud das palavras mais recorrentes (leva em conta o valor n) 
wordcloud2(freqNRC,size=2,color = "random-dark",shape = "pentagon",backgroundColor = "black")
#Wordcloud das palavras dado suas TFs
freqNRCTF=freqNRC
TermCloud=wordcloud2(freqNRCTF,size=1,color = "random-dark",shape = "circle",backgroundColor = "black") 
#Análise da frequência dos tokens mais utilizados junto da emoção 
ContadorNRC <- Tokens %>%
  inner_join(nrc) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
#Contribuição de cada palavra para o sentimento do vídeo n=5%*nrow(contador)
ContadorNRC %>% 
  count(sentiment, word, wt = n,sort = TRUE) %>%
  filter(rownames(ContadorNRC) %in% Linha) %>%
  # mutate(n =ifelse(sentiment=="anger",-n,n))%>% 
  # mutate(n = ifelse(sentiment == "negative", -n, n)) %>% 
  # mutate(n = ifelse(sentiment == "disgust", -n, n)) %>%
  # mutate(n = ifelse(sentiment == "fear", -n, n)) %>%
  # mutate(n = ifelse(sentiment == "sadness", -n, n)) %>%
  # mutate(n = ifelse(sentiment == "anticipation", -n, n))%>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Contribuição para o sentimento")

#Gráfico de Número de palavras por sentimento
ggplot(ContadorNRC,aes(x=sentiment,y=n)) + 
  geom_col(fill="dodgerblue") + 
  labs(title = "Vídeo Qualquer",x="Sentimentos",y="Nº de palavras") + 
  theme(axis.text.x = element_text(angle = 90,hjust = 1))
#Gráfico de setores por sentimento nesse vídeo específico
plot_ly(tabelaNRC,labels=~sentiment,values=~n,type="pie") %>% 
  layout(title="Sentimentos no vídeo em porcentagem", 
         yaxis=list(showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE), 
         xaxis=list(showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE)) 

# sents=Tema$text 
# View(sents)
# cfv=CountVectorizer$new(max_features = 100,remove_stopwords = TRUE)
# cf_mat=cfv$fit_transform(sents) 
#  
# cfv=CountVectorizer$new(max_features = 100,remove_stopwords = TRUE,ngram_range = c(1,2))
#  
# cf_mat=cfv$fit_transform(sents) 
# head(cf_mat,3) 
#  
# treino=data.table(text=sents,target=rep(c("negative","positive"),3)) 
# teste=data.table(text=sample(sents),target=rep(c("positiv","negative"),3)) 
# treino$target=as.factor(treino$target)
# teste$target=as.factor(teste$target) 
# modelo=train(target~text,treino,method = "naive_bayes",metric = "Accuracy",trControl=trainControl(method = "cv",number = 5))
# previsao=predict(modelo,teste) 
# confusionMatrix(previsao,teste$target) 
 
 
# dtm=TermDocumentMatrix(Corpus.trans)
# dtm=removeSparseTerms(dtm,sparse = 0.99) 
# coments15=tidy(dtm) 
# #Calculando o tf-idf de cada par termo-fala para avaliar as mais importantes 
# coments15=coments15%>%bind_tf_idf(term,document,count) %>% arrange(desc(tf_idf))
# #Gráfico de barra com as palavras mais importantes do tema 15 
# ggplot(coments15,aes(x=term,y=tf)) + 
#         geom_col(fill="dodgerblue") + 
#         labs(title = "Tema 15",subtitle = "Pets and Animals",x="Palavras",y="Mais faladas") + 
#         theme(axis.text.x = element_text(angle = 90,hjust = 1))
# 
# coments15$tf

# Matriz=as.matrix(dtm) 
# inspect(dtm) 
# palavras=sort(rowSums(Matriz),decreasing = TRUE)
# DF=data.frame(word=names(palavras),freqBing=palavras) 

 
#Associacoes=findAssocs(Matriz,terms = TerfreqBing,corlimit = 0.4)
