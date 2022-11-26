################################################################################
###########################Paper limitations#################################### 
##################################HEDGING#######################################
################################################################################
################################################################################
#Working paper Cortes et al. 
################################################################################
#load packages 
if(!require(stringr)) install.packages("stringr"); library(stringr)
if(!require(gdata)) install.packages("gdata"); library(gdata)
if(!require(readtext)) install.packages("readtext"); library(readtext)
if(!require(tokenizers)) install.packages("tokenizers"); library(tokenizers)
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)

################################################################################
data=read.csv("data_extraction_2022_final_edit.csv")
data$AUTHOR=trim(gsub("[[:punct:]]+|[[:digit:]]+", " ", data$AUTHOR))  
names(data)
data=data[-c(1)]

#hedges clues
hedges=read.csv("hedges_clues_FINAL.csv")
names(hedges)
hedges=subset(hedges, hedges$Included=="Yes")

text_path <- list.files("C:/Users/Win/Desktop/PAPER LIMITATIONS/analises", pattern = "txt$")

#Read text files, discussion section
text_data=readtext(text_path)
names(text_data)
head(text_data)

text_data$NUMBER=vapply(strsplit(text_data$doc_id, "-"), "[", "", 1)
text_data$AUTHOR=vapply(strsplit(text_data$doc_id, "et al"), "[", "", 1)
text_data$AUTHOR=trim(gsub("[[:punct:]]+|[[:digit:]]+", " ", text_data$AUTHOR))  
text_data$YEAR=trim(vapply(strsplit(text_data$doc_id, "et al"), "[", "", 2))
text_data$YEAR=str_extract(text_data$YEAR, "[[:digit:]]{4}")

data_all=merge(data, text_data, by=c("YEAR", "AUTHOR", "NUMBER"))
names(data_all)

##################################################################################
##Cleaning, tokenize sentences
text_data$text[100]
text_data$text=gsub("et al.", "et al", text_data$text)
text_data$text=sub("(?<=\\d)\\.(?=\\d)|(?<=\\d)\\,(?=\\d)", " ", text_data$text, perl = TRUE)
text_data$text=gsub("DISCUSSION|discussion|Discussion", "", text_data$text)
text_data$text=gsub("[[:space:]]+", " ", text_data$text)
text_data$text=trimws(text_data$text)

#discussion=text=strsplit(text_data$text, "\\.")
#discussion_sent=strsplit(text_data$text, "(?<=\\.)\\s(?=[A-Z])", perl = T)
#str_replace_all(text_data$text, "[\r\n]" , "")


text_data$text=tokenize_sentences(text_data$text, lowercase = TRUE)
text_data$text[100]

#text_data$text=str_replace_all(text_data$discussion_sent, "[\r\n]|[\n]" , " ")
#discussion_sent=str_replace_all(discussion_sent, "" , "")

data_all=merge(data, text_data, by=c("YEAR", "AUTHOR", "NUMBER"))
names(data_all)

###############################################################
#hedges clues
hedges=read.csv("hedges_clues_FINAL.csv", na.strings=c("","NA"))
names(hedges)
hedges=subset(hedges, hedges$Included=="Yes")
hedges$Cue=trim(hedges$Cue)

hedges$Rule1_type=ifelse(is.na(hedges$Rule1_context), NA, hedges$Rule1_type)
hedges$Rule2_type=ifelse(is.na(hedges$Rule2_context), NA, hedges$Rule2_type)

hedges=hedges %>% 
  mutate(Rule1_context = ifelse(grepl(",",Rule1_context)==T, trim(strsplit(Rule1_context, ", ",
                                                                         Rule1_context)), Rule1_context)) %>%
  
  unnest(Rule1_context)

hedges$Rule1_type=trim(hedges$Rule1_type)


hedges=hedges %>% 
  mutate(Rule2_context = ifelse(grepl(",",Rule2_context)==T, trim(strsplit(Rule2_context, ", ",
                                                                           Rule2_context)), Rule1_context)) %>%
  
  unnest(Rule2_context)

hedges$Rule2_type=trim(hedges$Rule2_type)

hedges$Cue=tolower(hedges$Cue)
hedges$Cue=gsub("[[:punct:]]+","" ,hedges$Cue)

hedges$Rule1_context=tolower(hedges$Rule1_context)
hedges$Rule2_context=tolower(hedges$Rule2_context)

hedges$frase=ifelse(hedges$Rule1_type=="Followed by", 
                    paste(hedges$Cue, hedges$Rule1_context), hedges$Cue)

hedges$frase=ifelse(hedges$Rule1_type=="Preceded by", 
                    paste(hedges$Rule1_context, hedges$Cue), hedges$frase)

hedges$frase=ifelse(hedges$Rule1_type=="Not preceded by", 
                    paste(hedges$Rule1_context, hedges$Cue), hedges$frase)

hedges$frase=ifelse(hedges$Rule1_type=="Not followed by", 
                    paste(hedges$Cue, hedges$Rule1_context),hedges$frase)

hedges$frase=ifelse(hedges$Rule1_type=="Not", 
                    hedges$Cue, hedges$frase)

hedges$frase=ifelse(hedges$Rule1_type=="Preceded and", 
                    paste(hedges$Rule1_context, hedges$Cue, hedges$Rule2_context), hedges$frase)

#i give up
hedges$frase2=ifelse(is.na(hedges$Rule2_type), "NA", hedges$Cue)
                     
hedges$frase2=ifelse(hedges$Rule2_type=="Followed by", 
                    paste(hedges$Cue, hedges$Rule2_context),hedges$frase2)

hedges$frase2=ifelse(hedges$Rule2_type=="Preceded by" & (hedges$Rule1_type=="Preceded and")==FALSE, 
                    paste(hedges$Rule2_context, hedges$Cue), hedges$frase2)


hedges$frase2=ifelse(hedges$Rule2_type=="Not preceded by", 
                    paste(hedges$Rule2_context, hedges$Cue),hedges$frase2)


hedges$frase2=ifelse(hedges$Rule2_type=="Not followed by", 
                    paste(hedges$Cue, hedges$Rule2_context) ,hedges$frase2)

hedges$frase2=ifelse(hedges$Rule2_type=="Not", 
                    hedges$Cue, hedges$frase2)


hedges$frase2=ifelse(hedges$Rule2_type=="Not preceded by", 
                    paste(hedges$Rule2_context, hedges$Cue), hedges$frase2)


hedges$frase=gsub("NA", "", hedges$frase)
hedges$frase=trimws(hedges$frase)
hedges$frase2=gsub("NA", "", hedges$frase2)
hedges$frase2=trimws(hedges$frase2)

names(hedges)
 hedges1=hedges[c(1,2,3,5,6,9)]
 hedges2=hedges[c(1,2,3,7,8,10)]
 
 names(hedges1)
 names(hedges1)[4]="Rule"
 names(hedges1)[5]="Rule_context"
 names(hedges1)[6]="String_match"
 names(hedges2)
 names(hedges2)[4]="Rule"
 names(hedges2)[5]="Rule_context"
 names(hedges2)[6]="String_match"
 

 #hedges1=unique.data.frame(hedges1)
 #hedges2=unique.data.frame(hedges2)
 hedges=rbind.data.frame(hedges1, hedges2)
 hedges=unique.data.frame(hedges)



hedges$String_match=gsub("ussually", "usually", hedges$String_match)
write.csv(hedges, file="hedges_final_list.csv")

rm(hedges1, hedges2)

names(text_data)
text_data_red=text_data[c(1,2)]
hedged_sentences=merge(hedges, text_data_red, all = T)


names(hedged_sentences)=c("hedge_cue", "hedges_type", "rule",
                   
                   "text_id", "sentences_index")

###################################################################


   for(i in 1:nrow(hedges2)) {   
     print(i)
     
  #for(j in 1:nrow(text_data)) {
    
    hedged_sentences$text_id[i]=text_data$doc_id[j]
    hedged_sentences$hedge_cue[i,j]=hedges2$Cue[i]
    hedged_sentences$hedges_type [i,j]=hedges2$Rule_type[i]
    hedged_sentences$rule[i,j]=hedges2$final_rule[i]
    hedged_sentences$sentences_index[i,j]=as.character(grep(hedges2$final_rule[i], text_data$text[[j]]))
      
       }
#}


#CADA HEDGE 605 - INDICES DAS SENDESCES COM HEDGES 
#NAO SALVA O TEXTO SN PASSARA DE 10G, SALVA O HEDGES, 