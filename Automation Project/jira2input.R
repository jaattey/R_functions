#########################################################################################################################################################################
#From Jira to database of fields and columns


#Step 1 authenticate

rm(list=ls())
setwd("/Users/jattey/Documents/Automation Project/")
#dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_192.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
source("~/Documents/R/libraries.R")

log_in          = read_xlsx("corp_id.xlsx")
username        = as.character(log_in[1,1])
password        = as.character(log_in[1,2])


base            = 'https://jira.corp.ebay.com/browse/AUR-7024'
get_cred        = GET(base, authenticate(user = username,password = password))
#########################################################################################################################################################################
#raw_html         =  read_html("~/Documents/Automation Project/aur7024.html")
#Step 2, load html file
raw_html        = content(get_cred, "parsed") 
#write_html(raw_html, "raw_html.html")




#########################################################################################################################################################################
#manipulating titles
raw_title        =  unlist((raw_html %>% html_nodes("title") %>% html_text(trim = TRUE) %>% strsplit("- Jira -")))[1]
aur_num          = (gsub(".*\\[(.*)\\].*", "\\1", raw_title))
aur_tit          = unlist(strsplit(raw_title,"]"))[2]
rmd_title        = paste0(aur_num, ":", aur_tit)  



#########################################################################################################################################################################
#Getting fields and their values
raw_fld          =  as.list(raw_html %>% html_nodes("div.wrap") %>% html_text(trim = TRUE))
n_flds           =  length(raw_fld)
flds             = list()
raw_resp         = list()
rpls             = list()
rpls_n           = list()
value            = list()
n_values         = list()
values           = list()
rpl_list         = list()
pattern          = "Show\n"
br               = "\r \n"

for(i in 1:n_flds) {
  flds[i]        = unlist(strsplit(as.character(raw_fld[i]), ":\n"))[1]
  raw_resp[i]    = unlist(strsplit(as.character(raw_fld[i]), ":\n"))[-1]
  
  rpls[i]        = ifelse(grepl(pattern, raw_resp[i]),unlist(strsplit(as.character(raw_resp[i]), pattern))[-1], raw_resp[i] )
  rpls_n[i]      = trimws(sub("\n", "\\1",rpls[i]),"l")

#trying to get a  multi dimensional line  
  value[i]       = ifelse(grepl(br,rpls_n[i]),strsplit(as.character(rpls_n[i]), br),strsplit(as.character(rpls_n[i]),";") )   
  values[i]      = as.list(ifelse(grepl("\t\t\t", value[i]),str_extract_all(tolower(value[i]),"[a-z]{3,7}" ) , value[i]))
  n_values[i]    = length(unlist(values[i]))

}

#########################################################################################################################################################################

fldsjira         = sub("Falsifiable hypothesis", "Hypothesis", as.character(flds))
sections         = sub("Audience \\(text\\)", "Exposed groups", fldsjira)
responses        = values
numval           =  n_values
#########################################################################################################################################################################





#more efficient use
#lapply(names, strsplit, split = " ")


#tmp             = as.character(rpls_n[i])
# replies[i]     = ifelse(grepl(br ,rpls_n[i]),as.character(strsplit(tmp, br)),as.character(strsplit(tmp,";")))


#strsplit(as.character(rpls_n[17]), "\r \n")
#grepl("\r \n",rpls_n[17])
















#########################################################################################################################################################################

###Changing android and others to only platform
#pltfrm_index     = which(flds %in% "Platform(s)")
#platforms        = ....
#rpls[pltfrm_ind] = str_extract_all(tolower(test),"[a-z]{3,7}" )



#########################################################################################################################################################################
#Dividing R markdown sections in  two parts:
#         Those with info from JIRA
#         Those with info  from analysis







#########################################################################################################################################################################



#flds             = matrix(unlist(flds),n_flds,1)
#repls            = matrix(unlist(repls), n_flds,1)
#raw_resp         = matrix(unlist(raw_resp), n_flds,1)
#datjira          = data.frame(fields = flds, repls = repls, raw_resp = raw_resp)
#########################################################################################################################################################################
# Taking out duplicated lines
#Write a function  which takes out unn ecessary /n's
#test             = raw_resp[13]
#pattern          = "Show\n"
#grepl(pattern, test)





#########################################################################################################################################################################
#Next step is to make multiple lines special character seperated //<ja//> replaced from \r \n
# the words out of the \t\t sings use gsub()
#split on \t\t\t


