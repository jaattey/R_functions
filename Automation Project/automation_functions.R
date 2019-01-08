#######################################################################################################################################################
#Function to scrap the necessary data from the scraped website
#Inputs:
#       1.Jira number in the form AUR-7024
#       2. A vector containing your username and password
#       3. For this to work, you need to be able to access Ebay JIRA
#Output: 
#       1. Title of the R markdown report :report_title
#       2. A list from which section headers of the rmd report can be obtained : headers
#       3. Corresponding field values of this list : contents
#       4. Nummber of lines/elements of these field values to be used in another function: numbers
source("~/Documents/R/libraries.R")
setwd("/Users/jattey/Documents/Automation Project/")
id                    = as.character(read_xlsx("corp_id.xlsx"))
username              = id[1]
password              = id[2]
#######################################################################################################################################################












#######################################################################################################################################################

get_jira              = function(experiment = "AUR-7024", id =c(username,password)){
  ###Raw tml document
  urllink             = paste0("https://jira.corp.ebay.com/browse/",experiment)
  get_cred            = GET(urllink, authenticate(user = id[1],password = id[2]))
  raw_html            = content(get_cred, "parsed") 
  
  ###Rmd title
  raw_title           =  unlist((raw_html %>% html_nodes("title") %>% html_text(trim = TRUE) %>% strsplit("- Jira -")))[1]
  aur_num             = (gsub(".*\\[(.*)\\].*", "\\1", raw_title))
  aur_tit             = unlist(strsplit(raw_title,"]"))[2]
  rmd_title           = trimws(paste0(aur_num, ":", aur_tit),"r" )
  
  
  
  ### headers;contents;numbers
  raw_fld             =  as.list(raw_html %>% html_nodes("div.wrap") %>% html_text(trim = TRUE))
  n_flds              =  length(raw_fld)
  
  flds                = list()
  raw_resp            = list()
  rpls                = list()
  rpls_n              = list()
  value               = list()
  n_values            = list()
  values              = list()
  
  pattern             = "Show\n"
  br                  = "\r \n"
  
  for(i in 1:n_flds) {
    flds[i]           = unlist(strsplit(as.character(raw_fld[i]), ":\n"))[1]
    raw_resp[i]       = unlist(strsplit(as.character(raw_fld[i]), ":\n"))[-1]
    
    rpls[i]           = ifelse(grepl(pattern, raw_resp[i]),unlist(strsplit(as.character(raw_resp[i]), pattern))[-1], raw_resp[i] )
    rpls_n[i]         = trimws(sub("\n", "\\1",rpls[i]),"l")
    
    #trying to get a  multi dimensional line  
    value[i]          = ifelse(grepl(br,rpls_n[i]),strsplit(as.character(rpls_n[i]), br),strsplit(as.character(rpls_n[i]),";") )   
    values[i]         = as.list(ifelse(grepl("\t\t\t", value[i]),str_extract_all(tolower(value[i]),"[a-z]{3,7}" ) , value[i]))
    n_values[i]       = length(unlist(values[i]))
    
  }
  
  
  
  ###Adjusting the names
  fldsjira            = sub("Falsifiable hypothesis", "Hypothesis", as.character(flds))
  sections            = sub("Audience \\(text\\)", "Exposed groups", fldsjira)
  
  
  
  #Obtaining the full name
  metaflds            = raw_html %>% html_nodes("meta")
  nameindex           = which(metaflds %like% "ajs-remote-user-fullname")
  nameline            = metaflds[nameindex]
  names               = str_extract_all(gsub(".*(content=)(.*)>\n","\\2",nameline), "[A-Z][a-z]{0,100}")[[1]]
  fullname            = paste(names[1],names[2], sep = " ")
  
  email              = paste0(tolower(username), "@ebay.com")
  
  
    
    
    
  ###Check if partenrs filled the right tickets (Experiments)
  if(values[[1]] != "Experiment"){
    stop(paste(experiment,"is not an experiment canvas. Please fill it in usign a proper experiment canvas."))
  }else{
    
    output            = list(report_title=rmd_title, headers=sections, contents=values, numbers=n_values, link= urllink, name=fullname, email=email)
    return(output)
  }

  

}

#######################################################################################################################################################












#######################################################################################################################################################
#Special treatment for context==> no bullet
write_context_jira       = function(output = output){
  secttmp              = c("TL;DR","Context", "Hypothesis", "Success Metrics","Health Metrics", "Learning Metrics" , "Exposed groups","Platform(s)",
                           "Manipulation","Links","Results", "Period analysed",  "Recommendation", "Reasons")
  #Jira sections; non jira section
  context              = secttmp[2]
  indices              = match(context,output$headers)

  if (sum(!is.na(indices)) == 0) {
    stop("Cannot retrieve data from JIRA fields. Please make sure the fields are correctly filled.")
  }else{
    indices            = indices[!is.na(indices)]
  }
  
  
  space                = c("  ", "  ")
  n_prm                = length(indices) 
  n_spc                = length(space)
  
  headers              = output$headers
  link                 = output$urllink
  contents             = output$contents
  numbers              = output$numbers
  title                = output$report_title
  
  for(n in 1:n_prm ){
    h_ind             = indices[n]
    n_line            = numbers[[h_ind]] + 2
    header            = paste0("####",headers[h_ind])
    
    texts             = contents[[h_ind]]
    
    textsect          = append(header, texts)
    paragraph         = append(textsect,space)
    
    tmp       = "%s
"
    for(r in 1:(numbers[[h_ind]] +n_spc) ) {
      cat(sprintf(tmp,paragraph[r]))
    }  
  }
}




#######################################################################################################################################################














#######################################################################################################################################################


write_text_jira        = function(output =output){
  # headers             = append(sections,"TL;DR")
  # contents            = append(values, " `parameters` ")
  # numbers             = append(n_values,1)
  secttmp              = c("TL;DR","Context", "Hypothesis", "Success Metrics","Health Metrics", "Learning Metrics" , "Exposed groups","Platform(s)",
                           "Manipulation","Links","Results", "Period analysed",  "Recommendation", "Reasons")
  #Jira sections; non jira section
  nobullet             = secttmp[c(1,2)]
  bullet               = secttmp[-c(1,2)]
  platforms            = secttmp[9]
  context              = secttmp[2]
  
  #jira vs non jira
  analdat              = secttmp[c(1,11,12,13,14)]
  jiradat              = secttmp[-c(1,2,11,12,13,14)]
  
  ######################################################################################################################################################
  indices              = match(jiradat,output$headers)
  
  if (sum(!is.na(indices)) == 0) {
    stop("Cannot retrieve data from JIRA fields. Please make sure the fields are correctly filled.")
  }else{
    indices            = indices[!is.na(indices)]
  }
  
  space                = c("  ", "  ")
  n_prm                = length(indices) 
  n_spc                = length(space)
  
  headers              = output$headers
  link                 = output$urllink
  contents             = output$contents
  numbers              = output$numbers
  title                = output$report_title
  
  for(n in 1:n_prm ){
    h_ind              = indices[n]
    n_line             = numbers[[h_ind]] + 2
    header             = paste0("####",headers[h_ind])
    
    texts              = paste("  *", contents[[h_ind]])
    
    textsect           = append(header, texts)
    paragraph          = append(textsect,space)
    
    tmp       = "%s
"
    for(r in 1:(numbers[[h_ind]] +n_spc) ) {
      cat(sprintf(tmp,paragraph[r]))
    }
    
  }
  
  
}







