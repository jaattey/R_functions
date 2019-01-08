rm(list=ls())
setwd("/Users/jattey/Documents/Automation Project/")
#dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_192.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
source("~/Documents/R/libraries.R")


#Package rvest
#Try open websites
##JIRA for test is AUR-7024
####################################################################################################################################################
url             = 'https://jira.corp.ebay.com/browse/AUR-6511'
url             = 'http://forum.axishistory.com/memberlist.php'
session         = html_session(url)
pgform          = html_form(session)[[2]]
# log_in          = set_values(pgform, 
#                              "username"= "Jattey",
#                              "password"= "####s")
webpage         = read_html(url)
write_html(webpage,"test_write_1.html")





####################################################################################################################################################
testfile        = read_html("~/Documents/Automation Project/aur7024.html")
testfile %>% html_nodes("title") %>% xml_contents() #works for title
sort(testfile %>% html_nodes("strong") %>% html_text(trim = TRUE)) #-> fields # use this to get list of fields also "xml_contents(fields)" 

testfile %>% html_nodes("meta") %>% html_text(trim =TRUE)

###########deop staus, resolutions/ Fix/versions##################### do  it later



metaflds            = testfile %>% html_nodes("meta")
nameindex           = which(metaflds %like% "ajs-remote-user-fullname")
nameline            = metaflds[nameindex]
nameline            = as.character(nameline)










#testfile %>% html_nodes("div") %>% html_text(trim = TRUE) # use this to get list of fields




####################################################################################################################################################







#Specifying the url for desired website to be scraped
url <- 'http://www.imdb.com/search/title?count=100&release_date=2016,2016&title_type=feature'

#Reading the HTML code from the website
webpage <- read_html(url)
write_html(webpage,"see_stuff.html")


####################################################################################################################################################

base      = 'https://jira.corp.ebay.com/browse/AUR-7024'

data1     = GET(base, authenticate(username,password))

data2  = content(data1, "parsed") %>% html_nodes("div.wrap") %>% html_text(trim = TRUE)
#data2  = content(data1, "parsed") %>% html_nodes("div.wrap") %>% html_text(trim = TRUE)
data3   =content(data1, "parsed") %>% html_nodes("strong") %>% html_text(trim = TRUE)
 # content(data1, "parsed") %>% html_nodes("div.wrap") %>% html_text(trim = TRUE)
  #content(data1, "parsed") %>% html_nodes("div.user-content-block") %>% html_text(trim = TRUE)
  #content(data1, "parsed") %>% html_nodes("div.shorten") %>% html_text(trim = TRUE)

data = as.list(data2)
data


#hide/n and show/n

####################################################################################################################################################





get_stuff    = fromJSON(data2, flatten =FALSE)

text = write_html(data1,"see_stuf.html")









####################################################################################################################################################
#Learning regular expressions
rm(list=ls())
setwd("/Users/jattey/Documents/Automation Project/")
dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_151.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
source("~/Documents/R/libraries.R")

string <- c("Hiphopopotamus", "Rhymenoceros", "time for bottomless lyrics") 
pattern <- "t.m"
replacement = "jattey"

####################################################################################################################################################
#useful for opeining an rmd file
system("open -a Terminal .",wait=FALSE)























#######################################################################################################################################################
load("~/Documents/Automation Project/jirainputs.RData")
sectsum                = list(head ="TL;DR")
sectexp                = list(head = "Experiment", context = "Context",body = c("Hypothesis","Success Metrics","Health Metrics","Learning Metrics","Link"))
sectstr                = list(head = "Set-up and treatment",pltfrm = "Platform(s)", body = c("Exposed group", "groups"))
sectana                = list(head = "Analysis", body = c("Period analysed", "Results", "Recommendations", "Reasons"))

#######################################################################################################################################################
#Function to write main heads
write_header           = function(header = list(head = "Header")){
  space                = c("  ")
  n_spc                = length(space)
  head                 = header$head
  sect                 = paste0("###",head)
  paragraph            = append(sect,space)
  
  n_ind                = length(paragraph)

  tmp                  = "%s
  "
  
  for(r in 1: n_ind) {
    cat(sprintf(tmp,paragraph[r]))
  }
  
}

#######################################################################################################################################################
#writing shit sequentially
test2fun              = function(output = output, sect =sectres){
  write_context_jira(output = output)
  write_head(sectres)
}

my.name <- readline(prompt="Enter name: ")

#######################################################################################################################################################

andvrsn = readline(prompt = "appvrsn-android: ")
iosvrsn = readline(prompt = "appvrsn-ios: ")

pltfrm  = output$contents[[20]]



write_app_vrsn         = function(output = output){
  setup                = list(head = "Set-up and treatment",pltfrm = "Platform(s)", body = c("Exposed groups", "groups"))
  jirapltfrm          = setup$pltfrm
  indices              = match(jirapltfrm, output$headers)
  
  if (sum(!is.na(indices)) == 0) {
    warning("Cannot retrieve platform data from JIRA fields. Please make sure the fields are correctly filled.")
  }else{
    indices            = indices[!is.na(indices)]
  }
  
  values               = c("android" ,"ios"   ,  "web")
  contents             = output$contents[[indices]]
  pltfrms              = match(values,contents)
  pltfrm_ind           = !is.na(pltfrms)
  exp_pltfrm           = values[pltfrm_ind]
  lval                 = length(exp_pltfrm)
  
  
  pltfrm               = list()
  for(i in 1:lval){
    if(exp_pltfrm[i]  == values[1]){
      app_vrsn         = readline(prompt = "appvrsn-android: ")
      if(app_vrsn      == ""){
        pltfrm[i]      = paste0("  *",exp_pltfrm[i])
      }else{
        pltfrm[i]      = paste0("  *",exp_pltfrm[i], "[app_vrsn",app_vrsn,"]")
      }
    }else if (exp_pltfrm[i]  == values[2]){
      app_vrsn         = readline(prompt = "appvrsn-ios: ")
      if(app_vrsn      == ""){
        pltfrm[i]      = paste0("  *",exp_pltfrm[i])
      }else{
        pltfrm[i]      = paste0("  *",exp_pltfrm[i], "[app_vrsn",app_vrsn,"]")
      }
    }else {
      pltfrm[i]        =  paste0("  *",exp_pltfrm[i])
    }
    
    
  }
    
  return(pltfrm)  
}

#######################################################################################################################################################
#DFuntion to retirn android[app_vrsn('8.61.1','8.99.99')]


write_text_jira        = function(output =output, section = "all"){
  summary              = list(head ="TL;DR")
  experiment           = list(head = "Experiment", context = "Context",body = c("Hypothesis","Success Metrics","Health Metrics","Learning Metrics","Link"))
  setup                = list(head = "Set-up and treatment",  body = c("Platform(s)","Exposed groups", "Groups"))
  analysis             = list(head = "Analysis", body = c("Period analysed", "Results", "Recommendations", "Reasons"))
  
  sections             = list(summary = summary, experiment = experiment, setup = setup, analysis = analysis)                      
  
  
  secttmp              = c("TL;DR","Context", "Hypothesis", "Success Metrics","Health Metrics", "Learning Metrics" , "Exposed groups","Platform(s)",
                           "Manipulation","Links","Results", "Period analysed",  "Recommendation", "Reasons")
  
  
  #select JIRA field to write
  if(section == "all") {
    jiradat            = secttmp[-c(1,2,11,12,13,14)] 
  }else{
    jiradat            = (sections[[section]])$body
  }
  
  
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
  contents             = output$contents
  numbers              = output$numbers
  
  
  #Special definition for platform
  if("Platform(s)" %in% jiradat){
    p_ind                = match("Platform(s)",headers)
    if(!is.na(p_ind)){
      contents[[p_ind]]  = write_app_vrsn(output = output)
    }  
  }
  
  
  
  
  
  for(n in 1:n_prm ){
    h_ind              = indices[n]
    header             = paste("#### ",headers[h_ind])
    texts              = paste("  *", contents[[h_ind]])
    textsect           = append(header, texts)
    paragraph          = append(textsect,space)
    
    tmp       = "%s
    "
    for(r in 1:(numbers[[h_ind]] +n_spc) ) {
      cat(sprintf(tmp,paragraph[r]))
    }
    
  }
  
  #   #For link  
  #   if ("link" %in% tolower(jiradat)) {
  #     header            = paste0("####", "Link")
  #     text              = paste0(paste("  *", "[Link: Additional information on JIRA]"),paste0("(",output$link,")"))
  #     space             = c("  ", "  ")
  #     
  #     textsect           = append(header, text)
  #     paragraph          = append(textsect,space)
  #     
  #     
  #     tmp       = "%s
  # "
  #     for(r in 1:length(paragraph)) {
  #       cat(sprintf(tmp,paragraph[r]))
  #     }
  #   }
  #   
  
  }








#WRITE OUT REPORT
#####################################################################################################################################################################

# 1. All the other functions
# 2. Perhaps run the output =get_jira("AUR-XXXX") first


#Do common body writing for all and create an exception for link
#####################################################################################################################################################################
write_full_report    = function(output = output){
  write_summary_section()
}
# write_context_jira(output = output)
# write_header(section = "experiment")
# write_text_jira(output = output, section = "experiment")
# write_header(section = "setup")
# write_text_jira(output = output,section = "setup")
# 





#####################################################################################################################################################################

# 1. All the other functions
# 2. Perhaps run the output =get_jira("AUR-XXXX") first


#Do common body writing for all and create an exception for link
#####################################################################################################################################################################












######################################################################################################################################################
######################################################################################################################################################
######################################################################################################################################################
######################################################################################################################################################
write_text_jira        = function(output =output){
  # headers             = append(sections,"TL;DR")
  # contents            = append(values, " `parameters` ")
  # numbers             = append(n_values,1)
  secttmp              = c("TL;DR","Context", "Hypothesis", "Success Metrics","Health Metrics", "Learning Metrics" , "Exposed groups","Platform(s)",
                           "Manipulation","Links","Results", "Period analysed",  "Recommendation", "Reasons")
  #Jira sections; non jira section

  
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











