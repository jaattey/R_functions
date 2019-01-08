#GET MY JIRA 
#####################################################################################################################################################################
#1. Function to scrap content from web
#2. Inputs: experiment number and log-in details
#3. Outputs: shitloads of stuff


#####################################################################################################################################################################

get_jira              = function(experiment = "AUR-7024"){
  
  username            =  readline(prompt="username: ")  
  password            =  readline(prompt="password: ")
  
  ###Raw tml document
  urllink             = paste0("https://jira.corp.ebay.com/browse/",experiment)
  get_cred            = GET(urllink, authenticate(user = username,password = password))
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
  sections_n          = sub("Manipulation", "Groups", sections)
  team_ind            = match("Team", sections_n)
  team                = values[[team_ind]]
  
  
  
  #Obtaining the full name
  metaflds            = raw_html %>% html_nodes("meta")
  nameindex           = which(metaflds %like% "ajs-remote-user-fullname")
  nameline            = metaflds[nameindex]
  names               = str_extract_all(gsub(".*(content=)(.*)>\n","\\2",nameline), "[A-Z][a-z]{0,100}")[[1]]
  fullname            = paste(names[1],names[2], sep = " ")
  
  email               = paste0(tolower(username), "@ebay.com")


  
  
  
  
  ###Check if partenrs filled the right tickets (Experiments)
  if(values[[1]] != "Experiment"){
    warning(paste(experiment,"is not an experiment canvas. Please fill it in usign a proper experiment canvas."))
  }else{
    
    output            = list(report_title=rmd_title, headers=sections_n, contents=values, numbers=n_values, link= urllink, name=fullname, email=email, team = team)
    return(output)
  }
  
  
  
}








#WRITE MY HEADERS (TO BE USED LATER)
#####################################################################################################################################################################
# 1. Input a list with head options: "summary" , "experiment", "setup" or "analysis"
# 2. Write up main header 


#Do common body writing for all and create an exception for link
#####################################################################################################################################################################

write_header           = function(section = "summary"){
  summary              = list(head ="TL;DR")
  experiment           = list(head = "Experiment", context = "Context",body = c("Hypothesis","Success Metrics","Health Metrics","Learning Metrics","Link"))
  setup                = list(head = "Set-up and treatment",pltfrm = "Platform(s)", body = c("Exposed groups", "Groups"))
  analysis             = list(head = "Analysis", body = c("Period analysed", "Results", "Recommendations", "Reasons"))
  
  sections             = list(summary = summary, experiment = experiment, setup = setup, analysis = analysis)                      
  
  head                 = (sections[[section]])$head
  space                = c("  ")
  n_spc                = length(space)
  sect                 = paste0("###",head)
  paragraph            = append(sect,space)
  
  n_ind                = length(paragraph)
  
  tmp                  = "%s
  "
  
  for(r in 1: n_ind) {
    cat(sprintf(tmp,paragraph[r]))
  }
  
}








#WRITE CONTEXT: TO BE USED IN CONJUNCTION WITH WRITE_HEADER AND WRITE JIRA TEST
#####################################################################################################################################################################




#####################################################################################################################################################################

#Special treatment for context==> no bullet
write_context_jira     = function(output = output){
  secttmp              = c("TL;DR","Context", "Hypothesis", "Success Metrics","Health Metrics", "Learning Metrics" , "Exposed groups","Platform(s)",
                           "Manipulation","Links","Results", "Period analysed",  "Recommendation", "Reasons")
  #Jira sections; non jira section
  context              = secttmp[2]
  indices              = match(context,output$headers)
  
  if (sum(!is.na(indices)) == 0) {
    stop("Cannot retrieve data from JIRA fields. Please make sure the Context field is correctly filled.")
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
    
    texts              = contents[[h_ind]]
    
    textsect           = append(header, texts)
    paragraph          = append(textsect,space)
    
    tmp       = "%s
    "
    for(r in 1:(numbers[[h_ind]] +n_spc) ) {
      cat(sprintf(tmp,paragraph[r]))
    }  
  }
  }










#WRITE TEXT FOR SUMMARY
#####################################################################################################################################################################
#Input: user defined
#Input must be only one sentence, no matter how long





#####################################################################################################################################################################
write_summary_section   = function(){
  text                  = readline(prompt="TL;DR: ") 
  space                 = c(" "," ", " ")
  
  header                = paste0("###", "TL;DR")
  textsect              = append(header,text)
  paragraph             = append(textsect, space)
  
  if(trimws(text) != ""){
    tmp                 = "%s
"
    for(r in 1:length(paragraph)) {
      cat(sprintf(tmp,paragraph[r]))
    }
  }else{
    warning("Please fill-in the summary")
  }
  
  
  
  
}








#NEW PLATFORM INPUT
#####################################################################################################################################################################
#Inputs: Output from GET_JIRA function
#User inputed values




#####################################################################################################################################################################
write_app_vrsn         = function(output = output){
  setup                = list(head = "Set-up and treatment",pltfrm = "Platform(s)", body = c("Exposed groups", "Groups"))
  jirapltfrm           = setup$pltfrm
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
        pltfrm[i]      = exp_pltfrm[i]
      }else{
        pltfrm[i]      = paste(exp_pltfrm[i], "[app_vrsn",app_vrsn,"]")
      }
    }else if (exp_pltfrm[i]  == values[2]){
      app_vrsn         = readline(prompt = "appvrsn-ios: ")
      if(app_vrsn      == ""){
        pltfrm[i]      = exp_pltfrm[i]
      }else{
        pltfrm[i]      = paste(exp_pltfrm[i], "[app_vrsn",app_vrsn,"]")
      }
    }else {
      pltfrm[i]        =  exp_pltfrm[i]
    }
    
    
  }
  pltfrm              = unlist(pltfrm)
  return(pltfrm)  
}














#WRITE SECTIONS BODIES
#####################################################################################################################################################################

# 1. Input a list with head options: choose between c("all", "summary" , "experiment", "setup" "analysis")
# 2. Write up main header 


#Do common body writing for all and create an exception for link
#####################################################################################################################################################################
write_text_jira        = function(output =output, section = "experiment"){
  summary              = list(head ="TL;DR")
  experiment           = list(head = "Experiment", context = "Context",body = c("Context" ,"Hypothesis","Success Metrics","Health Metrics","Learning Metrics","Link"))
  setup                = list(head = "Set-up and treatment",  body = c("Platform(s)","Exposed groups", "Groups"))
  analysis             = list(head = "Analysis", body = c("Period analysed", "Results", "Recommendations", "Reasons"))
  
  sections             = list(summary = summary, experiment = experiment, setup = setup, analysis = analysis)                      
  
  
  secttmp              = c("TL;DR","Context", "Hypothesis", "Success Metrics","Health Metrics", "Learning Metrics" , "Exposed groups","Platform(s)",
                           "Manipulation","Links","Results", "Period analysed",  "Recommendation", "Reasons")
  
  
  #select JIRA field to write
  if(section == "all") {
    jiradat            = secttmp[-c(1,11,12,13,14)] 
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
  n_spc                = length(space)
  
  headers              = (output$headers)[indices]
  contents             = (output$contents)[indices]
  numbers_o            = (output$numbers)[indices]
  mainhead             = (sections[[section]])$head

  
  #Special definition for platform
  if("Platform(s)" %in% jiradat){
    p_ind                = match("Platform(s)",headers)
    if(!is.na(p_ind)){
      contents[[p_ind]]  = write_app_vrsn(output = output)
    }
  }

  
  #Special treatment for context
  
  
  

  header               = append(paste0("###",mainhead), paste0("####",headers))
  n_prm                = length(header)
  text                 = append(list(c(" ")), contents)
  numbers              = append(list(c(1)),numbers_o)
  
  
  
  
  
  
  for(n in 1:n_prm ){
    if(trimws(text[[n]]) == "" | header[[n]] == "####Context"){
      texts            = text[[n]]
    }else{
      texts            = paste("  *", text[[n]])      
    }
    textsect           = append(header[n], texts)
    paragraph          = append(textsect,space)
    
    tmp       = "%s
"
    for(r in 1:(numbers[[n]] +n_spc) ) {
      cat(sprintf(tmp,paragraph[r]))
    }
    
  }
  
  #For link
  
  if ("link" %in% tolower(jiradat)) {
    header            = paste0("####", "Link")
    text              = paste0(paste("  *", "[Link: Additional information on JIRA]"),paste0("(",output$link,")"))
    space             = c("  ", "  ")

    textsect           = append(header, text)
    paragraph          = append(textsect,space)


    tmp       = "%s
"
  for(r in 1:length(paragraph)) {
      cat(sprintf(tmp,paragraph[r]))
    }
  }


  }








#WRITE THE FINAL SECTION
#####################################################################################################################################################################
#1. SPLIT LINES WITH A SEMICOLON
#2. IF NOTHING FILLED-IN THEN SKIP
#3. IF FILLED THEN GO AHEAD AND PUT IT THERE

#4. VARIABLES TO THINK OF [numbers, content, headers] 

#####################################################################################################################################################################
write_results          = function(){
  analysis             = list(head = "Analysis", body = c("Period analysed", "Results", "Recommendations", "Reasons"))
  head                 = analysis$head
  body                 = analysis$body
  
  period_analysed      = readline(prompt = "Period analysed:  ")
  results              = readline(prompt = "Results-please separate distinct results by ';' :   ")
  recommendations      = readline(prompt = "Recommendations:  ")
  reason               = readline(prompt = "Reasons-please separate distinct results by ';' :  ")
  
  inputs               = list(period_analysed, results, recommendations, reason)
  indices              = inputs != ""
  n_val                = sum(indices)
  if(n_val == 0){
    warning("No input for analysis")
    return("")
  }else{
      
   
  
  
  headers              = body[indices]
  replies              = inputs[indices]
 
  
  pattern              = ";"
  
  values               = list() 
  numbers              = list()
  texts                = list()
  for(i in 1:n_val){
    values[i]          = strsplit(as.character(replies[i]),  pattern)
    numbers[i]         = length(values[[i]])
    texts[[i]]         = paste("   *", values[[i]])     
  }
  
  
  header               = append(paste0("###",head), paste0("####",headers))
  n_prm                = length(header)
  text                 = append(list(c(" ")), texts)
  
  space                = c("  ", "  ")
  n_spc                = length(space)
  number               = append(list(1),numbers)
  
  
  
  
  for(n in 1:n_prm ){
    textsect           = append(header[n], text[[n]])
    paragraph          = append(textsect,space)
    
    tmp       = "%s
"
    for(r in 1:(number[[n]] +n_spc) ) {
      cat(sprintf(tmp,paragraph[r]))
    }
    
  }
  }  
  
}

















#WRITE OUT REPORT
#####################################################################################################################################################################

# 1. All the other functions
# 2. Perhaps run the output =get_jira("AUR-XXXX") first


#Do common body writing for all and create an exception for link
#####################################################################################################################################################################
write_full_report    = function(output = output){
  write_summary_section()
  write_text_jira(output, section = "experiment")
  write_text_jira(output, section = "setup")
  write_results()
}
# write_context_jira(output = output)
# write_header(section = "experiment")
# write_text_jira(output = output, section = "experiment")
# write_header(section = "setup")
# write_text_jira(output = output,section = "setup")
# 



