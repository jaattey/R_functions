
get_jira_2_text      = function(output){
  # headers             = append(sections,"TL;DR")
  # contents            = append(values, " `parameters` ")
  # numbers             = append(n_values,1)
  secttmp              = c("TL;DR","Context", "Hypothesis", "Success Metrics","Health Metrics", "Learning Metrics" , "Exposed groups","Platform(s)",
                           "Manipulation","Links","Results", "Period analysed",  "Recommendation", "Reasons")
  #Jira sections; non jira section
  nobullet             = secttmp[c(1,2)]
  bullet               = secttmp[-c(1,2)]
  platforms            = secttmp[9]
  
  #jira vs non jira
  analdat              = secttmp[c(1,11,12,13,14)]
  jiradat              = secttmp[-c(1,11,12,13,14)]
  
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
    h_ind             = indices[n]
    n_line            = numbers[[h_ind]] + 2
    header            = paste0("###",headers[h_ind])
    
    texts             = ifelse(header %in% nobullet,  contents[[h_ind]],  paste("  *", contents[[h_ind]]))
    
    textsect          = append(header, texts)
    paragraph         = append(textsect,space)
    
    tmp       = "%s
"
    for(r in 1:(numbers[[h_ind]] +n_spc) ) {
      cat(sprintf(tmp,paragraph[r]))
    }
    
  }
  
  
}
# headers             = append(sections,"TL;DR")
# contents            = append(values, " `parameters` ")
# numbers             = append(n_values,1)
secttmp              = c("TL;DR","Context", "Hypothesis", "Success Metrics","Health Metrics", "Learning Metrics" , "Exposed groups","Platform(s)",
                         "Manipulation","Links","Results", "Period analysed",  "Recommendation", "Reasons")
#Jira sections; non jira section
nobullet             = secttmp[c(1,2)]
bullet               = secttmp[-c(1,2)]
platforms            = secttmp[9]

#jira vs non jira
analdat              = secttmp[c(1,11,12,13,14)]
jiradat              = secttmp[-c(1,11,12,13,14)]

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
  h_ind             = indices[n]
  n_line            = numbers[[h_ind]] + 2
  header            = paste0("###",headers[h_ind])

  texts             = paste("  *", contents[[h_ind]])
  
  textsect          = append(header, texts)
  paragraph         = append(textsect,space)
  
  tmp       = "%s
"
  for(r in 1:(numbers[[h_ind]] +n_spc) ) {
     cat(sprintf(tmp,paragraph[r]))
  }
  
}






#######################################################################################################################################################



#######################################################################################################################################################

preamble            = ordered[c(1:3)]
metrics             = ordered[c(7:9)]
treatment           = ordered[c(10,11)]
links               = "link"#inputs
results             = ordered[c(4,13,14)]

######################################################################################################################################################
#first check if these chuncs are in sect JIRA
# If they are then pick their contents
#match(preamble,contents)
#print chunck consisting for sections and  others
#Only print when they exists
######################################################################################################################################################
indices              = match(preamble, headers)
indices              = indices[!is.na(indices)]
space                = c("  ", "  ","  ", "  ")
n_prm                = length(indices)               
showpar              = list()
for (n in 1:n_prm){
  h_ind                = indices[n]
  n_line               = numbers[[h_ind]] + 4
  header               = paste0("###",headers[h_ind])
  texts                = paste("  *", contents[[h_ind]])
  
  textsect             = append(header, texts)
  paragraph            = append(textsect,space)
  
  tmp       = "%s
"
  for(r in 1:(numbers[[h_ind]] +2) ) {
    cat(sprintf(tmp,paragraph[r]))
  }
  
}




######################################################################################################################################################
indices              = match(metrics, headers)
indices              = indices[!is.na(indices)]
space                = c("  ", "  ","  ", "  ")
n_prm                = length(indices)               

for (n in 1:n_prm){
  h_ind                = indices[n]
  n_line               = numbers[[h_ind]] + 2
  header               = paste0("###",headers[h_ind])
  texts                = paste("  *", contents[[h_ind]])
  
  textsect             = append(header, texts)
  paragraph            = append(textsect,space)
  
  tmp       = "%s
"
  for(r in 1:(numbers[[h_ind]] +2) ) {
    showpar   = cat(sprintf(tmp,paragraph[r]))
  }
  
}

######################################################################################################################################################

indices              = match(treatment, headers)
indices              = indices[!is.na(indices)]
space                = c("  ", "  ","  ", "  ")
n_prm                = length(indices)               

for (n in 1:n_prm){
  h_ind                = indices[n]
  n_line               = numbers[[h_ind]] + 2
  header               = paste0("###",headers[h_ind])
  texts                = paste("  *", contents[[h_ind]])
  
  textsect             = append(header, texts)
  paragraph            = append(textsect,space)
  
  tmp       = "%s
"
  for(r in 1:(numbers[[h_ind]] +2) ) {
    showpar   = cat(sprintf(tmp,paragraph[r]))
  }
  
}



