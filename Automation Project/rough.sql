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



