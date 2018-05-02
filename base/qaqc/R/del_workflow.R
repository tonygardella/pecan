##' @export del_workflows
##' @author Tony Gardella
##' 
##' @param workflow_id  Id number of workflow you want to delete 
##' @param hostname The name of the machine you wish to delete files from 
##' @param con connection to the bety database
##' 
##' @description This is a function that takes in workflow_id and the name of the server that hosts BETY, 
##' and then deletes all files and records associated with it.
##'
##'
##'wkf_id = 1000004781
del_workflow<- (wkf_id,hostname,con){
  
#Find workflow, ensemble,run,likelihood, input, and dbfile information
  wkf_info <- as.data.frame(dplyr::tbl(con, "workflows") %>%
              filter(id %in% wkf_id))
  
  if(exists("wkf_info") && !all(is.na(wkf_info)) ){
  ens_info <- as.data.frame(dplyr::tbl(con, "ensembles") %>%
                         filter(workflow_id %in% wkf_id))
  }
  
  if(exists("ens_info") && !all(is.na(ens_info))){
  run_info <- as.data.frame(dplyr::tbl(con, "runs") %>%
                            filter(ensemble_id %in% ens_info$id))
  }
  
  if(exists("run_info") && !all(is.na(run_info))){
  likeli_info <- as.data.frame(dplyr::tbl(con, "likelihoods") %>%
                            filter(run_id %in% run_info$id))
  }
  
  if(exists("likeli_info") && !all(is.na(likeli_info))){       
  input_info <- as.data.frame(dplyr::tbl(con, "inputs") %>%
                              filter(id %in% likeli_info$input_id))
  }
   
  if(exists("input_info") && !all(is.na(input_info))){                         
  file_info <- as.data.frame(dplyr::tbl(con, "dbfiles") %>%
                             filter(container_id %in% input_info$id))
  }
  
# Print Info into human readable format
  
 model_name <- tbl(con, "models")%>%
              filter(id %in% wkf_info$model_id) %>%
              pull(model_name)
 
 type <- pull(ens_info = runtype)
 
 site <- PEcAn.DB::query.site(wkf_info$site_id,con)
 
 user_name <- tbl(con,"users") %>%
                filter(id %in% wkf_info$user_id) %>%
                pull()
 
glue::glue('You are deleting a workflow created by {user_name} with the following information: ',
            'Model = {model_name} ',
            'Site = {site$sitename} ',
            'Start Date = {wkf_info$start_date}',
            'End Date = {wkf_info$end_date}',
            .sep ="\n"
            )


glue('My name is {name},',
     ' my age next year is {age + 1},',
     ' my anniversary is {format(anniversary, "%A, %B %d, %Y")}.',
     name = "Joe",
     age = 40,
     anniversary = as.Date("2001-10-12"))

 ## Pause for 15 sec so person can stop execution if needed
 PEcAn.logger::logger.warn("Deletion will begin in 15 seconds")
 profvis::pause(15)
 PEcAn.logger::logger.warn("Deletion of records and files begginning now")
 
 ## Delete From the bottom up
 
 if(li)
 
 
}
