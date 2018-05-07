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
##'wkf_id = 1000000679
del_workflow<- function(wkf_id,hostname,con){
  
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
 
 type <- if(exists("ens_info") && !all(is.na(ens_info))){
                                collapse(ens_info$runtype, sep = ",")
                                                          }else
                                                           { "No Type" 
                                                             }
                                                        
 site <- PEcAn.DB::query.site(wkf_info$site_id,con)
 
 user_name <- if(is.na(wkf_info$user_id)){
                  NA
                  }else{
                    tbl(con,"users") %>%
                      filter(id %in% wkf_info$user_id) %>%
                      pull(name)
                  }

 
glue::glue('You are deleting a workflow created by {user_name} with the following information: ',
            'Model = {model_name} ',
            'Site = {site$sitename} ',
            'Start Date = {wkf_info$start_date}',
            'End Date = {wkf_info$end_date}',
            'Date Created = {wkf_info$created_at} ',
            'Date Updated = {wkf_info$updated_at}',
            'Type = {type}',
            'Host = {wkf_info$hostname}',
            'Files deleted are located here',
            'Workflow Folder Location = {wkf_info$folder}',
            .sep ="\n"
            )

 ## Pause for 15 sec so person can read info and stop execution if needed
 PEcAn.logger::logger.warn("Deletion will begin in 15 seconds")
 profvis::pause(15)
 PEcAn.logger::logger.warn("Deletion of records and files begginning now")
 
 ## Delete
 if(exists("run_info"))
 for (i in seq_along(run_info$id)){
   PEcAn.DB::db.query(query =paste0("DELETE FROM likelihoods where run_id =",run_info$id[i]),
            con)
 }
 
 for (i in seq_along(ens_info$id)){
   PEcAn.DB::db.query(query =paste0("DELETE FROM runs where ensemble_id =",ens_info$id[i]),
            con)
   PEcAn.DB::db.query(query =paste0("DELETE FROM ensembles where id =",ens_info$id[i]),
            con)
 }
 
 for (i in seq_along(wkf_info$id)){
   PEcAn.DB::db.query(query =paste0("DELETE FROM workflows where id =",wkf_info$id[i]),
            con)
   
 }

 
 for (i in seq_along(wkf_info$folder)){
   unlink(wkf_info$folder, recursive = TRUE)
 }
 
 PEcAn.logger::logger.warn("Deletion Complete. All records and files were deleted")
}
