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
##'wkf_id = 1000009234
del_workflow<- (wkf_id,hostname,con){
  
#Find workflow, ensemble,run,liklihood, input, and dbfile information
  wkf_info <- as.data.frame(dplyr::tbl(con, "workflows") %>%
              filter(id %in% wkf_id))
  
  ens_info <- as.data.frame(dplyr::tbl(con, "ensembles") %>%
                         filter(workflow_id %in% wkf_id))
  
  run_info <- as.data.frame(dplyr::tbl(con, "runs") %>%
                            filter(ensemble_id %in% ens_info$id))
  
  likli_info <- as.data.frame(dplyr::tbl(con, "likelihoods") %>%
                            filter(run_id %in% run_info$id))
           
  input_info <- as.data.frame(dplyr::tbl(con, "inputs") %>%
                              filter(id %in% likli_info$input_id))
                              
  file_info <- as.data.frame(dplyr::tbl(con, "dbfiles") %>%
                             filter(container_id %in% input_info$id))
  
# Print Info into human readable format
  
 model <- tbl(con, "models")%>%
              filter(id %in% wkf_info$model_id) %>%
              select(model_name)
 
 type <- ens_info$runtype
 
 site <-  as.data.frame(tbltbl(con,"sites")%>%
                        filter(id %in% wkf_info$site_id)%>%
                        select_('sitename'))
 
 user_name <- as.data.frame(tbl)
 
 glue('You are deleting a workflow executed by {user_name}.')
 
 
 ## Pause for 15 sec so person can stop execution if needed
 PEcAn.logger::logger.warn("Deletion will begin in 15 seconds")
 profvis::pause(15)
 PEcAn.logger::logger.warn("Deletion of records and files begginning now")
 
 ## Delete From the bottom up
 
 if
 
 
}
