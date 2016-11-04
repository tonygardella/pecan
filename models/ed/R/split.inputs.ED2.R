#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

## split clim file into smaller time units to use in KF
##' @title split.inputs.ED2
##' @name  split.inputs.ED2
##' @author Tony Gardella
##' 
##' @param settings
##' @description Returns the paths listed under inputs so that SDA workflow knows where to look for met
##' @return Returns settings input paths
##' @export
split.inputs.ED2 <- function(settings) {
  
  return(settings$run$inputs)
  
} # split.inputs.ED2
