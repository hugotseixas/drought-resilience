## ------------------------------------------------------------------------- ##
####* ------------------------------- HEADER ---------------------------- *####
## ------------------------------------------------------------------------- ##
##
#### Description ####
##
## Script name:   
##
## Description:    
##                 
##                
##                 
##                
##                 
##                
##                
##
## Author:        Hugo Tameirao Seixas
## Contact:       hugo.seixas@alumni.usp.br / tameirao.hugo@gmail.com
##
## Date created:  
## Last update:   
## Last tested:   
##
## Copyright (c) Hugo Tameirao Seixas, 2020
##
## ------------------------------------------------------------------------- ##
##
## Notes:           
##                  
##                               
##
## ------------------------------------------------------------------------- ##
##
#### Libraries ####
##

##
## ------------------------------------------------------------------------- ##
##
#### Options ####
##
options(scipen = 6, digits = 4) # View outputs in non-scientific notation
##
## ------------------------------------------------------------------------- ##
####* ------------------------------- CODE ------------------------------ *####
## ------------------------------------------------------------------------- ##

ylist <- seq(from = 2003, to = 2012)

dlist <- dir('./data/noah_out/', pattern = 'scen')

for (d in seq_along(dlist)) {
  
  print(dlist[d])
  
  for (y in seq_along(ylist)) {
    
    print(ylist[y])
    
    system(paste(
      'cdo -daysum -selvar,GPP,LAI,LH,RAINRATE,TRAD -cat ../', 
      dlist[d],
      '/',
      ylist[y],
      '* ../merged/',
      dlist[d], 
      '_', 
      ylist[y], 
      '.nc', 
      sep = ''
    ))
    
  }
  
}

for (d in seq_along(dlist)) {
  
  print(dlist[d])
  
  system(paste(
    'cdo -cat ../merged/', 
    dlist[d], 
    '* ../merged/', 
    dlist[d], 
    '.nc', 
    sep = ''
  ))
  
}


## ------------------------------------------------------------------------- ##
####* ------------------------------- END ------------------------------- *####
## ------------------------------------------------------------------------- ##