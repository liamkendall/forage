
bodysize2=function(x,taxa,type) {
  bee_IT <- bee_phy_mod <- bee_tax_mod <- hov_IT <- hov_tax_mod <- bee_sex <- hov_sex <- pollimetry_dataset <- NULL
  if(is.null(x$Species)==TRUE & is.null(x$Region)==FALSE){
    warning("Species have not been provided, these models will only consider fixed and random biogeographical effects.")
  }
  
  if(is.null(x$Species)==TRUE & is.null(x$Region)==TRUE){
    warning("Species and region have not been provided, these 
            models will only consider fixed effects.")
  }
  if(is.null(x$Species)==FALSE & is.null(x$Region)==TRUE){
    warning("Region has not been provided, these 
            models will only consider fixed and species-level random effects.")
  }
  
  data("pollimetry_dataset", envir = environment())
  check_sp <- x$Species %in% pollimetry_dataset$Species 
  if(any(check_sp==FALSE)){
    warning("Species are different from those used in model formulation, for those species, these models will only consider fixed and random biogeographical effects.")
  } 
  
  check_Region <- x$Region %in% c("Australasia","NorthAmerica","SouthAmerica","Europe")
  if(any(check_Region == FALSE)){
    warning("Only the following regions are supported currently; 'Australasia','NorthAmerica','SouthAmerica' and 'Europe'. Model will only consider fixed effects.")
  }
  if(type == "taxo" & taxa == "bee"){
    check_taxo <- x$Family %in% c("Andrenidae","Apidae","Colletidae","Halictidae","Melittidae","Megachilidae")
    if(any(check_taxo == FALSE)){
      stop("Family should be either 'Andrenidae','Apidae','Colletidae',Halictidae','Megachilidae','Melittidae'. If family = 'Stenotritidae' or is unknown, use type = 'sex' or 'IT'.")
    }
  }
  if(type == "taxo" & taxa == "bee" & is.null(x$Family)==TRUE){
    stop("Family not provided. If family is unknown, use type = 'IT'.")
  }
  if(type == "taxo" & taxa == "hov"){
    check_taxo <- x$Family %in% c("Eristalinae","Syrphinae")
    if(any(check_taxo == FALSE)){
      stop("Subfamily should be either 'Eristalinae' or 'Syrphinae'. If family is unknown, use type = 'IT'.")
    }
  }
  if(type %in% c("taxo", "phylo","sex")){
    if("Sex" %in% colnames(x)==FALSE) {
      stop("Sex should be either 'Female' and/or 'Male'. If sex is unknown, we recommend adding a data column denoting all specimens as females i.e df$Sex='Female'.")
    }
  }  
  check_Sex <- x$Sex %in% c("Female","Male")
  if(any(check_Sex==FALSE)){
    stop("Sex should be either 'Female' and/or 'Male'. If sex is unknown, we recommend adding a data column denoting all specimens as females i.e df$Sex='Female'.")
  }
  ##PHYLO ERRORS
  if(type=="phylo" & is.null(x$Species)==TRUE){
    stop("Species not provided. Use type 'taxo'.")
    
  }
  if(type=="phylo" & is.null(x$Region)==TRUE){
    warning("Region not provided. Model will only consider fixed and random species-level effects.")
  }
  ##HOV ERRORS
  check_hovregion <- x$Region %in% c("Europe","Australasia")
  if(any(check_Sex==FALSE)){
    warning("Only specimens from Europe and Australasia were used in model formulation. New regions will be modelled with group-level uncertainty in the predictions based on the variation of the existing levels.")
  }
  check_hovsex <- x$Sex %in% c("Female","Male")
  if(type=="taxo" & any(check_hovsex==FALSE)){
    stop("Sex should be either 'Female' and/or 'Male'. If sex is unknown, we recommend adding a data column denoting all specimens as females i.e df$Sex='Female'.")
  }
  check_hov_sex <- x$Sex %in% c("Female","Male")
  if(type=="sex" & any(check_hov_sex==FALSE)){
    stop("Sex should be either 'Female' and/or 'Male'. If sex is unknown, we recommend adding a data column denoting all specimens as females i.e df$Sex='Female'.")
  }
  check_hovsub <- x$Subfamily %in% c("Eristalinae","Syrphinae")
  if(type=="taxo" & any(check_hovsub==FALSE)){
    stop("Only Eristalinae and Syrphinae supported currently. Use type = 'IT'.")
  } 
  if(taxa=="hov" & is.null(x$Region)==TRUE){
    warning("Region not found. Only fixed effects will be inferred.") 
  }
  
  ##Incorrect type and taxa combos
  if(type=="phylo" & taxa =="hov"){
    stop("Bad combination: No phylogenetic model implemented for hoverflies yet!")
    
    #FIT MODELS
  } else if(requireNamespace('pollimetrydata')) {
    if(type=="taxo" & taxa=="bee"){
      mod=pollimetrydata::bee_tax_mod
    }
    if(type=="phylo" & taxa=="bee"){  
      mod=pollimetrydata::bee_phy_mod
    }
    if(type=="sex" & taxa=="bee"){  
      mod=pollimetrydata::bee_sex
    }
    if(type=="IT" & taxa=="bee"){  
      mod=pollimetrydata::bee_IT
    }
    if(type=="taxo" & taxa=="hov"){  
      mod=pollimetrydata::hov_tax_mod
    }
    if(type=="sex" & taxa=="hov"){  
      mod=pollimetrydata::hov_sex
    }
    if(type=="IT" & taxa=="hov"){  
      mod=pollimetrydata::hov_IT
    }
  }else {
    ###BEES
    if(type=="taxo" & taxa=="bee"){
      if(system.file("pollimetrydata")==""){
        repmis::source_data("https://github.com/liamkendall/pollimetrydata/raw/master/data/bee_tax_mod.rdata", envir = environment())
        mod=bee_tax_mod}
    }
  }
  
  if(type=="phylo" & taxa=="bee"){
    if(system.file("pollimetrydata")==""){
      repmis::source_data("https://github.com/liamkendall/pollimetrydata/raw/master/data/bee_phy_mod.rdata", envir = environment())
      mod=bee_phy_mod
    }
  }
  if(type=="sex" & taxa=="bee"){
    if(system.file("pollimetrydata")==""){
      repmis::source_data("https://github.com/liamkendall/pollimetrydata/raw/master/data/bee_sex_mod.rdata", envir = environment())
      mod=bee_sex
    }
  }
  if(type=="IT" & taxa=="bee"){
    if(system.file("pollimetrydata")==""){
      repmis::source_data("https://github.com/liamkendall/pollimetrydata/raw/master/data/bee_IT.rdata", envir = environment())
      mod=bee_IT}
  }
  
  ###HOVERFLIES
  if(type=="taxo" & taxa=="hov"){
    if(system.file("pollimetrydata")==""){
      repmis::source_data("https://github.com/liamkendall/pollimetrydata/raw/master/data/hov_tax_mod.rdata", envir = environment())
      mod=hov_tax_mod}
  }
  if(type=="sex" & taxa=="hov"){
    if(system.file("pollimetrydata")==""){
      repmis::source_data("https://github.com/liamkendall/pollimetrydata/raw/master/data/hov_sex_mod.rdata", envir = environment())
      mod=hov_sex}
  }
  
  if(type=="IT" & taxa=="hov"){
    if(system.file("pollimetrydata")==""){
      repmis::source_data("https://github.com/liamkendall/pollimetrydata/raw/master/data/hov_IT.rdata", envir = environment())
      mod=hov_IT
    }
  }
  #More tests can be implemented e.g. warn depreciated columns (e.g. if Family is provided with type Phylo, explain that it will be depreciated)
  ##OUTPUT
  out <- predict(object=mod,
                 newdata=x,
                 allow_new_levels=TRUE,
             #    transform=exp,
                 probs = c(0.05, 0.95))
  colnames(out)=c("Est.Weight","SE","CI_Lower","CI_Upper")
  out<-cbind(x,out)
  out
}
