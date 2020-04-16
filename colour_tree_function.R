#' plotting tree with background sequences labelled
#' 
#' Edit of mcc_tree_plot allowing non-region tips to be coloured by continent.
#' requires nexus input tree, most recent sample date and dictionary converting second element of 
#' sequence names to continent.
#' 
#' @param nexfn Path to nexus file containing annotated MCC tree (output of treeannotator2)
#' @param mostRecentSampleDate A character string containign the date of the most recent sample in the form 2020-03-17
#' @param country_dictionary data dictionary to convert country names to world bank continents
#' @param regionDemes A character vector of deme names which will be coloured red 
#' @param ofn Output file name of figure 
#' @return A ggtree object which can be customized further
#' @export 

mcc_col_tree_plot <- function(nexfn, mostRecentSampleDate, country_dictionary, regionDemes = c( 'Il', 'Ih', 'E' ), ofn = 'mcc.png' ){
  country_dict <- utils::read.table(country_dictionary, header = TRUE)
  tr = treeio::read.beast ( nexfn )
  trd = treeio::get.tree( tr )
 
  btr = ggtree::ggtree(tr, mrsd=mostRecentSampleDate, ladderize=TRUE) + ggtree::geom_range(range='height_0.95_HPD', color='steelblue', branch.length="height", alpha=.4, size=2) + ggtree::theme_tree2() 
  
  
  tipdeme <- sapply( strsplit( trd$tip.label, '_' ), function(x) tail(x,1))
  tipcols <- sapply( strsplit( trd$tip.label, '/' ), function(x) x[[2]])
  
  ## recode tipcols w/ dictionary
  tipcols <- country_dict$continent[match(tipcols, country_dict$name)]
  
  continent_names <- c("Africa", "Americas", "Asia", "Europe", "Oceania", "Region")
  levels(tipcols) <- continent_names
  col_pal <- c("#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#a65628", "#e41a1c")
  names(col_pal) <- continent_names
  
  tipdata <- data.frame( 
    taxa =trd$tip.label
    , region =  tipdeme %in% regionDemes
    , Location = tipcols
    , stringsAsFactors = F
  )
  tipdata[tipdata$region==TRUE,]$Location <- "Region" # can we get location from the yaml and pass to the R code?
  
  btr <- btr %<+% tipdata
  btr = btr + 
    ggtree::geom_tippoint( aes(color = Location), size = 2) +
    ggtree::theme_tree2( legend.position = "none" ) +
    ggplot2::scale_color_manual(values = col_pal, 
                                guide = ggplot2::guide_legend(title.position="top",
                                                              title.hjust = 0.5,
                                                              nrow=2,
                                                              byrow=TRUE
                                                              ))+
    ggplot2::theme(legend.position = "bottom", legend.title = ggplot2::element_text(face="bold", size=10))
  
  ggsave( btr, file= "mcc2.png" , width = 4, height=7)
  btr
}

# mcc_col_tree_plot("./Luxembourg/mcc.nex", "2020-03-18", "country_dict.txt")
