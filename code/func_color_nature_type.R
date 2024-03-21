
##' Define the colors of nature types
##'   add named colors to each category to keep the color consistent 

##' color from https://developers.google.com/earth-engine/datasets/catalog/USGS_NLCD_RELEASES_2021_REL_NLCD#bands
nature_type_list <- c("Greenspace",               # '#68ab5f'	(Deciduous Forest)
                      "Greenspace - Forest/Tree", # '#1c5f2c'	
                      "Greenspace - Shrub/scrub", # '#ccb879'	
                      "Greenspace - Grassland",   # '#dfdfc2'
                      "Greenspace - Farmland",    # '#ab6c28'
                      "Greenspace - Park",        # 
                      "Greenspace - Garden",      # '#aaff00'
                      "Greenspace - Green alley/Roadside green", 
                      "Greenspace - Green roof/wall",# '#79ffd2'              (Shrub-Forest)
                      
                      "Bluespace - Open water",      # '#466b9f'
                      "Bluespace - Wetland",         # '#b8d9eb'
                      "Bluespace - Sea",             # '#0000ff'
                      "Bluespace - Beach/coastline", # '#0000ff'
                      "Bluespace",
                      "Overall",                     # '#000000'
                      "Other"                        # '#000000'	
)
colors_nature_type <- c(
  '#68ab5f', # "Greenspace"
  '#1c5f2c', # "Greenspace - Forest/Tree"
  '#ccb879', # "Greenspace - Shrub/scrub"
  '#b2df8a', # "Greenspace - Grassland"
  '#ab6c28', # "Greenspace - Farmland"
  '#fb9a99', # "Greenspace - Park"
  '#aaff00', # "Greenspace - Garden" (forest disturbed before/at 2006)
  '#beaed4', # "Greenspace - Green alley/Roadside green"
  '#79ffd2', # "Greenspace - Green roof/wall"
  
  '#466b9f', # "Bluespace - Open water"
  '#b8d9eb', # "Bluespace - Wetland"
  '#0000ff', # "Bluespace - Sea"
  '#0000ff', # "Bluespace - Beach/coastline"
  '#08306b', # "Bluespace"
  '#000000', # "Overall"
  '#000000'  # "Other"
)

names(colors_nature_type) <- nature_type_list

func_color_nature_type <- function(df, column_name) {
  df <- as.data.frame(df)
  element_list <- unique(df[,column_name]); element_list
  colors_nature_type_viz<- colors_nature_type[element_list]; 
  
  ## return the nature types with assigned colors
  return(colors_nature_type_viz)
  
}