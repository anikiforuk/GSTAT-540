# use of ggplot,install
install.packages("tidyverse", dependencies = TRUE)
# re-load
library(tidyverse)
#call for the ggplot function
ggplot2::ggplot()

# list of ggplot functions

%>% - Syntactic sugar for easily piping the result of one function into another.
ggplot2::ggplot() - Base function for using ggplot2. Lays out the invisible 'canvas' for graphing.
ggplot2::geom_point() - Geom function for drawing data points in scatterplots.
ggplot2::geom_smooth() - Geom function for drawing fitted lines in trend charts.
ggplot2::geom_bar() - Geom function for drawing bars in bar graphs.
ggplot2::geom_density() - Geom function for drawing density plots.
ggplot2::geom_boxplot() - Geom function for drawing box plots.
ggplot2::geom_violin() - Geom function for violin plots.
ggplot2::facet_wrap() - ggplot2 function for separating factor levels into multiple graphs.
ggplot2::xlab() - Manually set x-axis label.
ggplot2::ylab() - Manually set y-axis label.
ggplot2::scale_y_reverse() - Reverse y-axis.
ggplot2::coord_flip() - Flip x and y axes.
ggplot2::coord_polar() - Use polar axes.
dplyr::group_by() - Commonly used with summarize() to derive summarized values for multiple rows sharing certain attribues, e.g. Average fuel consumption rates of different car manufacturer.
dplyr::summarize() - Commonly used with summarize() to derive summarized values for multiple rows sharing certain attribues.



# load the car data, data frame
ggplot2::mpg

#view mpg data
view(mpg)

# use GGplot to illustrate the relationship between displ ( a car's engine size)
# and hwy ( a car's  fuel efficency on the highway)
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy))

# ggplot bare bones graphing template
ggplot(data = <DATA>) + <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

# adding a third variable to the plot by using and AESTHETIC, think of it like a data LEVEL

# for example we can sort the data by the class (level) of car and color
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = class))

# we can sort the data by the class (level) of car and size
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, size = class))
## Warning: Using size for a discrete variable is not advised. Data should have a natural order

# use manual color scale
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy), color = "blue")
# notice in manual the color is out side of the aes function


# the power of ggplot is in you ability to layer functions (layers) one over the other 

#Aesthetics: graphic elements mapped to the data by aes(). x-y position, color, size, shape, linetype

#Geometries: are actual graphic elements used to plot data used by geom_.
apropos("^geom_")

[1] "geom_abline"     "geom_area"       "geom_bar"        "geom_bin2d"      "geom_blank"      "geom_boxplot"   
[7] "geom_col"        "geom_contour"    "geom_count"      "geom_crossbar"   "geom_curve"      "geom_density"   
[13] "geom_density_2d" "geom_density2d"  "geom_dotplot"    "geom_errorbar"   "geom_errorbarh"  "geom_freqpoly"  
[19] "geom_hex"        "geom_histogram"  "geom_hline"      "geom_jitter"     "geom_label"      "geom_line"      
[25] "geom_linerange"  "geom_map"        "geom_path"       "geom_point"      "geom_pointrange" "geom_polygon"   
[31] "geom_qq"         "geom_qq_line"    "geom_quantile"   "geom_raster"     "geom_rect"       "geom_ribbon"    
[37] "geom_rug"        "geom_segment"    "geom_sf"         "geom_sf_label"   "geom_sf_text"    "geom_smooth"    
[43] "geom_spoke"      "geom_step"       "geom_text"       "geom_tile"       "geom_violin"     "geom_vline" 

#Statistics: simple and powerful way to summarize data and present calculated STATS on the plot. ex: regression, spline
# or density curve
apropos("^stat_")
[1] "stat_bin"            "stat_bin_2d"         "stat_bin_hex"        "stat_bin2d"          "stat_binhex"        
[6] "stat_boxplot"        "stat_contour"        "stat_count"          "stat_density"        "stat_density_2d"    
[11] "stat_density2d"      "stat_ecdf"           "stat_ellipse"        "stat_function"       "stat_identity"      
[16] "stat_qq"             "stat_qq_line"        "stat_quantile"       "stat_sf"             "stat_sf_coordinates"
[21] "stat_smooth"         "stat_spoke"          "stat_sum"            "stat_summary"        "stat_summary_2d"    
[26] "stat_summary_bin"    "stat_summary_hex"    "stat_summary2d"      "stat_unique"         "stat_ydensity"      

# Scale: allows for log transformation or other transformation of data and scale. 