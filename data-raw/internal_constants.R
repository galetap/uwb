## Internal_constants

# General size for theme
.tsize <- 10

# uwb_val ---------------
.uwb_vals <- list(
  # Font
  # font = "Franklin Gothic",
  font = "Franklin Gothic",
  # font = c("Franklin Gothic",
  #          "Franklin Gothic Book",
  #          "sans"),
  # Text sizes
  tsize = 10, # main text size; influences theme and labsizes
  labsize = (1.25 * 10) / 2.54, # size of text labels in plots
  chrnum = 30, # number of characters before line break in axis labels
  lineheight_tit = 1, # space between lines in titlea and subtitle labels
  lineheight = 0.75, # space between lines in axis labels
  # Position of text labels
  lim_single_pos = 5, # Position of text label inside vs. outside bar in barplots
  lim_stack_no = 5, # Whether text labels is printed in a stacked barplot
  # Line and point sizes
  axissize = 0.25, # Width of axes
  linesize = 1, # Width of lines in a line plot
  pointsize = 8, # size of circles in lollipop, scatter and trend plots
  # Line colors (except palettes that go into aes)
  barcol = "white", # Outline of bars
  gridcol = "gray85", # Plot grid
  lollistick = "gray35",
  axiscol = 'grey35', # Color for axis is muted because values are labelled
  # color of N sizes: very small, small, enough
  c_nsize1 = "#c73a3a",
  c_nsize2 = "#944e4e",
  c_nsize3 = "grey35"
)


# University colors ------------------------------------------------------------
.c_zcu = "#31539D" # hsl 221,52,40
.c_zcu_grey = "grey30"

# Faculties
.c_fav = "#CEAA1B" #hsl(48, 77%, 46%)
.c_fdu = "#C7362D" #hsl 4,63,48
.c_fek = "#D67C1C" #hsl(31, 77%, 47%)
.c_fel = "#293D83" #hsl(227, 52%, 34%)
.c_ff = "#5DB3DA" #hsl 199,63,61
.c_fpe = "#99BC39" #hsl(76, 53%, 48%)
.c_fpr = "#8A172E" #348, 71%, 32%
.c_fst = "#4C8CCB" #hsl(210, 55%, 55%)
.c_fzs = "#1C966A" #hsl(158, 69%, 35%)

.c_ntc = "#80217E" #hsl(301, 59%, 32%)

# Faculties + parts - color vectors
.c_faks = c(.c_fav, .c_fdu, .c_fek, .c_fel, .c_ff, .c_fpe, .c_fpr, .c_fst, .c_fzs)
names(.c_faks) = c('FAV','FDU','FEK','FEL','FF','FPE','FPR','FST','FZS')

.c_parts = c(.c_ntc, .c_zcu, .c_zcu, .c_fav, .c_zcu)
names(.c_parts) = c("NTC", "UJP", "RTI", "RICE", "NTIS")


# Single colors to define the uwb palettes -------------------------------------
# Single color
.c_single='#3670BC' #mix of the 4 zcu blues hsl(214, 55%, 47%)

# Light and dark ends of blue monochromatic scale
.c_light= "#82AFBD"
.c_dark="#2A2666"

# Light and dark ends of red monochromatic scale
.c_lightred = "#be8b83ff"
.c_darkred = "#682755ff"

# Ends of blue-red scale (bipolar, positive-negative)
.c_red = "#8F2439" # hsl(348, 60, 35)
#c_green = "#1A6166"
.c_blue = "#24528f" # darker c_single hsl(214, 60%, 35%)

# Midpoint of blue-red scale
#c_mid = '#c2b170'
.c_mid = "#84738c"

# Long scale (alternative of  monochromatic for longer range of values)
.c_long1 = "#82BDB7"
.c_long2 = '#651a64'

# Don't know answers
.c_nor = "#7f7f7f"


# uwb_scales / uwb color scales--------------------------------------------------------------
#list of color scales #ADD FURTHER SCALES HERE IF NECESSARY
.uwb_scales = list(
  quali=c("#3670BC", "#BF4059", "#82A329", "#5C3399", "#E0B406", "#326754",
          "#E68319", "#8F5682", "#707CA9", "#93261F", rep(.c_nor,10)), #qualitative palette; 10 greys at the end are to fill enough slots for long lists of categories
  faks=c(.c_fav, .c_fdu, .c_fek, .c_fel, .c_ff, .c_fpe, .c_fpr, .c_fst, .c_fzs, .c_zcu_grey),
  uwb_faks=c(.c_zcu_grey, .c_fav, .c_fdu, .c_fek, .c_fel, .c_ff, .c_fpe, .c_fpr, .c_fst, .c_fzs),
  mono=c(.c_light, .c_single, .c_dark),
  mono_rev=c(.c_dark, .c_single, .c_light),
  monored = c(.c_lightred, "#BF4059", .c_darkred),
  monored_rev = c(.c_darkred, "#BF4059", .c_lightred),
  long=c(.c_long1, .c_single, .c_long2),
  long_rev=c(.c_long2, .c_single, .c_long1),
  bi=c(.c_blue, .c_mid, .c_red),
  bi_rev=c(.c_red, .c_mid, .c_blue)
)


# Ulozeni do sysdata.rda --------------------
# Najde všechny objekty v aktuálním prostředí, které začínají tečkou
vsechny_interni_objekty <- ls(all.names = TRUE, pattern = "^\\.")

# Uloží je hromadně do sysdata.rda
do.call(usethis::use_data,
        c(lapply(vsechny_interni_objekty, as.name),
          internal = TRUE,
          overwrite = TRUE))
