list_samplecities <- c("Abidjan", "Berlin", "Miami", "Singapore")

overleafwidth <- 14.58792 # Textwidth in Overleaf in cm

#########################
# LCZ color classes
# Official LCZ Color Scheme (Stewart & Oke 2012 / WUDAPT standard)
colors_lcz_complete <- c(
  "#8c0000",  # LCZ 1  - Compact high-rise (dark red)
  "#d10000",  # LCZ 2  - Compact midrise (red)
  "#ff0000",  # LCZ 3  - Compact lowrise (bright red)
  "#bf4d00",  # LCZ 4  - Open high-rise (dark orange)
  "#ff6600",  # LCZ 5  - Open midrise (orange)
  "#ff9955",  # LCZ 6  - Open lowrise (light orange)
  "#faee05",  # LCZ 7  - Lightweight lowrise (yellow)
  "#bcbcbc",  # LCZ 8  - Large lowrise (light gray)
  "#ffccaa",  # LCZ 9  - Sparsely built (peach)
  "#555555",  # LCZ 10 - Heavy industry (dark gray)
  "#006a00",  # LCZ A  - Dense trees (dark green)
  "#00aa00",  # LCZ B  - Scattered trees (green)
  "#648525",  # LCZ C  - Bush, scrub (olive green)
  "#b9db79",  # LCZ D  - Low plants (light green)
  "#000000",  # LCZ E  - Bare rock or paved (black)
  "#fbf7ae",  # LCZ F  - Bare soil or sand (beige)
  "#6a6aff"   # LCZ G  - Water (blue)
)

# For the built types (LCZ 1-10):
colors_lcz_built <- c(
  "#8c0000",  # LCZ 1
  "#d10000",  # LCZ 2
  "#ff0000",  # LCZ 3
  "#bf4d00",  # LCZ 4
  "#ff6600",  # LCZ 5
  "#ff9955",  # LCZ 6
  "#faee05",  # LCZ 7
  "#bcbcbc",  # LCZ 8
  "#ffccaa",  # LCZ 9
  "#555555"   # LCZ 10
)

# Without LCZ 7 and 10:
colors_lcz_no7no10 <- c(
  "#8c0000",  # LCZ 1
  "#d10000",  # LCZ 2
  "#ff0000",  # LCZ 3
  "#bf4d00",  # LCZ 4
  "#ff6600",  # LCZ 5
  "#ff9955",  # LCZ 6
  "#bcbcbc",  # LCZ 8
  "#ffccaa"   # LCZ 9
)


# # Some functions to provide labels and color schemes
# Names for LCZ to be used in the paper
names_lcz <- c("Compact high-rise", "Compact midrise", "Compact lowrise", "Open high-rise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built", "Heavy industry")

fct_labeler_lcz_built_no7no10 <- function(df) {
  # Set factor levels and labels for local climate zones (urban form)
  df <- df %>%
    #sf::st_drop_geometry(df) %>%
    mutate(lcz = factor(lcz, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, NA),
                        labels = c("Compact high-rise", "Compact midrise", "Compact lowrise",
                                   "Open high-rise", "Open midrise", "Open lowrise",
                                   "Lightweight lowrise", "Large lowrise", "Sparsely built",
                                   "Heavy industry", "Dense trees", "Scattered trees",
                                   "Bush, scrub", "Low plants", "Bare rock", "Bare soil",
                                   "Water"),
                        ordered = T))
}

fct_labeler_lcz2 <- function(lcz) {
  factor(lcz, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, NA),
         labels = c("Compact high-rise", "Compact midrise", "Compact lowrise",
                    "Open high-rise", "Open midrise", "Open lowrise",
                    "Lightweight lowrise", "Large lowrise", "Sparsely built",
                    "Heavy industry", "Dense trees", "Scattered trees",
                    "Bush, scrub", "Low plants", "Bare rock", "Bare soil",
                    "Water"),
         ordered = T)
}


##########################
# Colors climate zones (KGC)

colors_Clsmain <- c("#33a02c", "#ff7f00", "#b2df8a", "#fb9a99", "#a6cee3")

fct_factorlabels <- function(df) {
  df <- fct_labeler_lcz(df)
  df <- fct_labeler_Clsmain(df)
  df <- fct_labeler_subregions(df)
}


fct_labeler_Clsmain2 <- function(Clsmain) {
  factor(Clsmain, levels = c("A", "B", "C", "D"),
         labels = c("Tropical", "Dry", "Temperate", "Continental"),
         ordered = T)
}


fct_labeler_Clsmain <- function(df) {
  df <- df %>%
    mutate(Clsmain = factor(Clsmain,
                            levels = c("A", "B", "C", "D"),
                            labels = c("Trop.", "Dry", "Temp.", "Cont."),
                            ordered = T))
}

fct_labeler_Cls <- function(df) {
  df <- df %>%
    mutate(Cls = factor(Cls,
                            levels = c("Af", "Am", "As", "Aw", "BSh", "BSk", "BWh" , "BWk" , "Cfa" , "Cfb" , "Csa" , "Csb" , "Cwa", "Cwb" , "Dfa", "Dfb", "Dwa"),
                            labels = c("Af" = "Af (Eq. hum.)", "Am" = "Am (Eq. mons.)", "Aw" = "Aw (Eq. wint.dry)", "As" = "As (Eq. s.dry)", "BSh" = "BSh (Arid step.hot.arid)", "BSk" = "BSk (Arid step.cold.arid)", "BWh" = "BWh (Arid des.hot.arid)", "BWk" = "BWk (Temp. des.cold.arid)", "Cfa" = "Cfa (Temp. hum.hotsum)", "Cfb" = "Cfb (Temp. hum.warmsum)", "Csa" = "Csa (Temp. sum.dry.hotsum)", "Csb" = "Csb (Temp. sum.dry.warmsum)", "Cwa" = "Cwa (Temp. wint.dry.hotsum)",  "Cwb" = "Cwb (Temp. wint.dry.warmsum)", "Dfa" = "Dfa (Snow full.hum.hotsum)", "Dfb" = "Dfb (Snow hum.warmsum)", "Dwa" = "Dwa (Snow wint.dry.hotsum)"),
                            ordered = T))
}

fct_labeler_subregions <- function(df){
  df <- df %>%
    mutate(subreg =
             factor(`Sub-region Name`,
                    levels = c("Northern Africa", "Sub-Saharan Africa", "Northern America", "Latin America and the Caribbean", "Northern Asia", "Southern Asia", "South-eastern Asia", "Western Asia", "Eastern Asia", "Eastern Europe", "Northern Europe", "Southern Europe", "Western Europe", "Australia and New Zealand"),
                    labels = c("N. Africa", "Sub-Sah. Africa", "N. America",
                               "Lat. Am. & Car.", "N. Asia", "S. Asia", "SE Asia",
                               "W. Asia", "E. Asia", "E. Europe", "N. Europe", "S. Europe",
                               "W. Europe", "AUS & NZ"),
                    ordered = T))
}

# Dictionary for etable statistics output, for etable(..., dict = dictstats")
dictstats <- c("log(t)" = "log(CDH)", "t" = "CDH", "log(t_min)" = "log($T_{2, min}$)", "t_min" = "$T_{2, min}$", "log(t_max)" = "log($T_{2, max}$)", "t_max" = "$T_{2, max}$",
               "out_b_mean" = "GVI (mean)", "out_b_min" = "GVI (min)", "out_b_min" = "GVI (min)", "out_b_max" = "GVI (max)",
               "pop_dens" = "Pop. density", "build_h" = "Buildings height", "build_v" = "Buildings volume", "water" = "Water body", "elevation" = "Elevation",
               "lcz" = "Urban Form", "city" = "City",
               "ClsmainTrop." = "Trop.", "ClsmainDry" = "Dry", "ClsmainTemp." = "Temp.", "ClsmainCont." = "Cont.",
               "ClsAm" = "Am (Eq. mons.)", "ClsAf" = "Af (Eq. hum.)", "ClsAw" = "Aw (Eq. wint.dry)", "ClsAs" = "As (Eq. s.dry)", "ClsBSh" = "BSh (Arid step.hot.arid)", "ClsBSk" = "BSk (Arid step.cold.arid)", "ClsBWh" = "BWh (Arid des.hot.arid)", "ClsBWk" = "BWk (Temp. des.cold.arid)", "ClsCfa" = "Cfa (Temp. hum.hotsum)", "ClsCfb" = "Cfb (Temp. hum.warmsum)", "ClsCsa" = "Csa (Temp. sum.dry.hotsum)", "ClsCsb" = "Csb (Temp. sum.dry.warmsum)", "ClsCwb" = "Cwb (Temp. wint.dry.warmsum)", "ClsDfb" = "Dfb (Snow hum.warmsum)")

fct_add_kgc <- function(df) {
  library(kgc)
  cl <- climatezones
  df$x_s <- RoundCoordinates(df$x)
  df$y_s <- RoundCoordinates(df$y)
  df <- df %>% mutate(Clsmain = Cls) %>% dplyr::select(-Cls)
  df <- merge(df, cl, by.x=c("x_s", "y_s"), by.y=c("Lon", "Lat"))
  df$Clsmain <- substr(as.character(df$Cls), 1, 1) # Add the main KGC [A (tropical), B (arid), C (temperate), D (continental), and E (polar)]
}