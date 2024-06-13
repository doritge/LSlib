##======================================
## Generate a random L-systems object based on parameters
##======================================

##  Generate a random rule (Currently supports one rule only with F as re-writing symbol)
gen_random_LS_rule <- function(length,         # rule string length
                               symbols,        # set of rule symbols
                               p,              # probability of symbols
                               b_depth,        # branching depth
                               seed = NULL){

  if (!is.null(seed)) set.seed(seed)
  string <- sample(symbols, size = length, replace = TRUE, prob = p)

  # randomly insert branching brackets in string
  if(b_depth == 0) return(string)

  nb <- b_depth * 2
  b_set <- rep(c("[", "]"), b_depth)
  b_location <- sample(1:(length), size = nb)|>
    sort() # after

  #  special case: last closing bracket located at end of string
  b <- ifelse(b_location[nb] < length, nb, nb-1)
  for(i in 1:b){
    string <- c(string[1:(b_location[i] + i - 1)],
                b_set[i],
                string[(b_location[i] + i):length(string)])
  }
  if(b < nb) string <- c(string, b_set[nb])

  return(string)
}

## Read log file with random generation parameters
read_LS_random_log <- function(fname){
  log <- read_csv(fname, col_types = list(name = col_character(),
                                          symbols = col_character(),
                                          axiom = col_character(),
                                          prob = col_character(),
                                          seed = col_integer(),
                                          rlen = col_integer(),
                                          depth = col_integer(),
                                          angle = col_double(),
                                          slen = col_double(),
                                          factor = col_double(),
                                          gens = col_integer()))
  return(log)
}

## imports
#' @import dplyr
#' @import readr
#' @import ggplot2
##
NULL

#' @title Generate random L-systems object
#' @description Generate a random L-System object from parameters in file.
#' This is done by generating a random rule from the parameters and then calling generate_LS()
#'
#' @param fname log file name
#' @param r_seed seed (also object id)
#'
#' @return Segments of L-Systems at axes origin. Each with segment's coordinates, direction, id, level, function placeholder
#'
#' @export

generate_random_LS <- function(fname,
                               r_seed){

  #---- object from a parameter file
  params <- read_LS_random_log(fname) |>
    filter(seed == r_seed)

  #----- generate a random rule
  s <- gen_random_LS_rule(length = params$rlen,
                         symbols = str_split(params$symbols,",")[[1]],
                         p = as.integer(str_split(params$prob,",")[[1]]),
                         b_depth = params$depth,
                         seed = r_seed)
  rules <- list("F" = s)

  #------ generate the L-System object
  object <- generate_LS(axiom = params$axiom,
                             rules = rules,
                             angle = params$angle,
                             len = params$slen,
                             l_factor = params$factor,
                             n_gen = params$gens)
}

#' @title New random L-system objects
#' @description creating a batch of random LS images, writing them to a folder and writing their parameters into a log file
#'
#' @param log_name log file name
#' @param img_prefix prefix of image file names in batch
#' @param nimages number of images in batch
#' @param a direction of movement. a single value or a vector to be sampled
#'
#' @return a batch of images in a folder and an updated log file
#'
#' @export

new_random_LS <- function(log_name,
                          img_prefix,
                          nimages,
                          a){

  log <- read_LS_random_log(log_name)

  for (i in 1: nimages){
    #--- rule parameters
    seed <- sample(1:10000, 1)
    r_length <- sample(8:12 , 1)
    symbols <- c("F", "|", "+", "-", "X")
    p <- c(12, 3, 5, 5, 2)
    b_depth <-  sample(0:2, 1)

    #--- object parameters
    axiom = "F"
    angle <- ifelse(length(a) == 1, a, sample(a,1))
    print(angle)
    len <- 1
    l_factor <- runif(1, 0.50, 1)
    n_gens <- sample(3:5, 1)

    #---- generate image
    s <- gen_random_LS_rule(length = r_length, symbols = symbols, p = p, b_depth = b_depth, seed = seed)
    rules <- list("F" = s)

    image <- generate_LS(axiom = axiom, rules = rules,  angle = angle, len = len, l_factor = l_factor,
                         n_gen = n_gens) |>
      ggplot2::ggplot() +
      ggplot2::geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),
                            linewidth = 1,
                            lineend = "round") +
      ggplot2::coord_equal()

    #---- save image + log
    img_name <- stringr::str_c(img_prefix, format(Sys.time(), "%d-%H-%M-%S"), "_s_", seed, ".png")
    ggplot2::ggsave(img_name, image)
    log <- log |>
      add_row(name = img_name, seed = seed,
              rlen = r_length, symbols = stringr::str_c(symbols, collapse = ","),
              prob = stringr::str_c(p, collapse = ","), depth = b_depth,
              axiom = axiom, angle = angle, slen = len, factor = l_factor, gens = n_gens)
  }

  write_csv(log, log_name)
}
