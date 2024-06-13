##======================================
## Generate L-systems object
##======================================

#' @title Generate L-systems object
#' @description Generate L-System object by first generating an action string from axiom & rules, then the object itself
#' and then translate the object to the axes origins.
#'
#' @param axiom starting string
#' @param rules replacement rules
#' @param angle direction to next point
#' @param len initial length, defaults to 1
#' @param l_factor length factor per branch level, defaults to 1
#' @param n_gen number of generations
#'
#' @return Segments of L-Systems at axes origin. Each with segment's coordinates, direction, id, level, function placeholder
#'
#' @export
#'

generate_LS <- function(axiom,
                        rules,
                        angle,
                        len = 1,
                        l_factor = 1,
                        n_gen){

  ## Generate a turtle action list from an axiom, rules and number of generations
  alphabet <- "\\+|\\-|F|L|R|f|X|\\[|\\]|\\|"    # turtle alphabet

  string <- axiom
  if (n_gen > 0)       # case n_gen = 0 generate axiom
    for (i in 1:n_gen)
      string <- gsubfn::gsubfn(".", rules, string)

  actions <- stringr::str_extract_all(string, alphabet) |>
    unlist()

  ## Generate a Ls object based on a list of turtle actions and angle
  stack <- tibble(x = numeric(0), y = numeric(0), dir = numeric(0))
  object <- tibble(x1 = 0, y1 = 0, x2 = NA, y2 = NA, dir = 90, level = 0, func = 0)

  for (action in actions)
    switch(action,
           "F" = ,
           "L" = ,
           "R" = ,
           "|" = {
             ## move forward by len*(factor^level)
             curr_x <- object$x1[1]
             curr_y <- object$y1[1]
             level <- object$level[1]
             d <- object$dir[1]

             next_x <- curr_x + len*(l_factor^level)*cos(d * (pi/180))
             next_y <- curr_y + len*(l_factor^level)*sin(d * (pi/180))
             object$x2[1] <- next_x
             object$y2[1] <- next_y

             ## add new segment
             object <- add_row(object,
                               x1 = next_x, y1 = next_y, x2 = NA, y2 = NA, dir = d,
                               level = object$level[1], func = 0, .before = 1)
           },
           "f" = {
             ## move forward - skip a segment
             curr_x <- object$x1[1]
             curr_y <- object$y1[1]
             level <- object$level[1]
             d <- object$dir[1]

             next_x <- curr_x + len*(l_factor^level)*cos(d * (pi/180))
             next_y <- curr_y + len*(l_factor^level)*sin(d * (pi/180))
             object$x1[1] <- next_x
             object$y1[1] <- next_y
           },
           "X" = {
             ## mark node as a function point
             object$func[1] <- 1
           },
           "+" = object$dir[1] <- object$dir[1] + angle,   ## change direction
           "-" = object$dir[1] <- object$dir[1] - angle,   ## change direction
           "[" = {
             ## push current state to stack
             stack <- add_row(stack,
                              x = object$x1[1], y = object$y1[1],
                              dir = object$dir[1], .before = 1)
             object$level[1] <- object$level[1] + 1
           },
           "]" = {
             ## pop current state from stack
             l <- object$level[1]
             object <- object %>%
               filter(row_number() > 1) %>%
               add_row(x1 = stack$x[1], y1 = stack$y[1], x2 = NA, y2 = NA,
                       dir = stack$dir[1], level = l - 1, func = 0, .before = 1)
             stack <- filter(stack, row_number() > 1)
           })

  ## translate object to axes origin
  object <- tidyr::drop_na(object) |>
    mutate(s_id = row_number(),
           tx = min(x1), ty = min(y1),
           x1 = x1 - tx, y1 = y1-ty, x2 = x2 -tx, y2 = y2 - ty) %>%
    select(-tx, -ty)

  return(object)
}
