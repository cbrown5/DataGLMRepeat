#' Evaluate expressoin by groups of grouped dataframe
#'
#' @Usage with_groups(df, expr)
#'
#' @param df agrouped data frame
#' @param expr and expression, possible wrapped in curly braces. 
#'
#' @return A list with the contents of the expression evaluated  by 
#' groups. 
#' @Details Basically this loops over the groups and applies an
#' expression
#'
#' @examples 
#' library(dplyr)
#' dat2 <- tibble(
#'    grp = sample(c(10, 20, 30), 100, replace = TRUE), 
#'    x = 1:100, 
#'    y = x*2 + rnorm(100) + grp)
#' gout <- dat2 %>% 
#'   group_by(grp) %>%
#'     with_groups(., {
#'      lm(y ~x)
#'   })
#'   
#' @author Christopher J. Brown
#' @rdname with_groups
#' @export

with_groups <- function(df, expr){
  stopifnot("grouped_df" %in% class(df)) 

  expr_txt <- deparse(substitute(expr))
  ngroups <- nrow(attr(df, "groups"))
  group_names <- names(attr(df, "groups"))
  group_names <- group_names[-length(group_names)]
  labels <- tidyr::unite(attr(df, "groups"), "group", group_names)$group
  dtemp <- NULL
  for (g in 1:ngroups){
    dfcopy <- df[attr(df, "groups")$.rows[[g]],]
    dtemp <- c(dtemp, 
               list(eval(parse(text = expr_txt), dfcopy, 
                         enclos = parent.frame())))
  }
  names(dtemp) <- labels
    return(dtemp)
  }
  



