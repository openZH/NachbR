#' Retry Function Execution with Delay on Failure
#'
#' Attempts to execute a given function with specified arguments. If the function
#' fails (throws an error), it retries the execution up to `max_tries` times,
#' waiting `sleep_time` seconds between each attempt.
#'
#' @param fun A function to be executed.
#' @param args A named list of arguments to pass to the function.
#' @param max_tries Integer. Maximum number of attempts before giving up.
#' @param sleep_time Numeric. Time (in seconds) to wait between retries.
#'
#' @return The result of the successful function execution.
#' If all attempts fail, the function throws an error.
#'
#' @examples
#' \dontrun{
#' 
#' url <- "https://amtsblattportal.ch/api/v1/publications/xml?"
#' 
#' params <- list(
#'   publicationStates = "PUBLISHED",
#'   rubrics = "BP-ZH",
#'   subrubrics = "BP-ZH01",
#'   publicationDate.start = start,
#'   publicationDate.end = as.character(Sys.Date()),
#'   cantons = "ZH",
#'   pageRequest.size = page_size
#'   )
#' 
#' new_url <- try_later_if_fail(fun = get_new_pub_url,
#'                              args = list(page_size = page_size,
#'                              url = url,
#'                              params = params, 
#'                              df_bp = df_bp), 
#'                              max_tries = 2, 
#'                              sleep_time = 3600)
#'}
#' @export
try_later_if_fail <- function(fun, args, max_tries, sleep_time) {
  max_tries
  tries <- 0
  
  repeat {
    tries <- tries + 1
    tmp <- try(do.call(fun, args = args), silent = TRUE)
    
    if (!inherits(tmp, "try-error")) {
      return(tmp)
    }
    
    if (tries >= max_tries) {
      stop("Function failed after 3 attempts.")
    }
    
    # Optional: wait before retrying
    Sys.sleep(sleep_time)
  }
}