#' Probability of having to queue in a system
#'
#' This algorithm calculates the probability of having to queue using the Erlang C traffic formula over an M/M/c queue model.
#'
#' @param demandqty a numeric vector of the number of demand units entering the queue per given time interval
#' @param perxmin an integer value indicating the number of minutes the demand qty is measured over.
#' @param serviceduration a numeric vector of the average service duration in seconds
#' @param numagents an integer value of the number of agents staffing the queue system
#' @return a dataframe containing Agents_In_Use and probability to queue Prob_Queue
#' @author Dean Marchiori
#' @details
#' The function accepts the specified service levels and demand statistics and calculates the
#' probability of queueing using the Erlang C formula.
#' @references http://en.wikipedia.org/wiki/Erlang_unit
#' @export
#'

probQ<- function(demandqty, perxmin = 30, serviceduration, numagents) {

  #Declare variables
  s               <-NULL
  arrivalrate     <-demandqty/(perxmin *60)
  intensity       <-arrivalrate * serviceduration

  # as occupancy maxes out at 100%, whenever there are insufficient agents to match intensity, set to 1
  if (numagents < intensity) {

    occupancy <- intensity/ceiling(intensity)

  }else{

    occupancy <-intensity/numagents

  }

  #Calculate sum of series for Erlang C calculation
  for (i in 0:(numagents-1)) {

    s[i+1]<- (intensity^i)/factorial(i)

  }

  #Erlang C calculation
  ErlangC <- (intensity^numagents)/factorial(numagents)/(((intensity^numagents)/factorial(numagents))+(1-occupancy)*sum(s))

  #Data returned
  ProbQResults <<- data.frame("Agents_In_Use"= as.integer(numagents), "Prob_Queue" =round(ErlangC,2))
  ProbQueue <<- round(ErlangC,2)
  #END

}



