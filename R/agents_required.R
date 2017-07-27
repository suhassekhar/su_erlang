#' Resource planning for queues
#'
#' This algorithm calculates the number of agents required to staff a queue
#' to achieve a given service level using the Erlang C traffic formula over an M/M/c queue model.
#'
#' @param demandqty a numeric vector of the number of demand units entering the queue per time interval
#' @param perxmin an integer value indicating the number of minutes the demand qty is measured over.
#' @param serviceduration a numeric vector of the average service duration in seconds
#' @param targetservetime a numeric vector of the targeted servce time in seconds
#' @param targetservicelevel a floating point decimal of the target Grade of Service percentage i.e. percent of traffic within targetserve duration
#' @return a dataframe containing Agents_Required and attainable Service_Level
#' @author Dean Marchiori
#' @details
#' The function accepts the specified service levels and demand statistics and calculates the
#' probability of queueing using the Erlang C formula. The algorithm iterates the number
#' of agents present until the target GOS is met.
#' The model assumes a M/M/c queue system where arrivals form a single queue and are governed by a Poisson
#' process where there are c servers and service times are exponentially distributed.
#' @references http://en.wikipedia.org/wiki/Erlang_unit
#' @export
#'

agents_required <- function(demandqty, perxmin = 30, serviceduration, targetservetime, targetservicelevel) {

  #Declare variables
  s               <-NULL
  arrivalrate     <-demandqty/(perxmin *60)
  intensity       <-arrivalrate*serviceduration
  numagents       <-ceiling(intensity)
  occupancy       <-intensity/numagents

  #Repeating loop to find optimal agent number.
  repeat{

    #Re-declare variable
    s<-NULL
    arrivalrate <-demandqty/(perxmin *60)
    intensity   <-arrivalrate*serviceduration
    occupancy   <-intensity/numagents

    #Calculate sum of series for Erlang C calculation
    for (i in 0:(numagents-1)) {

      s[i+1]<- (intensity^i)/factorial(i)

    }

    #Erlang C calculation
    ErlangC<-(intensity^numagents)/factorial(numagents)/(((intensity^numagents)/factorial(numagents))+(1-occupancy)*sum(s))

    #Service Level Calculation
    servicelevel<- 1- (ErlangC * exp(-(numagents-intensity)*(targetservetime/serviceduration)))

    #break condition, will assess if service level has risen above target, if not it will increment agent numbers by one
    if (servicelevel > targetservicelevel) {

      break

    } else

      numagents <- numagents+1

  }

  #Data returned
  AgentResults <<- data.frame("Agents_Required"= as.integer(numagents), "Service_Level" =round(servicelevel,2))
  print(paste("Agents Required: ", as.integer(numagents)))
  #print(paste("Service Level: ", round(servicelevel,2)))
  #END

}
