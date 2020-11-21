
#' Weather Simulation
#'
#'  Weather is either sunny or rainy in Richmond. If a day is sunny, P(sunny tomorrow)=0.85.
#'  If a day is rainy, P(rainy tomorrow)=0.35. If a day is rainy, the amount of rainfall accumulation
#'  in the city is governed by an Exponential(L=2) distribution, where the value from that distribution
#'  is the rainfall in inches. If a day is sunny, there can be no rain.
#'  Specifically, given an initial day's weather conditions, simulate the following 10 days of weather and calculate the projected rainfall acc
#'
#'
#' Given an initial day's weather conditions, simulate the following
#' 10 days of weather and calculate the projected rainfall accumulation in inches.
#'
#' @param w character; "sunny" or "rainy"; the weather condition today.
#'
#' @return
#' @export
#'
#' @examples
#' weather_simulator("sunny")


weather_simulator <- function(w){

  if (w!="sunny" & w!="rainy"){
    stop("Expected weather is sunny or rainy")
  }
  else{
    weathers <- c("sunny","rainy")
    weatherMatrix <- matrix(data=c(0.85,0.15,0.65,0.35),
                            byrow=TRUE, nrow=2,
                            dimnames=list(weathers, weathers))
    MC_weather <- new("markovchain", states=weathers, byrow=TRUE,
                      transitionMatrix=weatherMatrix, name="Weather")

    if (w == "sunny"){
      weathersOf10Days <- rmarkovchain(n = 10, object = MC_weather, t0 = "sunny")}
    else {
      weathersOf10Days <- rmarkovchain(n = 10, object = MC_weather, t0 = "rainy")}

    rainfallof10Days <- vector(length = 10)
    for(i in 1:10){
      rainfallof10Days[i] <- ifelse(weathersOf10Days[i] == "sunny", 0, pexp(1,2))
    }
    sunny         <- sum(weathersOf10Days =="sunny")
    rainy         <- sum(weathersOf10Days =="rainy")
    totalrainfall <- sum(rainfallof10Days)
    output <-list(data.frame(sunny,rainy,totalrainfall),data.frame(weathersOf10Days, rainfallof10Days))
    return(output)
  }
}
