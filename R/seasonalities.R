YEAR_LENGTH <- 365.25

generateTemperature <- function(day, amplitude = 6, mean_temp = 20, phase_shift = -1.5) {
  # Convert day_of_year to radians for the sinusoidal function
  radian_angle <- 2 * pi * (day-1) / YEAR_LENGTH
  
  # Calculate the temperature using the sinusoidal function
  temperature <- amplitude * sin(radian_angle - phase_shift) + mean_temp
  return(temperature)
}

isSchoolHoliday <- function(day) {
  # Define school holiday periods in 2020
  schoolTermDates <- c(
    # 2020
    seq(as.Date("2020-01-28"), as.Date("2020-03-27"), by = "days"),
    seq(as.Date("2020-04-14"), as.Date("2020-06-26"), by = "days"),
    seq(as.Date("2020-07-13"), as.Date("2020-09-18"), by = "days"),
    seq(as.Date("2020-10-05"), as.Date("2020-12-18"), by = "days"),
    # 2021
    seq(as.Date("2021-01-27"), as.Date("2021-03-31"), by = "days"),
    seq(as.Date("2021-04-19"), as.Date("2021-06-25"), by = "days"),
    seq(as.Date("2021-07-12"), as.Date("2021-09-17"), by = "days"),
    seq(as.Date("2021-10-04"), as.Date("2021-12-17"), by = "days")
  )
  
  publicHolidays <- c(
    # 2020
    as.Date("2020-01-01"),  # New Year's Day
    as.Date("2020-01-27"),  # Australia Day
    as.Date("2020-03-09"),  # Labor Day
    as.Date("2020-04-10"),  # Good Friday
    as.Date("2020-04-11"),  # Saturday before Easter Sunday
    as.Date("2020-04-12"),  # Easter Sunday
    as.Date("2020-04-13"),  # Easter Monday
    as.Date("2020-04-25"),  # ANZAC Day
    as.Date("2020-06-08"),  # Queen's Birthday
    as.Date("2020-10-23"),  # Friday before AFL Grand Final
    as.Date("2020-11-03"),  # Melbourne Cup
    as.Date("2020-12-25"),  # Christmas Day
    as.Date("2020-12-26"),  # Boxing Day
    as.Date("2020-12-28"),  # Monday after Boxing Day
    
    # 2021
    as.Date("2021-03-08"),  # Labor Day
    as.Date("2021-04-02"),  # Good Friday
    as.Date("2021-04-03"),  # Saturday before Easter Sunday
    as.Date("2021-04-04"),  # Easter Sunday
    as.Date("2021-04-05"),  # Easter Monday
    as.Date("2021-04-25"),  # ANZAC Day
    as.Date("2021-06-14"),  # Queen's Birthday
    as.Date("2021-09-24"),  # Friday before AFL Grand Final
    as.Date("2021-11-02"),  # Melbourne Cup
    as.Date("2021-12-25"),  # Christmas Day
    as.Date("2021-12-26"),  # Boxing Day
    as.Date("2021-12-27"),  # Monday after Christmas Day
    as.Date("2021-12-28")   # Tuesday after Boxing Day
  )
  
  date <- as.Date("2020-01-01") + day - 1

  return (
      !(date %in% schoolTermDates) | 
      weekdays(date) %in% c("Saturday", "Sunday") |
      date %in% publicHolidays
  )
}

underLockdown <- function(day) {
  # Define the lockdown periods as sequences of dates
  lockdownDates <- c(
    seq(as.Date("2020-03-30"), as.Date("2020-05-12"), by = "days"),
    seq(as.Date("2020-07-08"), as.Date("2020-10-27"), by = "days"),
    seq(as.Date("2021-02-12"), as.Date("2021-02-17"), by = "days"),
    seq(as.Date("2021-05-27"), as.Date("2021-06-10"), by = "days"),
    seq(as.Date("2021-07-15"), as.Date("2021-07-27"), by = "days"),
    seq(as.Date("2021-08-05"), as.Date("2021-10-21"), by = "days")
  )
  
  date <- as.Date("2020-01-01") + day - 1
  return(date %in% lockdownDates)
}

