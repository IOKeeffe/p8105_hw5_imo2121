create_birthdays = function(n) {
  birthdays = sample(1:365, size = n, replace = T)
  repeated_birthdays = length(unique(birthdays)) < n
  
  return(repeated_birthdays)
}
