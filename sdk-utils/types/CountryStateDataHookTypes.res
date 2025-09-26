type country = {
  country_code: string,
  country_name: string,
  country_flag?: string,
  phone_number_code: string,
  validation_regex?: string,
  format_example?: string,
  format_regex?: string,
  timeZones: array<string>,
}
type state = {
  label: string,
  value: string,
  code: string,
}
type states = Dict.t<array<state>>
type countries = array<country>
type countryStateData = {
  countries: countries,
  states: states,
}
let defaultTimeZone = {
  country_code: "",
  country_name: "",
  phone_number_code: "",
  timeZones: [],
}
