/**
 * Country State Service
 * Handles loading and managing country, state, and phone code data
 */
open Promise
open Array

type country = {
  code: string,
  name: string,
  phoneCode: string,
  validationRegex: string,
  formatExample: string,
  formatRegex: string,
  timeZones: array<string>,
}

type countryData = {
  country_code: string,
  country_name: string,
  phone_number_code: string,
  validation_regex: string,
  format_example: string,
  format_regex: string,
  timeZones: array<string>,
}

type countryStateData = {
  country: array<countryData>,
  states: Dict.t<array<JSON.t>>,
}

type countryStateService = {
  mutable data: option<countryStateData>,
  mutable countries: array<country>,
  mutable states: Dict.t<array<JSON.t>>,
  mutable phoneCodes: array<string>,
}

// External bindings for fetch API
@val external fetch: string => promise<'response> = "fetch"
@send external json: 'response => promise<JSON.t> = "json"

// JSON decoding helpers
@get external getCountry: JSON.t => array<countryData> = "country"
@get external getStates: JSON.t => Dict.t<array<JSON.t>> = "states"

let make = () => {
  data: None,
  countries: [],
  states: Dict.make(),
  phoneCodes: [],
}

/**
 * Initialize the service by loading country-state data
 * @returns Promise<bool> Success status
 */
let initialize = (service: countryStateService): promise<bool> => {
  fetch("countrystate.json")
  ->then(response => json(response))
  ->then(jsonData => {
    let data = {
      country: getCountry(jsonData),
      states: getStates(jsonData),
    }
    service.data = Some(data)

    service.countries =
      data.country->map(country => {
        code: country.country_code,
        name: country.country_name,
        phoneCode: country.phone_number_code,
        validationRegex: country.validation_regex,
        formatExample: country.format_example,
        formatRegex: country.format_regex,
        timeZones: country.timeZones,
      })

    service.states = data.states

    // Process phone codes (unique list)
    let allPhoneCodes = service.countries->map(country => country.phoneCode)
    let uniquePhoneCodes = []
    allPhoneCodes->forEach(phoneCode => {
      if !(uniquePhoneCodes->includes(phoneCode)) {
        uniquePhoneCodes->push(phoneCode)->ignore
      }
    })

    // Remove the duplicate sorting call
    service.phoneCodes =
      uniquePhoneCodes->toSorted((a, b) =>
        if a < b {
          -1.
        } else if a > b {
          1.
        } else {
          0.
        }
      )

    Console.log("CountryStateService initialized successfully")
    Console.log(
      `Loaded ${length(service.countries)->Int.toString} countries and ${Dict.keysToArray(
          service.states,
        )
        ->length
        ->Int.toString} country-state mappings`,
    )

    resolve(true)
  })
  ->catch(error => {
    Console.error2("Error loading country-state data:", error)
    resolve(false)
  })
}

/**
 * Get all countries
 * @returns Array of country objects
 */
let getCountries = (service: countryStateService): array<country> => {
  service.countries
}

/**
 * Get country by code
 * @param service - Country state service instance
 * @param countryCode - Country code (e.g., 'US', 'IN')
 * @returns option<country> Country object or None if not found
 */
let getCountryByCode = (service: countryStateService, countryCode: string): option<country> => {
  service.countries->find(country => country.code === countryCode)
}

/**
 * Get states for a country
 * @param service - Country state service instance
 * @param countryCode - Country code
 * @returns Array of state objects
 */
let getStatesByCountry = (service: countryStateService, countryCode: string): array<JSON.t> => {
  switch Dict.get(service.states, countryCode) {
  | Some(states) => states
  | None => []
  }
}

/**
 * Get all phone codes
 * @returns Array of phone codes
 */
let getPhoneCodes = (service: countryStateService): array<string> => {
  service.phoneCodes
}

/**
 * Get phone code for a country
 * @param service - Country state service instance
 * @param countryCode - Country code
 * @returns option<string> Phone code or None if not found
 */
let getPhoneCodeByCountry = (service: countryStateService, countryCode: string): option<string> => {
  switch getCountryByCode(service, countryCode) {
  | Some(country) => Some(country.phoneCode)
  | None => None
  }
}

/**
 * Get validation regex for a country's phone number
 * @param service - Country state service instance
 * @param countryCode - Country code
 * @returns option<string> Validation regex or None if not found
 */
let getPhoneValidationRegex = (service: countryStateService, countryCode: string): option<
  string,
> => {
  switch getCountryByCode(service, countryCode) {
  | Some(country) => Some(country.validationRegex)
  | None => None
  }
}

/**
 * Get format example for a country's phone number
 * @param service - Country state service instance
 * @param countryCode - Country code
 * @returns option<string> Format example or None if not found
 */
let getPhoneFormatExample = (service: countryStateService, countryCode: string): option<string> => {
  switch getCountryByCode(service, countryCode) {
  | Some(country) => Some(country.formatExample)
  | None => None
  }
}

/**
 * Search countries by name
 * @param service - Country state service instance
 * @param searchTerm - Search term
 * @returns Array of matching countries
 */
let searchCountries = (service: countryStateService, searchTerm: string): array<country> => {
  let term = String.toLowerCase(searchTerm)
  service.countries->filter(country => {
    String.toLowerCase(country.name)->String.includes(term) ||
      String.toLowerCase(country.code)->String.includes(term)
  })
}

/**
 * Get popular countries (commonly used ones)
 * @returns Array of popular country codes
 */
let getPopularCountries = (): array<string> => {
  [
    "US",
    "GB",
    "CA",
    "AU",
    "DE",
    "FR",
    "IN",
    "JP",
    "CN",
    "BR",
    "IT",
    "ES",
    "NL",
    "SE",
    "NO",
    "DK",
    "FI",
    "CH",
    "AT",
    "BE",
  ]
}

/**
 * Get countries sorted with popular ones first
 * @param service - Country state service instance
 * @returns Array of countries with popular ones first
 */
let getCountriesSorted = (service: countryStateService): array<country> => {
  let popular = getPopularCountries()
  let popularCountries = []
  let otherCountries = []

  service.countries->forEach(country => {
    if popular->some(code => code === country.code) {
      popularCountries->push(country)->ignore
    } else {
      otherCountries->push(country)->ignore
    }
  })

  // Sort popular countries by the order in popular array
  popularCountries
  ->toSorted((a, b) => {
    let indexA = popular->findIndex(code => code === a.code)
    let indexB = popular->findIndex(code => code === b.code)
    let indexAVal = indexA === -1 ? 999. : Int.toFloat(indexA)
    let indexBVal = indexB === -1 ? 999. : Int.toFloat(indexB)
    indexAVal -. indexBVal
  })
  ->ignore

  // Sort other countries alphabetically
  otherCountries
  ->toSorted((a, b) =>
    if a.name < b.name {
      -1.
    } else if a.name > b.name {
      1.
    } else {
      0.
    }
  )
  ->ignore

  concat(popularCountries, otherCountries)
}

/**
 * Check if service is initialized
 * @param service - Country state service instance
 * @returns bool Initialization status
 */
let isInitialized = (service: countryStateService): bool => {
  switch service.data {
  | None => false
  | Some(_) => true
  }
}

/**
 * Reset the service
 * @param service - Country state service instance
 */
let reset = (service: countryStateService): unit => {
  service.data = None
  service.countries = []
  service.states = Dict.make()
  service.phoneCodes = []
}

// Export singleton instance
let countryStateService = make()
