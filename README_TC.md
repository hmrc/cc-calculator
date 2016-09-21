# TC Calculator

Tax Credits Calculator service layer has 2 different endpoints with separate calculations, both calculation share the same input and output model.

Total Award calculation returns the result of the overall Tax Credits calculation.

Income Advice calculation returns the result with income advice amount when the income itself is not yet known.

* **Endpoint URLs:**

  * **TC (Income Advice Calculation):**   /tax-credits/calculate/income-advice  <br />
  * **TC (Total Award Calculation):**   /tax-credits/calculate/total-award  <br />


* **Data Parameters:**

  `Data structure for Tax Credits input json:`

    ```javascript
    {
      payload: {
        eligibility: {
          tc: {
            proRataEnd: Option[LocalDate] = None,
            taxYears: [
              {
                from: [LocalDate],
                until: [LocalDate],
                houseHoldIncome: [BigDecimal] = 0.00,
                periods: [
                  {
                    from: [LocalDate],
                    until: [LocalDate],
                    householdElements: {
                      basic: [Boolean] = false,
                      hours30: [Boolean] = false,
                      childcare: [Boolean] = false,
                      loneParent: [Boolean] = false,
                      secondParent: [Boolean] = false,
                      family: [Boolean] = false
                    },
                    claimants: [
                      {
                        qualifying: [Boolean] = false,
                        isPartner: [Boolean] = false,
                        claimantElements: {
                          disability: [Boolean] = false,
                          severeDisability: [Boolean] = false
                        },
                        doesNotTaper: [Boolean] = false
                      }
                    ],
                    children: [
                      {
                        id: [Short],
                        name: [String] = None,
                        qualifying: [Boolean] = false,
                        childcareCost: [BigDecimal] = 0.00,
                        childcareCostPeriod: [Enumeration] {
                           Week,
                           Fortnight,
                           Month,
                           3 month,
                           Year,
                           INVALID
                         },
                        childElements: {
                          child: [Boolean] = false,
                          youngAdult: [Boolean] = false,
                          disability: [Boolean] = false,
                          severeDisability: [Boolean] = false,
                          childcare: [Boolean] = false
                        }
                      }
                    ]
                  }
                ]
              }
            ]
          }
        }
      }
    }
    ```

  `Data structure for Tax Credits output json:`

     ```javascript
     {
     calculation: {
        tc: {
            from: [LocalDate],
            until: [LocalDate],
            proRataEnd: Option[LocalDate] = None,
            totalAwardAmount: [BigDecimal] = 0.00,
            totalAwardProRataAmount: [BigDecimal] = 0.00,
            houseHoldAdviceAmount: [BigDecimal] = 0.00,
            totalHouseHoldAdviceProRataAmount: [BigDecimal] = 0.00,
            taxYears: [
                {
                    from: [LocalDate],
                    until: [LocalDate],
                    proRataEnd: Option[LocalDate] = None,
                    taxYearAwardAmount: [BigDecimal] = 0.00,
                    taxYearAwardProRataAmount: [BigDecimal] = 0.00,
                    taxYearAdviceAmount: [BigDecimal] = 0.00,
                    taxYearAdviceProRataAmount: [BigDecimal] = 0.00,
                    periods: [
                      {
                        from: [LocalDate],
                        until: [LocalDate],
                        periodNetAmount: [BigDecimal] = 0.00,
                        periodAdviceAmount: [BigDecimal] = 0.00,
                        elements: {
                            wtcWorkElement: {
                              netAmount: [BigDecimal] = 0.00,
                              maximumAmount: [BigDecimal] = 0.00,
                              taperAmount: [BigDecimal] = 0.00
                            },
                            wtcChildcareElement: {
                              netAmount: [BigDecimal] = 0.00,
                              maximumAmount: [BigDecimal] = 0.00,
                              taperAmount: [BigDecimal] = 0.00
                            },
                            ctcIndividualElement: {
                              netAmount: [BigDecimal] = 0.00,
                              maximumAmount: [BigDecimal] = 0.00,
                              taperAmount: [BigDecimal] = 0.00
                            },
                            ctcFamilyElement: {
                              netAmount: [BigDecimal] = 0.00,
                              maximumAmount: [BigDecimal] = 0.00,
                              taperAmount: [BigDecimal] = 0.00
                            }
                        }
                      }
                    ]
                }
            ]
        }
       }
     }
     ```


  * **Sample Input Request for TC:**

     ```javascript
     {
     "calculation": {
        "tc": {
            "from": "2015-09-27T00:00:00",
            "until": "2016-04-06T00:00:00",
            "proRataEnd": "2015-11-06T00:00:00",
            "totalAwardAmount": 2981.94,
            "totalAwardProRataAmount" :621.24,
            "houseHoldAdviceAmount": 0.00,
            "totalHouseHoldAdviceProRataAmount" :0.00,
            "taxYears": [
                {
                    "from": "2015-09-27T00:00:00",
                    "until": "2016-04-06T00:00:00",
                    "proRataEnd": "2015-11-06T00:00:00",
                    "taxYearAwardAmount": 2981.94,
                    "taxYearAwardProRataAmount" : 621.24,
                    "taxYearAdviceAmount": 0.00,
                    "taxYearAdviceProRataAmount" : 0.00,
                    "periods": [
                      {
                        "from": "2015-09-27T00:00:00",
                        "until":"2015-12-12T00:00:00",
                        "periodNetAmount": 2981.94,
                        "periodAdviceAmount": 0.00,
                        "elements": {
                            "wtcWorkElement": {
                              "netAmount": 93.24,
                              "maximumAmount": 994.08,
                              "taperAmount": 900.84
                            },
                            "wtcChildcareElement": {
                              "netAmount": 702.94,
                              "maximumAmount": 702.94,
                              "taperAmount": 0.00
                            },
                            "ctcIndividualElement": {
                              "netAmount": 2072.52,
                              "maximumAmount": 2072.52,
                              "taperAmount": 0.00
                            },
                            "ctcFamilyElement": {
                              "netAmount": 113.24,
                              "maximumAmount": 113.24,
                              "taperAmount": 0.00
                            }
                        }
                    }
                 ]
              }
           ]
        },
        "tfc": null,
        "esc": null
       }
     }
     ```

    * **Sample Success Response:**

      **Code:** 200
      **Content:**

      ```javascript
      {
        "calculation": {
         "tc": {
           "from": "2015-09-27T00:00:00",
           "until": "2016-04-06T00:00:00",
           "proRataEnd": "2015-11-06T00:00:00",
           "totalAwardAmount": 2981.94,
           "totalAwardProRataAmount" :621.24,
           "houseHoldAdviceAmount": 0.00,
           "totalHouseHoldAdviceProRataAmount" :0.00,
           "taxYears": [
            {
               "from": "2015-09-27T00:00:00",
               "until": "2016-04-06T00:00:00",
               "proRataEnd": "2015-11-06T00:00:00",
               "taxYearAwardAmount": 2981.94,
               "taxYearAwardProRataAmount" : 621.24,
               "taxYearAdviceAmount": 0.00,
                "taxYearAdviceProRataAmount" : 0.00,
                "periods": [
                 {
                  "from": "2015-09-27T00:00:00",
                  "until":"2015-12-12T00:00:00",
                   "periodNetAmount": 2981.94,
                   "periodAdviceAmount": 0.00,
                   "elements": {
                       "wtcWorkElement": {
                         "netAmount": 93.24,
                         "maximumAmount": 994.08,
                         "taperAmount": 900.84
                       },
                       "wtcChildcareElement": {
                         "netAmount": 702.94,
                         "maximumAmount": 702.94,
                         "taperAmount": 0.00
                       },
                       "ctcIndividualElement": {
                         "netAmount": 2072.52,
                         "maximumAmount": 2072.52,
                         "taperAmount": 0.00
                       },
                       "ctcFamilyElement": {
                         "netAmount": 113.24,
                         "maximumAmount": 113.24,
                         "taperAmount": 0.00
                       }
                     }
                 },
                 {
                   "from": "2015-12-12T00:00:00",
                   "until": "2016-04-06T00:00:00",
                   "periodNetAmount": 0.00,
                   "periodAdviceAmount": 0.00,
                   "elements": {
                     "wtcWorkElement": {
                         "netAmount": 0.00,
                         "maximumAmount":  879.28,
                         "taperAmount":  879.28
                       },
                       "wtcChildcareElement": {
                         "netAmount": 0.00,
                         "maximumAmount": 0.00,
                         "taperAmount": 0.00
                       },
                       "ctcIndividualElement": {
                         "netAmount": 0.00,
                         "maximumAmount": 0.00,
                         "taperAmount": 0.00
                         },
                       "ctcFamilyElement": {
                         "maximumAmount": 0.00,
                         "netAmount": 0.00,
                         "taperAmount": 0.00
                       }
                     }
                   }
                 ]
            }
           ]
         },
         "tfc": null,
         "esc": null
        }
      }
      ```
 * **Notes**
