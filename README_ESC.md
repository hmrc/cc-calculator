# ESC Calculator

Employer Supported Childcare Calculator accepts Eligibility Microservice's output result with additional fields added for calculation.

* **Endpoint URLs:**

  * **ESC :**  /employer-supported-childcare/calculate <br />


* **Data Parameters:**

  `Data structure for ESC input json:`

    ```javascript
    {
          payload: {
            eligibility: {
              esc: {
                taxYears: [
                  {
                    startDate: [LocalDate],
                    endDate: [LocalDate],
                    periods: [
                      {
                        from: [LocalDate],
                        until: [LocalDate],
                        claimants: [
                          {
                            qualifying: [Boolean] = false
                            isPartner: [Boolean] = false
                            eligibleMonthsInPeriod: [Int],
                            income: {
                              taxablePay: [BigDecimal] = 0.00,
                              gross: [BigDecimal] = 0.00,
                              taxCode: [String] = "",
                              niCategory: [String] = ""
                            },
                            elements: {
                              vouchers: [Boolean] = false
                            },
                            escAmount: [BigDecimal] = 0.00,
                            escAmountPeriod: [Enumeration] {
                                Week,
                                Fortnight,
                                Month,
                                3 month,
                                Year,
                                INVALID
                    	    },
                            escStartDate: [LocalDate],
                            failures: List[String]
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

  `Data structure for ESC output json:`

     ```javascript
     {
       calculation : {
         esc: {
           from: [LocalDate],
           until: [LocalDate],
           totalSavings: {
             totalSaving: [BigDecimal] = 0.00,
             taxSaving: [BigDecimal] = 0.00,
             niSaving: [BigDecimal] = 0.00
           },
           taxYears: [
             {
               from: [LocalDate],
               until: [LocalDate],
               totalSavings: {
                 totalSaving: [BigDecimal] = 0.00,
                 taxSaving: [BigDecimal] = 0.00,
                 niSaving: [BigDecimal] = 0.00
               },
               claimants: [
                 {
                   qualifying: [Boolean] = false,
                   eligibleMonthsInTaxYear: [Int],
                   isPartner: [Boolean] = false,
                   escAmount: [BigDecimal] = 0.00,
                   escAmountPeriod: [Enumeration] {
                    Week,
                    Fortnight,
                    Month,
                    3 month,
                    Year,
                    INVALID
                   },
                   escStartDate: [LocalDate],
                   maximumRelief: [BigDecimal] = 0.00,
                   maximumReliefPeriod: [Enumeration] {
                    Week,
                    Fortnight,
                    Month,
                    3 month,
                    Year,
                    INVALID
                   },
                   income: {
                     taxablePay: [BigDecimal] = 0.00,
                     gross: [BigDecimal] = 0.00,
                     taxCode: [String] = "",
                     niCategory: [String] = ""
                   },
                   elements: {
                     vouchers: [Boolean] = false
                   },
                   savings: {
                     totalSaving: [BigDecimal] = 0.00,
                     taxSaving: [BigDecimal] = 0.00,
                     niSaving: [BigDecimal] = 0.00
                   },
                   taxAndNIBeforeSacrifice: {
                     taxPaid: [BigDecimal] = 0.00,
                     niPaid: [BigDecimal] = 0.00
                   },
                   taxAndNIAfterSacrifice: {
                     taxPaid: [BigDecimal] = 0.00,
                     niPaid: [BigDecimal] = 0.00
                   }
                 }
               ]
             }
           ]
         }
       }
     }
     ```


  * **Sample Input Request for ESC:**

     ```javascript
     {
       "payload": {
         "eligibility": {
           "esc": {
             "taxYears": [
               {
                 "startDate": "2015-04-06T00:00:00",
                 "endDate": "2016-04-06T00:00:00",
                 "periods": [
                   {
                     "from": "2015-04-06T00:00:00",
                     "until": "2016-04-06T00:00:00",
                     "claimants": [
                       {
                         "qualifying": false,
                         "isPartner": false,
                         "eligibleMonthsInPeriod": 0,
                         "income": {
                           "taxablePay": 50000.00,
                           "gross": 50000.00,
                           "taxCode": 1060Y,
                           "niCategory": B
                         },
                         "elements": {
                           "vouchers": false
                         },
                         "escAmount": 250.00,
                         "escAmountPeriod": "Month",
                         "escStartDate": "2012-08-27T00:00:00",
                         "failures": []
                       }
                     ]
                   }
                 ]
               }
             ]
           },
           tc: null,
           tfc: null
         }
       }
     }
     ```

    * **Sample Success Response:**

      **Code:** 200
      **Content:**

      ```javascript
      {
        "calculation" : {
          "esc": {
            "from": "2015-04-06T00:00:00",
            "until": "2016-04-06T00:00:00",
            "totalSavings": {
              "totalSaving": 0.00,
              "taxSaving": 0.00,
              "niSaving": 0.00
            },
            "taxYears": [
              {
                "from": "2015-04-06T00:00:00",
                "until": "2016-04-06T00:00:00",
                "totalSavings": {
                  "totalSaving": 0.00,
                  "taxSaving": 0.00,
                  "niSaving": 0.00
                },
                "claimants": [
                  {
                    "qualifying": false,
                    "eligibleMonthsInTaxYear": 0,
                    "isPartner": false,
                    "escAmount": 250,
                    "escAmountPeriod": "Month",
                    "escStartDate":"2012-08-27T00:00:00",
                    "maximumRelief": 124,
                    "maximumReliefPeriod": "Month",
                    "income": {
                      "taxablePay": 50000,
                      "gross": 50000,
                      "taxCode": "1060Y",
                      "niCategory": "B"
                    },
                    "elements": {
                      "vouchers": false
                    },
                    "savings": {
                      "totalSaving": 0.00,
                      "taxSaving": 0.00,
                      "niSaving": 0.00
                    },
                    "taxAndNIBeforeSacrifice": {
                      "taxPaid": 783.8,
                      "niPaid": 180.01
                    },
                    "taxAndNIAfterSacrifice": {
                      "taxPaid": 734.2,
                      "niPaid": 177.53
                    }
                  }
                ]
              }
            ]
          },
          "tc": null,
          "tfc": null
        }
      }
      ```
 * **Notes**
