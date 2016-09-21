# TFC Calculator

Tax Free Childcare Calculator accepts Eligibility Microservice's output result with additional fields added for calculation.

* **Endpoint URLs:**

  * **TFC :**  /tax-free-childcare/calculate <br />


* **Data Parameters:**

  `Data structure for TFC input json:`

    ```javascript
    {
          payload: {
            eligibility: {
              tfc: {
                from: [LocalDate],
                until: [LocalDate],
                householdEligibility: [Boolean] = true,
                periods: [
                    {
                      from: [LocalDate],
                      until: [LocalDate],
                      periodEligibility: [Boolean] = true,
                      children: [
                        {
                          id: [Short],
                          name: [String] = None,
                          qualifying: [Boolean] = true,
                          from: [LocalDate],
                          until: [LocalDate],
                          childcareCost: [BigDecimal] = 0.00,
                          childcareCostPeriod: [Enumeration]
                             {
                                Week,
                                Fortnight,
                                Month,
                                3 month,
                                Year,
                                INVALID
                             },
                          disability:
                          {
                            disability: [Boolean] = false,
                            severeDisability: [Boolean] = false,
                          }
                        }
                      ]
                    }
                ]
              }
            }
          }
        }
    ```

  `Data structure for TFC output json:`

     ```javascript
    {
        calculation : {
          tfc: {
            from: [LocalDate],
            until: [LocalDate],
            householdContribution: {
                  parent: [BigDecimal] = 0.00,
                  government: [BigDecimal] = 0.00,
                  totalChildCareSpend: [BigDecimal] = 0.00
                  },
            numberOfPeriods: [Short] = 0,
            periods: [
                {
            from: [LocalDate],
            until: [LocalDate],
            periodContribution: {
                parent: [BigDecimal] = 0.00,
                government: [BigDecimal] = 0.00,
                totalChildCareSpend: [BigDecimal] = 0.00
                },
            children: [
              {
               id: [Short],
               name: [String] = None,
               childcareCost: [BigDecimal] = 0.00,
               childcareCostPeriod: [Enumeration] {
                   Week,
                   Fortnight,
                   Month,
                   3 month,
                   Year,
                   INVALID
                   },
               childContribution: {
                  parent: [BigDecimal] = 0.00,
                  government: [BigDecimal] = 0.00,
                  totalChildCareSpend: [BigDecimal] = 0.00
                },
                timeToMaximizeTopUp : [BigDecimal] = 0.00,
                failures : []
              }
            ]
          }
        ]
      }
    }
  }
  ```


* **Sample Input Request for TFC:**

 ```javascript
{
  "payload": {
    "eligibility": {
      "tfc":{
        "from": "2016-08-27",
        "until": "2016-11-27",
        "householdEligibility": true,
        "periods": [
          {
            "from": "2016-08-27",
            "until": "2016-11-27",
            "periodEligibility": true,
            "children": [
              {
                "id": 0,
                "name": "Chandan",
                "qualifying": true,
                "from": "2016-08-27",
                "until": "2016-11-27",
                "childcareCost": 3000.00,
                "disability": {
                  "disabled": false,
                  "severelyDisabled": false
                }
              }

            ]
          }
          ]
      },
      "tc": null,
      "esc": null
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
    "tfc": {
      "from": "2016-08-27",
      "until": "2016-11-27",
      "householdContribution": {
        "parent": 8500.00,
        "government": 500.00,
        "totalChildCareSpend": 9000.00
      },
      "numberOfPeriods" : 1,
      "periods" : [
        {
          "from": "2016-08-27",
          "until": "2016-11-27",
          "periodContribution": {
            "parent": 8500.00,
            "government": 500.00,
            "totalChildCareSpend": 9000.00
          },
          "children": [
            {
              "id": 0,
              "name" : "Chandan",
              "childCareCost": 3000.00,
              "childContribution" : {
                "parent": 8500.00,
                "government": 500.00,
                "totalChildCareSpend": 9000.00
              },
              "timeToMaximizeTopUp" : 0,
              "failures" : []
            }
          ]
        }
      ]
    },
    "esc": null,
    "tc": null
  }
}
```

* **Notes**
