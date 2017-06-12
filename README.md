# cc-calculator

[![Build Status](https://travis-ci.org/hmrc/cc-calculator.svg?branch=master)](https://travis-ci.org/hmrc/cc-calculator) [ ![Download](https://api.bintray.com/packages/hmrc/releases/cc-calculator/images/download.svg) ](https://bintray.com/hmrc/releases/cc-calculator/_latestVersion)

This is a placeholder README.md for a new repository

Childcare Calculator microservice calculates actual childcare costs per different scheme (Tax Credits, Employer Supported Childcare and Tax Free Childcare).

Microservice consists of 3 different parts, each having separate endpoints and a different input/output structure.

The service will accept a JSON request formatted according to Eligibility Microservice's output with additional fields added which are required for calculation. The response of the service is also of a form of a structured JSON document.


* **Endpoint URLs**

  * **TC (Income Advice Calculation):**   /tax-credits/calculate/income-advice  <br />
  * **TC (Total Award Calculation):**   /tax-credits/calculate/total-award  <br />
  * **ESC :**  /employer-supported-childcare/calculate <br />
  * **TFC :**  /tax-free-childcare/calculate <br />

* **Port Number**

  * **CC Calculator:** 9372

* **API Documentation for each service:**

    * **[Tax Credits Calculation](/README_TC.md)**
    * **[Employer Supported Childcare Calculation](/README_ESC.md)**
    * **[Tax Free Childcare Calculation](/README_TFC.md)**



* **Method:**

  All requests are of type `POST`


* **Sample Input Request for TC:**

   ```javascript
   {
     "payload": {
       "eligibility": {
         "tc": {
           "taxYears": [
             {
               "from": "2016-09-27T18:46:17",
               "until": "2017-04-06T18:46:17",
               "houseHoldIncome" : 17000.00,
               "periods": [
                 {
                   "from": "2016-09-27T18:46:17",
                   "until": "2017-04-06T18:46:17",
                   "householdElements": {
                     "basic": true,
                     "hours30": false,
                     "childcare": true,
                     "loneParent": true,
                     "secondParent": false,
                     "family": true
                   },
                   "claimants": [
                     {
                       "qualifying": true,
                       "isPartner": false,
                       "claimantElements": {
                         "disability": false,
                         "severeDisability": false
                       }
                     }
                   ],
                   "children": [
                     {
                       "id": 0,
                       "name": "Adam",
                       "qualifying": true,
                       "childcareCost": 200.00,
                       "childcareCostPeriod": "Month",
                       "childElements": {
                         "child": true,
                         "youngAdult": false,
                         "disability": false,
                         "severeDisability": false,
                         "childcare": true
                       }
                     }
                   ]
                 }
               ]
             }
           ]
         },
         "esc": null,
         "tfc": null
       }
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


* **Example Error Response:**

  **Code:** 400
  **Content:**

  ```javascript
    {
      "status": 400,
      "errors":
      [
         {
          "path" : "/payload/eligibility/tc/taxYears(0)/periods(0)/children(0)/id",
           "validationErrors" :
           [
              {
                  "message": "ID should not be less than 0",
                  "args": []
              }
           ]
         }
      ]
    }
    ```

     OR

    **Code:** 500
    **Content:**

    ```javascript
    {
        "status": 500,
        "error": "Something bad happened"
    }
    ```

License
---

This code is open source software licensed under the [Apache 2.0 License]("http://www.apache.org/licenses/LICENSE-2.0.html").
