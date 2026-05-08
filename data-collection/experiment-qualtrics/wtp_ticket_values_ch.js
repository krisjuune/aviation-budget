Qualtrics.SurveyEngine.addOnload(function()
{
	// Get embedded data values
	var treatment = Qualtrics.SurveyEngine.getEmbeddedData('treatment');
    var income = Qualtrics.SurveyEngine.getEmbeddedData('income');
	var red_amt = Qualtrics.SurveyEngine.getEmbeddedData('red_amt');
	var route_length = Qualtrics.SurveyEngine.getEmbeddedData('route_length');
    var planned_flight = Qualtrics.SurveyEngine.getEmbeddedData('planned_flight');
	var flight_tour = parseInt(Qualtrics.SurveyEngine.getEmbeddedData('flight_tour'));
    var limit_15_high = Qualtrics.SurveyEngine.getEmbeddedData('limit_15_high');
    var limit_30_high = Qualtrics.SurveyEngine.getEmbeddedData('limit_30_high');
    var limit_45_high = Qualtrics.SurveyEngine.getEmbeddedData('limit_45_high'); 

  // Define ticket cost and additional cost based on route
  var ticket_cost;

  if (route_length == "short") {
    ticket_cost = 130
  } else {
    ticket_cost = 350
  }

  var add_cost;

  // Egalitarian
  if (treatment === "egal") {
    if (route_length == "short") {
        if (red_amt === "15%") {
            if (income === "low") {
              add_cost = 7  
            } else if (income === "mid") {
                add_cost = 18
            } else if (income === "high") {
                add_cost = 31
            }  
        } else if (red_amt === "30%") {
            if (income === "low") {
                add_cost = 8  
              } else if (income === "mid") {
                  add_cost = 21
              } else if (income === "high") {
                  add_cost = 36
              } 
        } else if (red_amt === "45%") {
            if (income === "low") {
                add_cost = 10  
              } else if (income === "mid") {
                  add_cost = 24
              } else if (income === "high") {
                  add_cost = 41
              } 
        }
    } else if (route_length == "long") {
        if (red_amt === "15%") {
            if (income === "low") {
              add_cost = 24  
            } else if (income === "mid") {
                add_cost = 58
            } else if (income === "high") {
                add_cost = 100
            }  
        } else if (red_amt === "30%") {
            if (income === "low") {
                add_cost = 27  
              } else if (income === "mid") {
                  add_cost = 67
              } else if (income === "high") {
                  add_cost = 115
              } 
        } else if (red_amt === "45%") {
            if (income === "low") {
                add_cost = 31  
              } else if (income === "mid") {
                  add_cost = 77
              } else if (income === "high") {
                  add_cost = 132
              } 
        }   
    }
    // Limitarian
  } else if (treatment === "limit") {
    if (route_length == "short") {
        if (red_amt === "15%") {
            if (planned_flight > limit_15_high) {
              add_cost = 43 
            } else {
                add_cost = 0
            }  
        } else if (red_amt === "30%") {
            if (planned_flight > limit_30_high) {
                add_cost = 50 
              } else {
                  add_cost = 0
              } 
        } else if (red_amt === "45%") {
            if (planned_flight > limit_45_high) {
                add_cost = 57 
              } else {
                  add_cost = 0
              } 
        }
    } else if (route_length == "long") {
        if (red_amt === "15%") {
            if (planned_flight > limit_15_high) {
              add_cost = 139 
            } else {
                add_cost = 0
            }  
        } else if (red_amt === "30%") {
            if (planned_flight > limit_30_high) {
                add_cost = 160 
              } else {
                  add_cost = 0
              } 
        } else if (red_amt === "45%") {
            if (planned_flight > limit_45_high) {
                add_cost = 184 
              } else {
                  add_cost = 0
              } 
        }   
    }
    // Prioritarian
  } else if (treatment === "prior") {
    if (route_length == "short") {
        if (red_amt === "15%") {
            if (flight_tour > 0) {
              add_cost = 48 
            } else if (flight_tour === 0) {
                add_cost = 0
            }  
        } else if (red_amt === "30%") {
            if (flight_tour > 0) {
                add_cost = 55 
              } else if (flight_tour === 0) {
                  add_cost = 0
              } 
        } else if (red_amt === "45%") {
            if (flight_tour > 0) {
                add_cost = 63 
              } else if (flight_tour === 0) {
                  add_cost = 0
              } 
        }
    } else if (route_length == "long") {
        if (red_amt === "15%") {
            if (flight_tour > 0) {
              add_cost = 153
            } else if (flight_tour = 0) {
                add_cost = 0
            }  
        } else if (red_amt === "30%") {
            if (flight_tour > 0) {
                add_cost = 176 
              } else if (flight_tour = 0) {
                  add_cost = 0
              } 
        } else if (red_amt === "45%") {
            if (flight_tour > 0) {
                add_cost = 203 
              } else if (flight_tour = 0) {
                  add_cost = 0
              } 
        }   
    }
    // Proportional
  } else if (treatment === "prop") {
        if (route_length == "short") {
            if (red_amt === "15%") {
            add_cost = 24
        } else if (red_amt === "30%") {
            add_cost = 27
        } else if (red_amt === "45%") {
            add_cost = 31
        }
        }  
    else if (route_length == "long") {
        if (red_amt === "15%") {
        add_cost = 77
    } else if (red_amt === "30%") {
        add_cost = 88
    } else if (red_amt === "45%") {
        add_cost = 101
    }
    } 
  }

  var total_cost;
  total_cost = ticket_cost + add_cost;

  console.log("Ticket cost: ", ticket_cost);
  console.log("Additional cost: ", add_cost);
  console.log("Route length: ", route_length);
  console.log("Treatment: ", treatment);
  console.log("Income group: ", income);
  console.log("Reduction amount: ", red_amt);
	
	Qualtrics.SurveyEngine.setEmbeddedData("ticket_cost", ticket_cost);
	Qualtrics.SurveyEngine.setEmbeddedData("total_cost", total_cost);

  // Populate table with dynamic data
  document.getElementById("ticketPriceCell").textContent = "CHF " + ticket_cost;
  document.getElementById("emissionsReductionCell").textContent = red_amt;
  document.getElementById("additionalCostCell").textContent = "CHF " + add_cost;
  document.getElementById("totalCostCell").textContent = "CHF " + total_cost;


});