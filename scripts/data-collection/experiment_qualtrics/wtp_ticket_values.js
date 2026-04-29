Qualtrics.SurveyEngine.addOnload(function()
{
	// Get embedded data values
	var treatment = Qualtrics.SurveyEngine.getEmbeddedData('treatment');
    var income = Qualtrics.SurveyEngine.getEmbeddedData('income');
	var red_amt = Qualtrics.SurveyEngine.getEmbeddedData('red_amt');
	var route_length = Qualtrics.SurveyEngine.getEmbeddedData('route_length');
	var flight_tour = parseInt(Qualtrics.SurveyEngine.getEmbeddedData('flight_tour'));
	var planned_flight = Qualtrics.SurveyEngine.getEmbeddedData('planned_flight');
    var limit_15_high = Qualtrics.SurveyEngine.getEmbeddedData('limit_15_high');
    var limit_30_high = Qualtrics.SurveyEngine.getEmbeddedData('limit_30_high');
    var limit_45_high = Qualtrics.SurveyEngine.getEmbeddedData('limit_45_high'); 

  // Define ticket cost and additional cost based on route
  var ticket_cost;

  if (route_length == "short") {
    ticket_cost = 150
  } else {
    ticket_cost = 400
  }

  var add_cost;

  // Egalitarian
  if (treatment === "egal") {
    if (route_length == "short") {
        if (red_amt === "15%") {
            if (income === "low") {
              add_cost = 9  
            } else if (income === "mid") {
                add_cost = 21
            } else if (income === "high") {
                add_cost = 36
            }  
        } else if (red_amt === "30%") {
            if (income === "low") {
                add_cost = 10  
              } else if (income === "mid") {
                  add_cost = 24
              } else if (income === "high") {
                  add_cost = 42
              } 
        } else if (red_amt === "45%") {
            if (income === "low") {
                add_cost = 11  
              } else if (income === "mid") {
                  add_cost = 28
              } else if (income === "high") {
                  add_cost = 48
              } 
        }
    } else if (route_length == "long") {
        if (red_amt === "15%") {
            if (income === "low") {
              add_cost = 28  
            } else if (income === "mid") {
                add_cost = 68
            } else if (income === "high") {
                add_cost = 117
            }  
        } else if (red_amt === "30%") {
            if (income === "low") {
                add_cost = 32  
              } else if (income === "mid") {
                  add_cost = 78
              } else if (income === "high") {
                  add_cost = 134
              } 
        } else if (red_amt === "45%") {
            if (income === "low") {
                add_cost = 37  
              } else if (income === "mid") {
                  add_cost = 90
              } else if (income === "high") {
                  add_cost = 155
              } 
        }   
    }
    // Limitarian
  } else if (treatment === "limit") {
    if (route_length == "short") {
        if (red_amt === "15%") {
            if (planned_flight > limit_15_high) {
              add_cost = 51 
            } else {
                add_cost = 0
            }  
        } else if (red_amt === "30%") {
            if (planned_flight > limit_30_high) {
                add_cost = 58 
              } else {
                  add_cost = 0
              } 
        } else if (red_amt === "45%") {
            if (planned_flight > limit_45_high) {
                add_cost = 67 
              } else {
                  add_cost = 0
              } 
        }
    } else if (route_length == "long") {
        if (red_amt === "15%") {
            if (planned_flight > limit_15_high) {
              add_cost = 163 
            } else {
                add_cost = 0
            }  
        } else if (red_amt === "30%") {
            if (planned_flight > limit_30_high) {
                add_cost = 187 
              } else {
                  add_cost = 0
              } 
        } else if (red_amt === "45%") {
            if (planned_flight > limit_45_high) {
                add_cost = 215 
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
              add_cost = 56 
            } else if (flight_tour === 0) {
                add_cost = 0
            }  
        } else if (red_amt === "30%") {
            if (flight_tour > 0) {
                add_cost = 64 
              } else if (flight_tour === 0) {
                  add_cost = 0
              } 
        } else if (red_amt === "45%") {
            if (flight_tour > 0) {
                add_cost = 74 
              } else if (flight_tour === 0) {
                  add_cost = 0
              } 
        }
    } else if (route_length == "long") {
        if (red_amt === "15%") {
            if (flight_tour > 0) {
              add_cost = 179
            } else if (flight_tour = 0) {
                add_cost = 0
            }  
        } else if (red_amt === "30%") {
            if (flight_tour > 0) {
                add_cost = 206 
              } else if (flight_tour = 0) {
                  add_cost = 0
              } 
        } else if (red_amt === "45%") {
            if (flight_tour > 0) {
                add_cost = 237 
              } else if (flight_tour = 0) {
                  add_cost = 0
              } 
        }   
    }
    // Proportional
  } else if (treatment === "prop") {
        if (route_length == "short") {
            if (red_amt === "15%") {
            add_cost = 28
        } else if (red_amt === "30%") {
            add_cost = 32
        } else if (red_amt === "45%") {
            add_cost = 37
        }
        }  
    else if (route_length == "long") {
        if (red_amt === "15%") {
        add_cost = 90
    } else if (red_amt === "30%") {
        add_cost = 103
    } else if (red_amt === "45%") {
        add_cost = 118
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

  // Save variables
  Qualtrics.SurveyEngine.setEmbeddedData("add_cost", add_cost);
	Qualtrics.SurveyEngine.setEmbeddedData("ticket_cost", ticket_cost);
	Qualtrics.SurveyEngine.setEmbeddedData("total_cost", total_cost);

  // Populate table with dynamic data
  document.getElementById("ticketPriceCell").textContent = "$" + ticket_cost;
  document.getElementById("emissionsReductionCell").textContent = red_amt;
  document.getElementById("additionalCostCell").textContent = "$" + add_cost;
  document.getElementById("totalCostCell").textContent = "$" + total_cost;


});