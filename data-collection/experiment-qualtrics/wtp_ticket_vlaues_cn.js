Qualtrics.SurveyEngine.addOnload(function()
{
	// Get embedded data values
	var treatment = Qualtrics.SurveyEngine.getEmbeddedData('treatment');
    var income = Qualtrics.SurveyEngine.getEmbeddedData('income');
    var red_amt = Qualtrics.SurveyEngine.getEmbeddedData('red_amt');
	var route_length = Qualtrics.SurveyEngine.getEmbeddedData('route_length');
	var planned_flight = parseInt(Qualtrics.SurveyEngine.getEmbeddedData('planned_flight'));
	var flight_tour = parseInt(Qualtrics.SurveyEngine.getEmbeddedData('flight_tour'));
    var limit_high = parseInt(Qualtrics.SurveyEngine.getEmbeddedData('limit_high'));


  // Define ticket cost and additional cost based on route
  var ticket_cost;

  if (route_length == "short") {
    ticket_cost = 1000
  } else {
    ticket_cost = 2140
  }

  var add_cost;

  // Egalitarian
  if (treatment === "egal") {
    if (route_length == "short") {
        if (red_amt === "15%") {
            if (income === "low") {
              add_cost = 61  
            } else if (income === "mid") {
                add_cost = 149
            } else if (income === "high") {
                add_cost = 257
            }  
        } else if (red_amt === "30%") {
            if (income === "low") {
                add_cost = 70
              } else if (income === "mid") {
                  add_cost = 172
              } else if (income === "high") {
                  add_cost = 295
              } 
        } else if (red_amt === "45%") {
            if (income === "low") {
                add_cost = 80  
              } else if (income === "mid") {
                  add_cost = 197
              } else if (income === "high") {
                  add_cost = 339
              } 
        }
    } else if (route_length == "long") {
        if (red_amt === "15%") {
            if (income === "low") {
              add_cost = 194 
            } else if (income === "mid") {
                add_cost = 479
            } else if (income === "high") {
                add_cost = 823
            }  
        } else if (red_amt === "30%") {
            if (income === "low") {
                add_cost = 224  
              } else if (income === "mid") {
                  add_cost = 550
              } else if (income === "high") {
                  add_cost = 946
              } 
        } else if (red_amt === "45%") {
            if (income === "low") {
                add_cost = 257  
              } else if (income === "mid") {
                  add_cost = 633
              } else if (income === "high") {
                  add_cost = 1087
              } 
        }   
    }
    // Limitarian
  } else if (treatment === "limit") {
    if (route_length == "short") {
        if (red_amt === "15%") {
            if (planned_flight > limit_high) {
              add_cost = 356 
            } else {
                add_cost = 0
            }  
        } else if (red_amt === "30%") {
            if (planned_flight > limit_high) {
                add_cost = 410 
              } else {
                  add_cost = 0
              } 
        } else if (red_amt === "45%") {
            if (planned_flight > limit_high) {
                add_cost = 471 
              } else {
                  add_cost = 0
              } 
        }
    } else if (route_length == "long") {
        if (red_amt === "15%") {
            if (planned_flight > limit_high) {
              add_cost = 1147 
            } else {
                add_cost = 0
            }  
        } else if (red_amt === "30%") {
            if (planned_flight > limit_high) {
                add_cost = 1319 
              } else {
                  add_cost = 0
              } 
        } else if (red_amt === "45%") {
            if (planned_flight > limit_high) {
                add_cost = 1516 
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
              add_cost = 392 
            } else if (flight_tour === 0) {
                add_cost = 0
            }  
        } else if (red_amt === "30%") {
            if (flight_tour > 0) {
                add_cost = 451 
              } else if (flight_tour === 0) {
                  add_cost = 0
              } 
        } else if (red_amt === "45%") {
            if (flight_tour > 0) {
                add_cost = 518 
              } else if (flight_tour === 0) {
                  add_cost = 0
              } 
        }
    } else if (route_length == "long") {
        if (red_amt === "15%") {
            if (flight_tour > 0) {
              add_cost = 1262
            } else if (flight_tour = 0) {
                add_cost = 0
            }  
        } else if (red_amt === "30%") {
            if (flight_tour > 0) {
                add_cost = 1451 
              } else if (flight_tour = 0) {
                  add_cost = 0
              } 
        } else if (red_amt === "45%") {
            if (flight_tour > 0) {
                add_cost = 1668 
              } else if (flight_tour = 0) {
                  add_cost = 0
              } 
        }   
    }
    // Proportional
  } else if (treatment === "prop") {
        if (route_length == "short") {
            if (red_amt === "15%") {
            add_cost = 196
        } else if (red_amt === "30%") {
            add_cost = 225
        } else if (red_amt === "45%") {
            add_cost = 259
        }
        }  
    else if (route_length == "long") {
        if (red_amt === "15%") {
        add_cost = 631
    } else if (red_amt === "30%") {
        add_cost = 725
    } else if (red_amt === "45%") {
        add_cost = 834
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
  document.getElementById("ticketPriceCell").textContent = ticket_cost + "元";
  document.getElementById("emissionsReductionCell").textContent = red_amt;
  document.getElementById("additionalCostCell").textContent = add_cost + "元";
  document.getElementById("totalCostCell").textContent = total_cost + "元";


});