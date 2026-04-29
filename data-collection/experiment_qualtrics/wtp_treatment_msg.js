Qualtrics.SurveyEngine.addOnload(function()
{
	 //hides the next button on a page
this.hidePreviousButton();
	
	var treatment = Qualtrics.SurveyEngine.getEmbeddedData('treatment');
	
	 var message = "";
    if (treatment == "egal") {
        message = "<strong>The additional cost of the ticket depends on one’s income.</strong>.";
    } else if (treatment == "limit") {
        message = "<strong>Only frequent flyers pay for the extra cost of the ticket.</strong>.";
    } else if (treatment == "prior") {
        message = "<strong>Only tickets for tourism flights include the extra cost.</strong>.";
    } else if (treatment == "prop") {
	    message = "<strong>Everyone pays for the additional cost of the flight ticket.</strong>"
	}

    document.getElementById("wtp_allocation_message").innerHTML = message;

});