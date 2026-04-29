Qualtrics.SurveyEngine.addOnload(function()
{
	var planned_flight = parseInt(Qualtrics.SurveyEngine.getEmbeddedData('planned_flight'));

    var message = "";
    if (planned_flight === 1) {
        message = "On the previous page, you said that you are thinking about taking " + planned_flight + " flight in the next 12 months.";
    } else if (planned_flight > 1) {
        message = "On the previous page, you said that you are thinking about taking " + planned_flight + " flights in the next 12 months.";
    }

    document.getElementById("planned_flight_message").innerHTML = message;
});