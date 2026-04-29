Qualtrics.SurveyEngine.addOnload(function() {
    // Retrieve embedded data values for 'red_amt' and 'treatment'
    var route_length = Qualtrics.SurveyEngine.getEmbeddedData('route_length');
    
    // Define a variable to hold the image URL
    var imageUrl = "";

    // Determine the correct image based on 'red_amt' and 'treatment'
    if (route_length == "short") {
      imageUrl = "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_dbPT9K448cPueQI";   
    } else if (route_length == "long") {
      imageUrl = "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_rkFTqSdzGAy4Ash";
    } 


	console.log("Route length: ", route_length);
	
    // Create an image element
    //var image_treatment_p = '<img src="' + imageUrl + '" alt="Dynamic Image" />';

	var imageContainer = document.getElementById('imageDisplay');
    imageContainer.innerHTML = '<img src="' + imageUrl + '" alt="Random Image" style="width: 80%; height: auto;">';

	
    // Display the image in the question
    //jQuery("#" + questionId + " .image-container").html(image_treatment_p);
});