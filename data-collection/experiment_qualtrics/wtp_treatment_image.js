Qualtrics.SurveyEngine.addOnload(function() {
    //hides the next button on a page
this.hidePreviousButton();
	
	// Retrieve embedded data values for 'red_amt' and 'treatment'
    var reduction = Qualtrics.SurveyEngine.getEmbeddedData('red_amt');
    var treatment = Qualtrics.SurveyEngine.getEmbeddedData('treatment');
    var route_length = Qualtrics.SurveyEngine.getEmbeddedData('route_length');
    var questionId = this.getQuestionContainer().id;
    
    // Define a variable to hold the image URL
    var imageUrl = "";

    // Determine the correct image based on 'red_amt' and 'treatment'
    if (route_length == "short") {
      if (reduction == "15%") {
        if (treatment == "egal") {
        imageUrl = "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_fnZbS3wXAoU7djd";    
        } else if (treatment == "limit") {
        imageUrl = "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_ChC8G6qyIXwiqbE";
        } else if (treatment == "prior") {
        imageUrl = "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_Ch7jZj2mZDDJ24r";
        } else if (treatment == "prop") {
        imageUrl = "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_1xoTO70Ln1mEdl2";
        }
    } else if (reduction == "30%") {
        if (treatment == "egal") {
            imageUrl = "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_d4pZANizj3ZvT9y";    
            } else if (treatment == "limit") {
            imageUrl = "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_E9yq60Q4IeYav9R";
            } else if (treatment == "prior") {
            imageUrl = "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_Toa7EXp1HIB9uKU";
            } else if (treatment == "prop") {
            imageUrl = "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_mohT3gasMQymQrO";
            }
    } else if (reduction == "45%") {
        if (treatment == "egal") {
            imageUrl = "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_lwhVVVrUFtVyeWC";    
            } else if (treatment == "limit") {
            imageUrl = "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_A5WFcEz2wQA4rGZ";
            } else if (treatment == "prior") {
            imageUrl = "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_Ynd0ui8LWDl2N1C";
            } else if (treatment == "prop") {
            imageUrl = "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_YfKP3pK1p8k0pzA";
            }
    }  
    } else if (route_length == "long") {
        if (reduction == "15%") {
            if (treatment == "egal") {
            imageUrl = "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_I3ZYYWQCPHDyvjK";    
            } else if (treatment == "limit") {
            imageUrl = "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_TQDlA5br6BWSpTN";
            } else if (treatment == "prior") {
            imageUrl = "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_BLBoK5WgHce1jxv";
            } else if (treatment == "prop") {
            imageUrl = "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_x5TAiOLf7JcUdmZ";
            }
        } else if (reduction == "30%") {
            if (treatment == "egal") {
                imageUrl = "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_iPtFTmjlY5MMQo6";    
                } else if (treatment == "limit") {
                imageUrl = "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_nSJH36gW0VZc5H8";
                } else if (treatment == "prior") {
                imageUrl = "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_HM6fYOQiVvigQIw";
                } else if (treatment == "prop") {
                imageUrl = "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_MLcE1j9GRMBkymI";
                }
        } else if (reduction == "45%") {
            if (treatment == "egal") {
                imageUrl = "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_p2SvV6k8thoK4Ab";    
                } else if (treatment == "limit") {
                imageUrl = "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_9jVaR79fwzvcbVd";
                } else if (treatment == "prior") {
                imageUrl = "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_zl9sjiRahlaeEcd";
                } else if (treatment == "prop") {
                imageUrl = "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_jS1DrJgcAdDn2XI";
                }
        } 
    }
    

    console.log("Question ID: ", questionId);
	console.log("Image: ", imageUrl);

    // Create an image element
    //var imgElement = '<img src="' + imageUrl + '" style="width: 75%; height: auto;" alt="Dynamic Image" />';
	var imageContainer = document.getElementById('imageDisplay');
    imageContainer.innerHTML = '<img src="' + imageUrl + '" alt="Random Image" style="width: 450px; height: auto;">';

    // Display the image in the question
    //jQuery("#image-container-" + questionId).html(imgElement);
});