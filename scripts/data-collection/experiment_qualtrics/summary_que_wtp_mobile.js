Qualtrics.SurveyEngine.addOnload(function()
{
	// Retrieve the embedded data variable `red_amt`
    var red_amt = Qualtrics.SurveyEngine.getEmbeddedData('red_amt');
	var route_length = Qualtrics.SurveyEngine.getEmbeddedData('route_length');

    // Define image URLs for each combination of `red_amt` and `route_length`
    var images = {
        "short": {
            "15%": [
                "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_fnZbS3wXAoU7djd",
                "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_ChC8G6qyIXwiqbE",
                "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_Ch7jZj2mZDDJ24r",
                "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_1xoTO70Ln1mEdl2"
            ],
            "30%": [
                "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_d4pZANizj3ZvT9y",
                "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_E9yq60Q4IeYav9R",
                "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_Toa7EXp1HIB9uKU",
                "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_mohT3gasMQymQrO"
            ],
            "45%": [
                "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_lwhVVVrUFtVyeWC",
                "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_A5WFcEz2wQA4rGZ",
                "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_Ynd0ui8LWDl2N1C",
                "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_YfKP3pK1p8k0pzA"
            ]
        },
        "long": {
            "15%": [
                "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_I3ZYYWQCPHDyvjK",
                "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_TQDlA5br6BWSpTN",
                "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_BLBoK5WgHce1jxv",
                "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_x5TAiOLf7JcUdmZ"
            ],
            "30%": [
                "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_iPtFTmjlY5MMQo6",
                "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_nSJH36gW0VZc5H8",
                "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_HM6fYOQiVvigQIw",
                "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_MLcE1j9GRMBkymI"
            ],
            "45%": [
                "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_p2SvV6k8thoK4Ab",
                "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_9jVaR79fwzvcbVd",
                "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_zl9sjiRahlaeEcd",
                "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_jS1DrJgcAdDn2XI"
            ]
        }
    };

    // Set default images if `red_amt` or `route_length` are unexpected
    var selectedImages = images[route_length] && images[route_length][red_amt] ? images[route_length][red_amt] : [];

    // Apply images to the img elements
    if (selectedImages.length > 0) {
        document.getElementById("img1").src = selectedImages[0];
        document.getElementById("img2").src = selectedImages[1];
        document.getElementById("img3").src = selectedImages[2];
        document.getElementById("img4").src = selectedImages[3];
    } else {
        console.warn("Invalid red_amt or route_length value");
    }

	
	console.log("Reduction: ", red_amt);
	console.log("Route length: ", route_length);
	
});