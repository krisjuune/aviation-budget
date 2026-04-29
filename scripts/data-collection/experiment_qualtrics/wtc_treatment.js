Qualtrics.SurveyEngine.addOnload(function() {
    // Define block-specific images
    var egal_images = [
        "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_VRTiIUPmuJpCMTi", 
        "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_43hbHN9T47KIyIW", 
        "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_QPSlrRuXn9uimSu"
    ];
    var limit_images = [
        "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_zGA6gBdt6I6iX82", 
        "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_2HX9rtehdD0xhct", 
        "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_GgmBgiBK6kgvck1"
    ];
    var prior_images = [
        "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_F4mQqgPQO3VTMDJ", 
        "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_DN4OVZmuEj8OVf1", 
        "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_UQBZE53jKI0Q29k"
    ];
    var prop_images = [
        "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_6jDa1lkiWqklxx2", 
        "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_wxCEzagwVLd4qDf", 
        "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_drBUvuDuabCG7uz"
    ];

    // Identify the block based on the assigned block (assigned in survey flow)
    var treatment = Qualtrics.SurveyEngine.getEmbeddedData('treatment');
    var images;

    if (treatment == "egal") {
        images = egal_images;
    } else if (treatment == "limit") {
        images = limit_images;
    } else if (treatment == "prior") {
        images = prior_images;
    } else if (treatment == "prop") {
        images = prop_images;
    }

    // Randomly pick an image index (0, 1, or 2)
    var randomIndex = Math.floor(Math.random() * images.length);

    // Get the image URL based on the random index
    var chosenImage = images[randomIndex];

    // Display the chosen image
    var imageContainer = document.getElementById('imageDisplay');
    imageContainer.innerHTML = '<img src="' + chosenImage + '" alt="Random Image" style="width: 450px; height: auto;">';

    // Save the selected image URL to an embedded data field
    Qualtrics.SurveyEngine.setEmbeddedData("random_image", chosenImage);

    // Now, check which percentage group the selected image corresponds to and save to "red_amt"
    var red_amt = "";  // Variable to hold the percentage value

    // Define the images corresponding to each percentage
    var pct_15_images = [
        "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_VRTiIUPmuJpCMTi",
        "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_zGA6gBdt6I6iX82",
        "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_F4mQqgPQO3VTMDJ",
        "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_6jDa1lkiWqklxx2"
    ];
    var pct_30_images = [
        "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_43hbHN9T47KIyIW",
        "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_2HX9rtehdD0xhct",
        "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_DN4OVZmuEj8OVf1",
        "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_wxCEzagwVLd4qDf"
    ];
    var pct_45_images = [
        "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_QPSlrRuXn9uimSu",
        "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_GgmBgiBK6kgvck1",
        "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_UQBZE53jKI0Q29k",
        "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_drBUvuDuabCG7uz"
    ];

    // Check if the chosen image belongs to 15%, 30%, or 45% group
    if (pct_15_images.includes(chosenImage)) {
        red_amt = "15%";
    } else if (pct_30_images.includes(chosenImage)) {
        red_amt = "30%";
    } else if (pct_45_images.includes(chosenImage)) {
        red_amt = "45%";
    }

    // Save the percentage amount to the "red_amt" embedded data field
    Qualtrics.SurveyEngine.setEmbeddedData("red_amt", red_amt);
});