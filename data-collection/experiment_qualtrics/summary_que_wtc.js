Qualtrics.SurveyEngine.addOnload(function()
{
// Retrieve the embedded data variable `red_amt`
var red_amt = Qualtrics.SurveyEngine.getEmbeddedData('red_amt');

// Define the image URLs for each reduction amount
var images15 = [
    "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_VRTiIUPmuJpCMTi",
    "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_zGA6gBdt6I6iX82",
    "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_F4mQqgPQO3VTMDJ",
    "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_6jDa1lkiWqklxx2"
];

var images30 = [
    "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_43hbHN9T47KIyIW",
    "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_2HX9rtehdD0xhct",
    "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_DN4OVZmuEj8OVf1",
    "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_wxCEzagwVLd4qDf"
];

var images45 = [
    "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_QPSlrRuXn9uimSu",
    "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_GgmBgiBK6kgvck1",
    "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_UQBZE53jKI0Q29k",
    "https://climatepolicy.qualtrics.com/ControlPanel/Graphic.php?IM=IM_drBUvuDuabCG7uz"
];

// Select the appropriate image set based on `red_amt`
var selectedImages;
if (red_amt === "15%") {
    selectedImages = images15;
} else if (red_amt === "30%") {
    selectedImages = images30;
} else if (red_amt === "45%") {
    selectedImages = images45;
} else {
    console.warn("Unexpected red_amt value: " + red_amt);
    selectedImages = [];  // Default to an empty array in case of an invalid value
}

// Set the image sources
if (selectedImages.length > 0) {
    document.getElementById("img1").src = selectedImages[0];
    document.getElementById("img2").src = selectedImages[1];
    document.getElementById("img3").src = selectedImages[2];
    document.getElementById("img4").src = selectedImages[3];
}

});