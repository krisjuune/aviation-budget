Qualtrics.SurveyEngine.addOnload(function () {
    // Use a flag to ensure this block runs only once
    if (window.tripSelectionExecuted) {
        console.log("Trip selection already executed. Skipping.");
        return;
    }
    window.tripSelectionExecuted = true;

    // Define the possible statements and their corresponding route lengths
    var statements = [
        {
            text: "Imagine you are planning a trip from Los Angeles to San Francisco and would like to fly there.",
            route_length: "short"
        },
        {
            text: "Imagine you are planning a trip from New York to London and would like to fly there.",
            route_length: "long"
        }
    ];

    // Randomly select one statement
    var randomIndex = Math.floor(Math.random() * statements.length);
    var selected = statements[randomIndex];

    // Log the random index and selected statement for debugging
    console.log("Random Index:", randomIndex);
    console.log("Selected Statement:", selected);

    // Find the <div> with id "trip_statement"
    var tripStatementDiv = document.getElementById("trip_statement");

    // Check if the div exists before trying to modify it
    if (tripStatementDiv) {
        // Update the div with the selected statement
        tripStatementDiv.innerHTML = selected.text;

        // Ensure the text is bold (the inline style already has bold, so this is optional)
        tripStatementDiv.style.fontWeight = "bold";

        // Save the statement and route length to embedded data
        Qualtrics.SurveyEngine.setEmbeddedData("trip_statement", selected.text);
        Qualtrics.SurveyEngine.setEmbeddedData("route_length", selected.route_length);
    } else {
        console.error("Div with id 'trip_statement' not found.");
    }

    // Log the statement and route length being saved
    console.log("Embedded Data - trip_statement:", selected.text);
    console.log("Embedded Data - route_length:", selected.route_length);
});