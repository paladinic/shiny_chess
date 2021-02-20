$(document).keyup(function(event) {
    if ($("#username").is(":focus") && (event.key == "Enter")) {
        $("#go").click();
    }
});
