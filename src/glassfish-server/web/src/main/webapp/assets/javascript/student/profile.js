// function that show the hidden form, that send the post request for the meeting delete
// it also add the relative button to the selected meeeting, setting up the value
// to be submitted with the id of the meeting selected
function showDeleteMeetingAlert(meetingIdToDelete) {
    // disable of all the buttons of the course until the operation is finished
    // or the cancel button is clicked
    var meetingButtons = document.querySelectorAll('.remove_booked_meeting');
    Array.from(meetingButtons).forEach(function (button){
        button.disabled = true;
    });
    document.getElementById("delete-meeting-alert").style.display = "flex";
    var deleteMeetingForm = document.getElementById("delete-meeting-form");
    var submitButton = document.createElement("button");
    submitButton.innerHTML = "Delete";
    submitButton.setAttribute("type", "submit");
    submitButton.setAttribute("id", meetingIdToDelete);
    submitButton.setAttribute("name", "meeting_id");
    submitButton.setAttribute("value", meetingIdToDelete);
    submitButton.setAttribute("class", "delete-button");
    deleteMeetingForm.appendChild(submitButton);
}

// function that hide the form, if the cancel button is clicked
function closeDeleteMeetingAlert() {
    // enable of all the buttons of the course previously disabled
    var meetingButtons = document.querySelectorAll('.remove_booked_meeting');
    Array.from(meetingButtons).forEach(function (button){
        button.disabled = false;
    });
    document.getElementById("delete-meeting-alert").style.display = "none";
    var deleteMeetingForm = document.getElementById("delete-meeting-form");
    deleteMeetingForm.innerHTML = "";
}