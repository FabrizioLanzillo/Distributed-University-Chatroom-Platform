/**
 * function that show the hidden form, which send the post request for the course delete
 * it also add the relative button to the selected course, setting up the value
 * to be submitted with the id of the course selected
 * @param courseIdToDelete is the id of the course to delete
 */
function showDeleteAlert(courseIdToDelete) {
    // disable of all the buttons of the course until the operation is finished
    // or the cancel button is clicked
    const coursesButtons = document.querySelectorAll('.selected-courses');
    Array.from(coursesButtons).forEach(function (button){
        button.disabled = true;
    });
    document.getElementById("delete-course-alert").style.display = "block";
    const deleteCourseForm = document.getElementById("delete-course-form");
    const submitButton = document.createElement("button");
    submitButton.innerHTML = "Delete";
    submitButton.setAttribute("type", "submit");
    submitButton.setAttribute("name", "courseId");
    submitButton.setAttribute("value", courseIdToDelete);
    submitButton.setAttribute("class", "delete-button");
    deleteCourseForm.appendChild(submitButton);
}

/**
 * function that hide the form, if the cancel button is clicked
 */
function closeDeleteAlert() {
    // enable of all the buttons of the course previously disabled
    const coursesButtons = document.querySelectorAll('.selected-courses');
    Array.from(coursesButtons).forEach(function (button){
        button.disabled = false;
    });
    document.getElementById("delete-course-alert").style.display = "none";
    const deleteCourseForm = document.getElementById("delete-course-form");
    deleteCourseForm.innerHTML = "";
}