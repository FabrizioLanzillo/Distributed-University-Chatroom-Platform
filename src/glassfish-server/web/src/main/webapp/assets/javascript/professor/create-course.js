$(document).ready(function(){
	fillWeekdayInput();
	$('.timepicker').val('');
	$('#start-time').timepicker({
		timeFormat: 'HH:mm',
		interval: 30,
		minTime: '8',
		maxTime: '20',
		dynamic: false,
		dropdown: true,
		scrollbar: true,
		change: enableTimepickerTo
	});
	console.log("ready");
});


function fillWeekdayInput () {
	const WEEKDAYS = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday"]
	const weekdayElem = document.getElementById("weekday");
	WEEKDAYS.forEach(
		(value, i) => {
			let optionElem = document.createElement("option");
			const dayNumber = i+1;
			optionElem.value = dayNumber.toString();
			optionElem.textContent = value;
			weekdayElem.append(optionElem);
		}
	);
	console.log("fillWeekdayInput");
}


function enableTimepickerTo (){
	const valueTimeFrom = $('#start-time').val();
	let date = new Date("1970-01-01T" + valueTimeFrom);
	date.setMinutes(date.getMinutes() + 30);
	let nextSlotToSelect = date.getHours().toString() + ":" + date.getMinutes().toString();
	
	const inputTimeTo = $('#end-time').val('');
	inputTimeTo
		.timepicker("destroy")
		.prop("disabled", false)
		.timepicker({
			timeFormat: 'HH:mm',
			interval: 30,
			minTime: nextSlotToSelect,
			maxTime: '20:00',
			dynamic: false,
			dropdown: true,
			scrollbar: true,
			alwaysSetTime: false
		});
	
	console.log("change");
}




