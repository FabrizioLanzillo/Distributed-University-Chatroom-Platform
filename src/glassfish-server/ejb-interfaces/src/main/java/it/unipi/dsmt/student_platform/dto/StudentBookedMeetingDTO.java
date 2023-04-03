package it.unipi.dsmt.student_platform.dto;

import java.io.Serializable;
import java.sql.Time;
import java.time.LocalDate;

public class StudentBookedMeetingDTO implements Serializable {

	private String BookedMeetingId = "";
	private String courseName = "";
	private LocalDate date = null;
	private Time time = null;


	public StudentBookedMeetingDTO(String BookedMeetingId, String courseName, LocalDate date, Time time) {
		this.BookedMeetingId = BookedMeetingId;
		this.courseName = courseName;
		this.date = date;
		this.time = time;
	}

	public String getBookedMeetingId() {
		return BookedMeetingId;
	}

	public String getCourseName() {
		return courseName;
	}

	public Time getTime() {
		return time;
	}

	public LocalDate getDate() {
		return date;
	}

	public void setBookedMeetingId(String bookedMeetingId) {
		BookedMeetingId = bookedMeetingId;
	}

	public void setCourseName(String courseName) {
		this.courseName = courseName;
	}

	public void setDate(LocalDate date) {
		this.date = date;
	}

	public void setTime(Time time) {
		this.time = time;
	}

	@Override
	public String toString() {
		return "StudentBookedMeetingDTO{" +
				"courseName='" + courseName + '\'' +
				", date=" + date +
				", time=" + time +
				'}';
	}
}
