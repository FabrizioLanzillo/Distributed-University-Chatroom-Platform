package it.unipi.dsmt.student_platform.dto;

import java.io.Serializable;
import java.sql.Date;
import java.sql.Time;
import java.time.LocalDate;

public class MeetingDTO implements Serializable {

    private String meetingId = "";
    private String studentName = "";

    private String studentSurname = "";
    private String courseName = "";
    private LocalDate date = null;
    private Time time = null;

    private String studentLanguage = "";

    public MeetingDTO(String studentName, String courseId, LocalDate date, Time time) {
        this.studentName = studentName;
        this.courseName = courseName;
        this.date = date;
        this.time = time;
    }

    public MeetingDTO() {
    }

    public String getStudentName() {
        return studentName;
    }

    public void setStudentName(String studentName) {
        this.studentName = studentName;
    }

    public String getCourseName() {
        return courseName;
    }

    public void setCourseName(String courseName) {
        this.courseName = courseName;
    }

    public LocalDate getDate() {
        return date;
    }

    public void setDate(LocalDate date) {
        this.date = date;
    }

    public Time getTime() {
        return time;
    }

    public void setTime(Time time) {
        this.time = time;
    }

    public String toString() {
        return studentName +
                " on " + date +
                " at " + time +
                " for course: " + courseName;
    }

    public String getMeetingId() {
        return meetingId;
    }

    public void setMeetingId(String meetingId) {
        this.meetingId = meetingId;
    }

    public String getStudentSurname() {
        return studentSurname;
    }

    public void setStudentSurname(String studentSurname) {
        this.studentSurname = studentSurname;
    }

    public String getStudentLanguage() {
        return studentLanguage;
    }

    public void setStudentLanguage(String studentLanguage) {
        this.studentLanguage = studentLanguage;
    }
    
}
