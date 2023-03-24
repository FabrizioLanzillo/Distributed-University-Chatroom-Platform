package it.unipi.dsmt.student_platform.dto;

import java.io.Serializable;
import java.sql.Date;
import java.sql.Time;
import java.time.LocalDate;

public class MeetingDTO implements Serializable {
    private String studentName;
    private int courseId;
    private LocalDate date;
    private Time time;

    public MeetingDTO(String studentName, int courseId, LocalDate date, Time time) {
        this.studentName = studentName;
        this.courseId = courseId;
        this.date = date;
        this.time = time;
    }

    public String getStudentName() {
        return studentName;
    }

    public void setStudentName(String studentName) {
        this.studentName = studentName;
    }

    public int getCourseId() {
        return courseId;
    }

    public void setCourseId(int courseId) {
        this.courseId = courseId;
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
                " at " + time;
    }
}
