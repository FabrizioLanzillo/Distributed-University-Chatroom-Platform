package it.unipi.dsmt.student_platform.dto;

import java.io.Serializable;
import java.sql.Time;
import java.time.LocalDate;

public class BookingDTO implements Serializable {
    private int dayOfWeek = -1;
    private Time start;
    private LocalDate date = null;

    public BookingDTO(Time start, int dayOfWeek){
            this.start = start;
            this.dayOfWeek = dayOfWeek;
    }

    public BookingDTO(Time start, LocalDate date){
        this.start = start;
        this.date = date;
    }

    public Time getStart() {
        return start;
    }

    public void setStart(Time start) {
        this.start = start;
    }

    public int getDayOfWeek() {
        return dayOfWeek;
    }

    public void setDayOfWeek(int dayOfWeek) {
        this.dayOfWeek = dayOfWeek;
    }

    public String toString(){
        String day;
        switch(dayOfWeek){
            case 1:
                 day="Monday";
            case 2:
                day="Tuesday";
            case 3:
                day="Wednesday";
            case 4:
                day="Thursday";
            case 5:
                day="Friday";
            case 6:
                day="Saturday";
            default:
                day="Sunday";
        }

        return day + " " + start.toString();
    }

    public LocalDate getDate() {
        return date;
    }

    public void setDate(LocalDate date) {
        this.date = date;
    }
}
