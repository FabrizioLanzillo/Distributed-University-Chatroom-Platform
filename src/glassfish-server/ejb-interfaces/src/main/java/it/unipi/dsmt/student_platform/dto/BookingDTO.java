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

    public BookingDTO(Time start, LocalDate date, int dayOfWeek){
        this.start = start;
        this.date = date;
        this.dayOfWeek = dayOfWeek;
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
                 break;
            case 2:
                day="Tuesday";
                break;
            case 3:
                day="Wednesday";
                break;
            case 4:
                day="Thursday";
                break;
            case 5:
                day="Friday";
                break;
            case 7:
                day="Saturday";
                break;
            default:
                day="Sunday";
                break;
        }

        return day + " " + date.toString() + " - " + start.toString();
    }

    public LocalDate getDate() {
        return date;
    }

    public void setDate(LocalDate date) {
        this.date = date;
    }
}
