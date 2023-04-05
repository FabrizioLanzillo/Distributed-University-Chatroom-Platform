package it.unipi.dsmt.student_platform.dto;

import java.io.Serializable;
import java.sql.Time;
import java.time.LocalDate;

/***
 * Serializable object for Booking, it carries the information that the server needs to store and retrieve inside the database.
 */
public class BookingDTO implements Serializable {

    private String id="";
    private int dayOfWeek = -1;
    private Time start;
    private LocalDate date = null;
    
    /**
     * Constructor used to retrieve the booking from the database.
     * @param start Time of the meeting
     * @param dayOfWeek Integer representing the week day of the meeting
     * @param id Meeting ID
     */
    public BookingDTO(Time start, int dayOfWeek, String id){
            this.start = start;
            this.dayOfWeek = dayOfWeek;
            this.id = id;
    }
    
    /**
     * Constructor used to store the information of already booked meetings.
     * @param start Time representing the start of the meeting
     * @param date Date of the meeting
     */
    public BookingDTO(Time start, LocalDate date){
        this.start = start;
        this.date = date;
    }
    
    /**
     * Constructor used in getSlots() method. Used to create Booking Object of not booked meetings.
     * @param start Time representing the hour in which the meeting starts.
     * @param date Date of the meeting.
     * @param dayOfWeek Day of the week of the meeting.
     * @param id Meeting ID.
     */
    public BookingDTO(Time start, LocalDate date, int dayOfWeek, String id){
        this.start = start;
        this.date = date;
        this.dayOfWeek = dayOfWeek;
        this.id = id;
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

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }
}
