package it.unipi.dsmt.student_platform.ejb;

import it.unipi.dsmt.student_platform.dao.BookingDAO;
import it.unipi.dsmt.student_platform.dto.BookingDTO;
import it.unipi.dsmt.student_platform.interfaces.BookingEJB;

import jakarta.ejb.Stateless;
import org.jetbrains.annotations.Nullable;

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.temporal.TemporalAdjusters;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

@Stateless
public class BookingEJBImpl implements BookingEJB {
    BookingDAO bookingDAO = new BookingDAO();

    // This method extract required data of a specific course of known id
    public @Nullable List<BookingDTO> getSlots(int id, int offset){
        // Get the available dates for the month in which the user wants to book
        Calendar cal = Calendar.getInstance();
        cal.add(Calendar.MONTH, offset + 1);

        // If is not the current month set starting day to the first of the month otherwise leave current one
        if(offset != 0){
            cal.set(Calendar.DAY_OF_MONTH, 1);
        }
        LocalDate result = LocalDate.of(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cal.get(Calendar.DAY_OF_MONTH));

        // Get data
        List<BookingDTO> bookedSlots = bookingDAO.getBookedSlots(result, id);
        List<BookingDTO> allSlots = bookingDAO.getAllPossibleSlots(result, id);

        if(allSlots == null){
            System.out.println("Error: No slots available");
            return new ArrayList<BookingDTO>();
        }

        ArrayList<BookingDTO> monthlySlots = new ArrayList<>();

        for(BookingDTO slot : allSlots){
            if(slot.getDayOfWeek() == 6 || slot.getDayOfWeek() == 7){
                System.out.println("Error: Saturday and Sunday are not allowed");
                continue;
            }

            // Get the first day of the next month
            LocalDate stop = result.with(TemporalAdjusters.firstDayOfNextMonth());

            // Get starting day (today or first day of specified month)
            LocalDate day = result.with(
                    TemporalAdjusters.nextOrSame(
                            DayOfWeek.of( slot.getDayOfWeek() )));
            //Until next month add all dates to ArrayList
            while( day.isBefore( stop ) ) {
                // Create a list of all possible slots
                monthlySlots.add(new BookingDTO(slot.getStart(), day, slot.getDayOfWeek()));
                // Set up the next loop.
                day = day.plusWeeks( 1 );
            }
        }

        // If no bookings are available then all slots are free
        if(bookedSlots == null){
            return monthlySlots;
        }

        // otherwise remove from the list all unavailable slots
        for(BookingDTO bs : bookedSlots){
            // If days and times coincide then the slots isn't available
            monthlySlots.removeIf(as -> (bs.getDate().equals(as.getDate())) && (bs.getStart().equals(as.getStart())));
        }

        return monthlySlots;
    }


    public boolean bookSlot(int course_id, BookingDTO dto){
        return bookingDAO.bookSlot(course_id, dto);
    }

}
