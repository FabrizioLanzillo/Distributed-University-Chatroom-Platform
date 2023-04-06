package it.unipi.dsmt.student_platform.ejb;

import it.unipi.dsmt.student_platform.dao.bookedMeetingDAO;
import it.unipi.dsmt.student_platform.dto.BookingDTO;
import it.unipi.dsmt.student_platform.dto.MeetingDTO;
import it.unipi.dsmt.student_platform.dto.StudentBookedMeetingDTO;
import it.unipi.dsmt.student_platform.interfaces.BookedMeetingEJB;

import jakarta.annotation.Resource;
import jakarta.ejb.Stateless;

import javax.sql.DataSource;
import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.temporal.TemporalAdjusters;
import java.util.ArrayList;
import java.util.List;

@Stateless
public class BookedMeetingEJBImpl implements BookedMeetingEJB {
    
    /**
     * Datasource used to access the database.
     */
    @Resource(lookup = "jdbc/StudentPlatformPool")
    private DataSource dataSource;
    bookedMeetingDAO bookedMeetingDAO = new bookedMeetingDAO();


    public List<BookingDTO> getSlots(int id, int offset){
        LocalDate start = LocalDate.now().plusMonths(offset);
        
        if(offset != 0){
            start = start.withDayOfMonth(1);
        }
        
        
        LocalDate end = start.withDayOfMonth(start.getMonth().length(start.isLeapYear()));

        // Get data
        List<BookingDTO> bookedSlots = bookedMeetingDAO.getBookedSlots(start, end, id, dataSource);

        List<BookingDTO> allSlots = bookedMeetingDAO.getAllPossibleSlots(id, dataSource);

        if(allSlots == null){
            System.out.println("Error: No slots available");
            return new ArrayList<>();
        }

        ArrayList<BookingDTO> monthlySlots = new ArrayList<>();

        for(BookingDTO slot : allSlots){
            if(slot.getDayOfWeek() == 6 || slot.getDayOfWeek() == 7){
                System.out.println("Error: Saturday and Sunday are not allowed");
                continue;
            }

            // Get the first day of the next month
            LocalDate stop = start.with(TemporalAdjusters.firstDayOfNextMonth());

            // Get starting day (today or first day of specified month)
            LocalDate day = start.with(
                    TemporalAdjusters.nextOrSame(
                            DayOfWeek.of( slot.getDayOfWeek() )));
            //Until next month add all dates to ArrayList
            while( day.isBefore( stop ) ) {
                // Create a list of all possible slots
                monthlySlots.add(new BookingDTO(slot.getStart(), day, slot.getDayOfWeek(), slot.getId()));
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
    
    public boolean bookSlot(String studentID, int courseID, BookingDTO dto, int offset){

        List<BookingDTO> allSlots = getSlots(courseID, offset);
        if(allSlots == null){
            System.out.println("Error: No slots available");
            return false;
        }

        String meetingID = null;
        for (BookingDTO slot : allSlots) {
            if(slot.getDate().equals(dto.getDate()) && slot.getStart().equals(dto.getStart())){
                meetingID = slot.getId();
            }
        }
        
        return bookedMeetingDAO.bookSlot(studentID, meetingID, dto, dataSource);
    }
    
    public List<StudentBookedMeetingDTO> getBookedMeetingsForStudent(String id){
        return bookedMeetingDAO.getBookedMeetingsForStudentDAO(id, dataSource);
    }
    
    public boolean removeSlot(String bookingID){
        return bookedMeetingDAO.removeSlotDAO(bookingID, dataSource);
    }
    
    public List<MeetingDTO> getSlots(String professorID, int offset){
        LocalDate start = LocalDate.now().plusMonths(offset);
        
        if(offset != 0){
            start = start.withDayOfMonth(1);
        }
        
        LocalDate end = start.withDayOfMonth(start.getMonth().length(start.isLeapYear()));
        
        return bookedMeetingDAO.getProfessorBookedSlotsDAO(professorID, start, end, dataSource);
    }
}
