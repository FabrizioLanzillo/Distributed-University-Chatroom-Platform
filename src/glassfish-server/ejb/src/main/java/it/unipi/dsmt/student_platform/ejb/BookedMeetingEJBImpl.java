package it.unipi.dsmt.student_platform.ejb;

import it.unipi.dsmt.student_platform.dao.BookedMeetingDAO;
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

/**
 * EJB class which handles all the business logic related to the bookings and meetings
 */
@Stateless
public class BookedMeetingEJBImpl implements BookedMeetingEJB {
    
    // Datasource used to access MySQL database
    @Resource(lookup = "jdbc/StudentPlatformPool")
    private DataSource dataSource;
    // DAO to query the database
    BookedMeetingDAO bookedMeetingDAO = new BookedMeetingDAO();
    
    
    /**
     * Method that extracts all the bookable slots for the specified course
     * @param CourseID: ID of the course for which data is requested
     * @param offset: offset from current month, 0 means current month, no negative values allowed
     * @return List of BookingDTO objects representing the bookable slots
     */
    public List<BookingDTO> getBookableSlots(int CourseID, int offset){
        // Get current date
        LocalDate start = LocalDate.now().plusMonths(offset);
        
        // If we are looking for future months we have to see the whole month
        if(offset != 0){
            start = start.withDayOfMonth(1);
        }
        //Get last day of the requested month
        LocalDate end = start.withDayOfMonth(start.getMonth().length(start.isLeapYear()));

        // Get already booked slots
        List<BookingDTO> bookedSlots = bookedMeetingDAO.getBookedSlotsDAO(start, end, CourseID, dataSource);
        // Get all slots
        List<BookingDTO> allSlots = bookedMeetingDAO.getAllPossibleSlotsDAO(CourseID, dataSource);
        
        //Check if the course have slots to be booked, otherwise return empty list
        if(allSlots == null){
            System.err.println("Error: No slots available");
            return new ArrayList<>();
        }

        // List of the available slot to be returned
        ArrayList<BookingDTO> monthlySlots = new ArrayList<>();

        // Sunday or Saturday are not allowed
        for(BookingDTO slot : allSlots){
            if (slot.getDayOfWeek() <= 0 || slot.getDayOfWeek() >= 6){
                System.err.println("Error: Saturday and Sunday are not allowed");
                continue;
            }
            
            // Get first occurrence, for the specified month, of the slot week day; i.e. first monday of July etc.
            LocalDate day = start.with(
                    TemporalAdjusters.nextOrSame(
                            DayOfWeek.of( slot.getDayOfWeek() )));
            // For each week day in this month create a new entry inside the return list
            while( day.isBefore( end ) ) {
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
    
    /**
     * Method that handle the book requests from the user for a specified slot and day
     * @param studentID ID of the student that wants to book the slot
     * @param courseID ID of the course for which the student want to book the meeting
     * @param dto BookingDTO object containing the slot information
     * @param offset Offset of the month in which the user wants to book the slot (0 is current month)
     * @return boolean indicating whether the slot has been successfully booked or not
     */
    public boolean bookSlot(String studentID, int courseID, BookingDTO dto, int offset){

        // Get all available slots showed to the user
        List<BookingDTO> allSlots = getBookableSlots(courseID, offset);
        if(allSlots == null){
            System.err.println("Error: No slots available");
            return false;
        }

        // Check if the selected meeting ID exists
        String meetingID = null;
        for (BookingDTO slot : allSlots) {
            if(slot.getDate().equals(dto.getDate()) && slot.getStart().equals(dto.getStart())){
                meetingID = slot.getId();
            }
        }
        
        // Set the slot as booked inside the DB and return the result of the query
        return bookedMeetingDAO.bookSlotDAO(studentID, meetingID, dto, dataSource);
    }
    
    public List<StudentBookedMeetingDTO> getStudentMeetings(String id){
        return bookedMeetingDAO.getBookedMeetingsForStudentDAO(id, dataSource);
    }
    
    /**
     * Method that manage professor requests to un-book the specified meeting with the student
     * @param bookingID: ID of the meeting to be unbooked
     * @return boolean value representing the state of the operation: True -> successful, False -> otherwise
     */
    public boolean removeSlot(String bookingID){
        return bookedMeetingDAO.removeSlotDAO(bookingID, dataSource);
    }
    
    /**
     * Method that extracts booked slots for the professor
     * @param professorID: ID of the professor that want to see booked meetings
     * @param offset: Offset of the month in which the professor wants to see the booked slots (0 is current month)
     * @return List of MeetingDTO objects representing booked slots
     */
    public List<MeetingDTO> getProfessorMeetings(String professorID, int offset){
        // Extract starting date
        LocalDate start = LocalDate.now().plusMonths(offset);
        if(offset != 0){
            start = start.withDayOfMonth(1);
        }
        
        // Set ending interval date
        LocalDate end = start.withDayOfMonth(start.getMonth().length(start.isLeapYear()));
        
        // Perform the query and return results
        return bookedMeetingDAO.getProfessorBookedSlotsDAO(professorID, start, end, dataSource);
    }
}

