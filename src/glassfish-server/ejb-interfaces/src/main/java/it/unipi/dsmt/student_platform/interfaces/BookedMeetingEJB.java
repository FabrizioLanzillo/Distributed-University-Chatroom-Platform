package it.unipi.dsmt.student_platform.interfaces;

import it.unipi.dsmt.student_platform.dto.BookingDTO;
import it.unipi.dsmt.student_platform.dto.MeetingDTO;
import it.unipi.dsmt.student_platform.dto.StudentBookedMeetingDTO;
import jakarta.ejb.Remote;

import java.util.List;

@Remote
public interface BookedMeetingEJB {
    
    List<BookingDTO> getSlots(int id, int offset);

    boolean bookSlot(String studentID, int courseID, BookingDTO dto, int offset);
    
    List<StudentBookedMeetingDTO> getBookedMeetingsForStudent(String id);
    
    boolean removeSlot(String bookingID);
    
    List<MeetingDTO> getSlots(String professorID, int offset);
}
