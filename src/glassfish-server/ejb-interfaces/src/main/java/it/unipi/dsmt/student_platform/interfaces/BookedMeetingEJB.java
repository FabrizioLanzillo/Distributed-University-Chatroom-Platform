package it.unipi.dsmt.student_platform.interfaces;

import it.unipi.dsmt.student_platform.dto.BookingDTO;
import it.unipi.dsmt.student_platform.dto.MeetingDTO;
import it.unipi.dsmt.student_platform.dto.StudentBookedMeetingDTO;
import jakarta.ejb.Remote;

import java.util.List;

@Remote
public interface BookedMeetingEJB {
    
    List<BookingDTO> getBookableSlots(int courseID, int offset);
    boolean bookSlot(String studentID, int courseID, BookingDTO dto, int offset);
    boolean removeSlot(String bookingID);
    
    List<StudentBookedMeetingDTO> getStudentMeetings(String studentID);
    List<MeetingDTO> getProfessorMeetings(String professorID, int offset);
}
