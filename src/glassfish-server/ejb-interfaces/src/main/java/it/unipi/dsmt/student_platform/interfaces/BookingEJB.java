package it.unipi.dsmt.student_platform.interfaces;

import it.unipi.dsmt.student_platform.dto.BookingDTO;
import jakarta.ejb.Remote;

import java.util.ArrayList;

@Remote
public interface BookingEJB {

    // This method extract required data of a specific course of known id
    ArrayList<BookingDTO> getSlots(int id, int offset);

    boolean bookASlot(int course_id, BookingDTO dto);
}
