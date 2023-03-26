package it.unipi.dsmt.student_platform.interfaces;

import it.unipi.dsmt.student_platform.dto.BookingDTO;
import jakarta.ejb.Remote;
import org.jetbrains.annotations.Nullable;

import java.util.List;

@Remote
public interface BookingEJB {

    // This method extract required data of a specific course of known id
    @Nullable List<BookingDTO> getSlots(int id, int offset);

    boolean bookSlot(String studentID, int courseID, BookingDTO dto, int offset);
}
