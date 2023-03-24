package it.unipi.dsmt.student_platform.interfaces;

import it.unipi.dsmt.student_platform.dto.MeetingDTO;
import jakarta.ejb.Remote;

import java.util.List;

@Remote
public interface MeetingEJB {

    // This method extract required data of a specific course of known id
    List<MeetingDTO> getSlots(int id, int offset);

    boolean removeSlot(int course_id, MeetingDTO dto);
}
