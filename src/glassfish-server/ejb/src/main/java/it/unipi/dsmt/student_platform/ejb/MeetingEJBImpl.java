package it.unipi.dsmt.student_platform.ejb;

import it.unipi.dsmt.student_platform.dto.MeetingDTO;
import it.unipi.dsmt.student_platform.interfaces.MeetingEJB;

import jakarta.annotation.Resource;
import jakarta.ejb.Stateless;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Time;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

@Stateless
public class MeetingEJBImpl implements MeetingEJB {

    @Resource(lookup = "jdbc/StudentPlatformPool")
    private DataSource dataSource;

    public List<MeetingDTO> getSlots(int id, int offset){
        Calendar cal = Calendar.getInstance();
        cal.add(Calendar.MONTH, offset + 1);

        if(offset != 0){
            cal.set(Calendar.DAY_OF_MONTH, 1);
        }
        LocalDate result = LocalDate.of(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cal.get(Calendar.DAY_OF_MONTH));

        try(Connection connection = dataSource.getConnection()){
            //TODO query
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
        ArrayList<MeetingDTO> rL = new ArrayList<>();
        rL.add(new MeetingDTO("Gigio", 1, result, Time.valueOf("12:00:00")));
        return rL;
    }

    public boolean removeSlot(int course_id, MeetingDTO dto){
        try(Connection connection = dataSource.getConnection()){
            //TODO query
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
        return true;
    }
}
