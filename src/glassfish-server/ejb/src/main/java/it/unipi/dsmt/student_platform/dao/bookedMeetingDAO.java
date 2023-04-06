package it.unipi.dsmt.student_platform.dao;

import it.unipi.dsmt.student_platform.dto.BookingDTO;
import it.unipi.dsmt.student_platform.dto.MeetingDTO;
import it.unipi.dsmt.student_platform.dto.StudentBookedMeetingDTO;
import jakarta.ejb.Stateless;

import javax.sql.DataSource;
import java.sql.*;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

@Stateless
public class bookedMeetingDAO {

    public List<BookingDTO> getBookedSlots(LocalDate start, LocalDate end, int id, DataSource dataSource) {
        try (Connection connection = dataSource.getConnection()) {

            String query = "SELECT date, ms.starting_time as starting_time\n " +
                    "FROM booked_meeting bs INNER JOIN meeting_slot ms on bs.time_slot = ms.id \n" +
                    "WHERE date BETWEEN ? AND ? \n " +
                    "AND ms.course = ?;";

            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
                // Set parameters in prepared statement
                preparedStatement.setDate(1, Date.valueOf(start));
                preparedStatement.setDate(2, Date.valueOf(end));
                preparedStatement.setInt(3, id);

                try (ResultSet resultSet = preparedStatement.executeQuery()) {
                    List<BookingDTO> list = new ArrayList<>();
                    while (resultSet.next()) {
                        BookingDTO bookingDTO = new BookingDTO(resultSet.getTime("starting_time"), resultSet.getDate("date").toLocalDate());
                        list.add(bookingDTO);
                    }
                    return list;
                }
            }
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    public List<BookingDTO> getAllPossibleSlots(int id, DataSource dataSource) {
        try (Connection connection = dataSource.getConnection()) {
            String query = "SELECT starting_time, weekday, BIN_TO_UUID(id) as id " +
                    "FROM meeting_slot " +
                    "WHERE course = ?;";

            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
                // Set parameters in prepared statement
                preparedStatement.setInt(1, id);

                try (ResultSet resultSet = preparedStatement.executeQuery()) {
                    List<BookingDTO> list = new ArrayList<>();
                    while (resultSet.next()) {
                        BookingDTO bookingDTO = new BookingDTO(resultSet.getTime("starting_time"), resultSet.getInt("weekday"), resultSet.getString("id"));
                        list.add(bookingDTO);
                    }
                    return list;
                }
            }
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    public boolean bookSlot(String studentID, String meetingID, BookingDTO dto, DataSource dataSource) {
        try (Connection connection = dataSource.getConnection()) {
            String query = "INSERT INTO booked_meeting (id, time_slot, date, student) VALUES (UUID_TO_BIN(UUID()),UUID_TO_BIN(?),?,UUID_TO_BIN(?));";

            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
                // Set parameters in prepared statement
                preparedStatement.setString(1, meetingID);
                preparedStatement.setDate(2, Date.valueOf(dto.getDate()));
                preparedStatement.setString(3, studentID);

                // Execute query
                return preparedStatement.executeUpdate() == 1;
            }
        } catch (SQLException e) {
            return false;
        }
    }
    
    public List<StudentBookedMeetingDTO> getBookedMeetingsForStudentDAO(String id,DataSource dataSource) {
        List<StudentBookedMeetingDTO> bookedMeeting = new ArrayList<>();
        
        try (Connection connection = dataSource.getConnection()) {
            // Get details of requested course
            String query =  "SELECT BIN_TO_UUID(bm.id) as id, c.name, bm.date, ms.starting_time " +
                    "FROM booked_meeting bm " +
                    "     INNER JOIN " +
                    "     meeting_slot ms on bm.time_slot = ms.id " +
                    "     INNER JOIN " +
                    "     course c on ms.course = c.id " +
                    "WHERE bm.student = UUID_TO_BIN(?);";
            
            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
                // Set parameters in prepared statement
                preparedStatement.setString(1, id);
                
                // Execute query
                try (ResultSet resultSet = preparedStatement.executeQuery()) {
                    // If the query returned a result set,
                    // then wrap it inside a CourseDTO object and return it
                    while (resultSet.next()){
                        bookedMeeting.add(new StudentBookedMeetingDTO(
                                        resultSet.getString("id"),
                                        resultSet.getString("c.name"),
                                        resultSet.getDate("bm.date").toLocalDate(),
                                        resultSet.getTime("ms.starting_time")
                                )
                        );
                    }
                }
            }
        }
        catch (SQLException e) {
            throw new RuntimeException(e);
        }
        return bookedMeeting;
    }
    
    public boolean removeSlotDAO(String bookingID, DataSource dataSource){
        try(Connection connection = dataSource.getConnection()) {
            String query = "DELETE FROM booked_meeting " +
                    "WHERE id = UUID_TO_BIN(?);";
            
            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
                
                preparedStatement.setString(1, bookingID);
                
                // Execute query
                int result = preparedStatement.executeUpdate();
                return result == 1;
            }
        }
        catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }
    
    public List<MeetingDTO> getProfessorBookedSlotsDAO(String professorID, LocalDate start, LocalDate end, DataSource dataSource) {
        try (Connection connection = dataSource.getConnection()) {
            String query = "SELECT BIN_TO_UUID(bm.`id`) as id, ms.`starting_time`, bm.`date`, s.`name`, s.`surname`, " +
                    " s.`language`, c.`name` as courseName " +
                    "FROM `booked_meeting` bm INNER JOIN `meeting_slot` ms on bm.time_slot = ms.id " +
                    "    INNER JOIN `course` c on ms.course = c.id " +
                    "    INNER JOIN `student` s on bm.student = s.id " +
                    "    INNER JOIN `professor` p on c.professor = p.id " +
                    "WHERE p.id = UUID_TO_BIN(?) AND bm.date BETWEEN ? AND ?;";
            
            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
                preparedStatement.setString(1, professorID);
                preparedStatement.setDate(2, java.sql.Date.valueOf(start));
                preparedStatement.setDate(3, java.sql.Date.valueOf(end));
                
                try (ResultSet resultSet = preparedStatement.executeQuery()) {
                    List<MeetingDTO> slots = new ArrayList<>();
                    while (resultSet.next()) {
                        MeetingDTO slot = new MeetingDTO();
                        slot.setMeetingId(resultSet.getString("id"));
                        slot.setTime(Time.valueOf(resultSet.getString("starting_time")));
                        slot.setDate(resultSet.getDate("date").toLocalDate());
                        slot.setStudentName(resultSet.getString("name"));
                        slot.setStudentSurname(resultSet.getString("surname"));
                        slot.setStudentLanguage(resultSet.getString("language"));
                        slot.setCourseName(resultSet.getString("courseName"));
                        slots.add(slot);
                    }
                    return slots;
                }
            }
            
            
        }
        catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

}
