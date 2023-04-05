package it.unipi.dsmt.student_platform.dao;

import it.unipi.dsmt.student_platform.dto.BookingDTO;
import it.unipi.dsmt.student_platform.dto.StudentBookedMeetingDTO;
import jakarta.annotation.Resource;
import jakarta.ejb.Stateless;

import javax.sql.DataSource;
import java.sql.*;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

@Stateless
public class BookingDAO {
    
    /**
     * Datasource used to access the database.
     */
    @Resource(lookup = "jdbc/StudentPlatformPool")
    private DataSource dataSource;

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

}
