package it.unipi.dsmt.student_platform.dao;

import it.unipi.dsmt.student_platform.dto.BookingDTO;
import jakarta.annotation.Resource;
import jakarta.ejb.Stateless;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.sql.DataSource;
import java.sql.*;
import java.time.LocalDate;
import java.util.ArrayList;


public class BookingDAO {

    @Resource(lookup = "jdbc/StudentPlatformPool")
    private DataSource dataSource;

    public @Nullable ArrayList<BookingDTO> getBookedSlots(LocalDate date, int id){
        /*try (Connection connection = dataSource.getConnection()) {
            // Check if username and password is correct
            String query = "SELECT * " +
                    "FROM booked_meeting INNER JOIN " +
                    "WHERE date >= ?;";
            //TODO
            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
                // Set parameters in prepared statement

            }
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }*/
        return null;
    }

    public @Nullable ArrayList<BookingDTO> getAllBookedSlots(LocalDate date, int id) {
        BookingDTO bookingDTO = new BookingDTO(new Time(12,0,0), 1);
        ArrayList<BookingDTO> list = new ArrayList<>();
        list.add(bookingDTO);
        return list;
    }

    public boolean bookASlot(int courseId, BookingDTO dto) {
        return true;
    }
}
