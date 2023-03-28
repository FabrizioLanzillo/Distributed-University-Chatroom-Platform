package it.unipi.dsmt.student_platform.ejb;

import it.unipi.dsmt.student_platform.dto.MeetingDTO;
import it.unipi.dsmt.student_platform.interfaces.MeetingEJB;

import jakarta.annotation.Resource;
import jakarta.ejb.Stateless;

import javax.sql.DataSource;
import java.sql.*;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

@Stateless
public class MeetingEJBImpl implements MeetingEJB {
	
	@Resource(lookup = "jdbc/StudentPlatformPool")
	private DataSource dataSource;
	
	public List<MeetingDTO> getSlots(String professorID, int offset){
		Calendar cal = Calendar.getInstance();
		cal.add(Calendar.MONTH, offset + 1);
		
		if(offset != 0){
			cal.set(Calendar.DAY_OF_MONTH, 1);
		}
		LocalDate start = LocalDate.of(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cal.get(Calendar.DAY_OF_MONTH));
		cal.add(Calendar.MONTH, 1);
		LocalDate end = LocalDate.of(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cal.get(Calendar.DAY_OF_MONTH));
		
		try(Connection connection = dataSource.getConnection()) {
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
	
	public boolean removeSlot(MeetingDTO dto){
		try(Connection connection = dataSource.getConnection()) {
			String query = "DELETE FROM booked_meeting " +
					"WHERE id = UUID_TO_BIN(?);";
			
			try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
				
				preparedStatement.setString(1, dto.getMeetingId());
				
				// Execute query
				System.out.println(dto.getMeetingId());
				int result = preparedStatement.executeUpdate();
				if(result == 1){
					return true;
				}
				return false;
			}
		}
		catch (SQLException e) {
			throw new RuntimeException(e);
		}
	}
}
