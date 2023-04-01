package it.unipi.dsmt.student_platform.ejb;

import it.unipi.dsmt.student_platform.dto.LoggedUserDTO;
import it.unipi.dsmt.student_platform.dto.LoginInformationDTO;
import it.unipi.dsmt.student_platform.dto.GeneralUserDTO;
import it.unipi.dsmt.student_platform.enums.UserRole;
import it.unipi.dsmt.student_platform.interfaces.UserEJB;
import jakarta.annotation.Resource;
import jakarta.ejb.Stateless;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.sql.DataSource;
import java.sql.*;
import java.util.ArrayList;
import java.util.List;

@Stateless
public class UserEJBImpl implements UserEJB {
	
	@Resource(lookup = "jdbc/StudentPlatformPool")
	private DataSource dataSource;
	
	@Override
	public @Nullable LoggedUserDTO login (@NotNull LoginInformationDTO loginInformation) {
		
		try (Connection connection = dataSource.getConnection()) {
			// Check if username and password is correct
			String query = "SELECT BIN_TO_UUID(id) AS id FROM "
					+ loginInformation.getRole().name()
					+ " WHERE `username` = ? AND `password` = ?";
			
			try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
				// Set parameters in prepared statement
				preparedStatement.setString(1, loginInformation.getUsername());
				preparedStatement.setString(2, loginInformation.getPassword());
				
				// Execute query
				try (ResultSet resultSet = preparedStatement.executeQuery()) {
					// If the query returned some results, then the login is successful!
					if (resultSet.next()) {
						return new LoggedUserDTO(
								resultSet.getString("id"),
								loginInformation.getUsername(),
								loginInformation.getRole()
						);
					} else {
						return null;
					}
				}
			}
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
	}

	public List<GeneralUserDTO> searchUsers(String entered_string, UserRole role, int index){
		List<GeneralUserDTO> users = new ArrayList<>();
		try (Connection connection = dataSource.getConnection()) {
			String query = "SELECT BIN_TO_UUID(`id`) as id, `username` as username, `email` as email, `name` as name, `surname` as surname FROM ";
			if (role == UserRole.student) {
				query = query + "student ";
			} else{
				query = query + "professor ";
			}
			if(entered_string.compareTo("") != 0){
				query = query + " WHERE `username` LIKE ? ";
			}
			query = query + "ORDER BY username DESC LIMIT 10 OFFSET " + index * 10 + ";";
			try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
				// Look in the correct table
				if(entered_string.compareTo("") != 0)
					preparedStatement.setString(1, "%" + entered_string + "%");
				
				// Execute query
				try (ResultSet resultSet = preparedStatement.executeQuery()) {
					// If the query returned some results, then the login is successful!
					while (resultSet.next()) {
						GeneralUserDTO usr = new GeneralUserDTO();
						usr.setId(resultSet.getString("id"));
						usr.setUsername(resultSet.getString("username"));
						usr.setEmail(resultSet.getString("email"));
						usr.setName(resultSet.getString("name"));
						usr.setSurname(resultSet.getString("surname"));
						users.add(usr);
					}
					return users;
				}
			}
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
	}

	public boolean banUser(String id, UserRole role) {
		try (Connection connection = dataSource.getConnection()) {
            String query = "DELETE FROM  ";
			if (role == UserRole.student) {
				query = query + "student ";
			}
			else {
				query = query + "professor ";
			}
			
			query = query + "WHERE `id` = UUID_TO_BIN(?);";

            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
				// Look in the correct table
                preparedStatement.setString(1, id);

                // Execute query
                int ret = preparedStatement.executeUpdate();
				return ret == 1;
			}
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
	}
	
}
