package it.unipi.dsmt.student_platform.ejb;

import it.unipi.dsmt.student_platform.dto.SignupDTO;
import it.unipi.dsmt.student_platform.enums.UserRole;
import it.unipi.dsmt.student_platform.interfaces.SignupEJB;
import jakarta.annotation.Resource;
import jakarta.ejb.Stateless;
import org.jetbrains.annotations.NotNull;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;

@Stateless
public class SignupEJBImpl implements SignupEJB {

    @Resource(lookup = "jdbc/StudentPlatformPool")
    private DataSource dataSource;

    @Override
    public boolean signup(@NotNull SignupDTO signupDTO){
        try(Connection connection = dataSource.getConnection()) {
            String query = "INSERT INTO `student` VALUES ( UUID_TO_BIN(UUID()) ,?, ?, ?, ?, ? ,?, ?);";

                try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
                    // Set parameters in prepared statement
                    preparedStatement.setString(1, signupDTO.getUsername());
                    preparedStatement.setString(2, signupDTO.getPassword());
                    preparedStatement.setString(3, signupDTO.getEmail());
                    preparedStatement.setString(4, signupDTO.getName());
                    preparedStatement.setString(5, signupDTO.getSurname());
                    preparedStatement.setString(6, signupDTO.getDegree());
                    preparedStatement.setString(7, signupDTO.getLanguage());

                    // Execute query
                    int result = preparedStatement.executeUpdate();
                    // evaluate the return value
                    if(result != 1)
                        return false;
                    return true;
                }
            }
            catch (SQLException e) {
                throw new RuntimeException(e);
                }
    }
}