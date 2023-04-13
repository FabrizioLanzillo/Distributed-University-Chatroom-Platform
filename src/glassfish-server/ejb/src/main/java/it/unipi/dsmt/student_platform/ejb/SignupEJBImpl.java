package it.unipi.dsmt.student_platform.ejb;

import it.unipi.dsmt.student_platform.dto.SignupDTO;
import it.unipi.dsmt.student_platform.interfaces.SignupEJB;
import jakarta.annotation.Resource;
import jakarta.ejb.Stateless;
import org.jetbrains.annotations.NotNull;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;

/**
 * Implementation of SignupEJB class, which manage the logic behind the signup operation ensuring a safe database operation.
 */
@Stateless
public class SignupEJBImpl implements SignupEJB {
    
    /**
     * Datasource used to access the database.
     */
    @Resource(lookup = "jdbc/StudentPlatformPool")
    private DataSource dataSource;
    
    /**
     * definition of the signup operation. Takes a SignupDTO, perform the query and return the result.
     * @param signupDTO: SignupDTO type which contains user data to be stored
     * @return boolean value indicating if the signup operation was successful or not
     */
    @Override
    public boolean signup(@NotNull SignupDTO signupDTO){
        try(Connection connection = dataSource.getConnection()) {
            //Prepare the query
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
                    return result == 1;
                }
            }
            catch (SQLException e) {
                return false;
            }
    }
}