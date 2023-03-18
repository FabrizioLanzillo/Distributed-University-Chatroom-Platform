package it.unipi.dsmt.student_platform.interfaces;

import it.unipi.dsmt.student_platform.dto.SignupDTO;
import it.unipi.dsmt.student_platform.dto.UserDTO;
import it.unipi.dsmt.student_platform.enums.UserRole;
import jakarta.ejb.Remote;
import org.jetbrains.annotations.NotNull;

import java.sql.SQLException;

@Remote
public interface SignupEJB {
	boolean signup(@NotNull String username, @NotNull String password, @NotNull String email, @NotNull String name,
							@NotNull String surname, @NotNull String degree, @NotNull String language) throws SQLException;
}
