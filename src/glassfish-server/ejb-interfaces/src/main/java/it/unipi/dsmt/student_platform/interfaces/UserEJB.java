package it.unipi.dsmt.student_platform.interfaces;

import it.unipi.dsmt.student_platform.dto.UserDTO;
import it.unipi.dsmt.student_platform.enums.UserRole;
import jakarta.ejb.Remote;
import org.jetbrains.annotations.NotNull;

@Remote
public interface UserEJB {
	UserDTO login(@NotNull String username, @NotNull String password, @NotNull UserRole role);
}
