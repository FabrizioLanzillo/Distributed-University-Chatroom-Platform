package it.unipi.dsmt.student_platform.interfaces;

import it.unipi.dsmt.student_platform.dto.*;
import it.unipi.dsmt.student_platform.enums.UserRole;
import jakarta.ejb.Remote;
import org.jetbrains.annotations.NotNull;

import java.util.List;

/**
 * Interface for EJBs responsible for handling all the business logic related to the user management.
 */
@Remote
public interface UserEJB {
	LoggedUserDTO login (@NotNull LoginInformationDTO loginInformationDTO);

	List<GeneralUserDTO> searchUsers(String entered_string, UserRole role, int index);

	boolean banUser(String id, UserRole role);

	boolean createProfessorAccount(CreateProfessorDTO createProfessorDTO);
}
