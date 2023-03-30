package it.unipi.dsmt.student_platform.interfaces;

import it.unipi.dsmt.student_platform.dto.LoggedUserDTO;
import it.unipi.dsmt.student_platform.dto.LoginInformationDTO;
import it.unipi.dsmt.student_platform.dto.GeneralUserDTO;
import it.unipi.dsmt.student_platform.enums.UserRole;
import jakarta.ejb.Remote;
import org.jetbrains.annotations.NotNull;

import java.util.List;

@Remote
public interface UserEJB {
	LoggedUserDTO login (@NotNull LoginInformationDTO loginInformationDTO);

	List<GeneralUserDTO> searchUsers(String entered_string, UserRole role, int index);

	boolean banUser(String id, UserRole role);
}
