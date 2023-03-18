package it.unipi.dsmt.student_platform.interfaces;

import it.unipi.dsmt.student_platform.dto.LoggedUserDTO;
import it.unipi.dsmt.student_platform.dto.LoginInformationDTO;
import jakarta.ejb.Remote;
import org.jetbrains.annotations.NotNull;

@Remote
public interface UserEJB {
	LoggedUserDTO login (@NotNull LoginInformationDTO loginInformationDTO);
}
