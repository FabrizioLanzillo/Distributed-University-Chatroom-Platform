package it.unipi.dsmt.student_platform.interfaces;

import it.unipi.dsmt.student_platform.dto.SignupDTO;
import jakarta.ejb.Remote;

@Remote
public interface SignupEJB {
	boolean signup(SignupDTO signupDTO);
}
