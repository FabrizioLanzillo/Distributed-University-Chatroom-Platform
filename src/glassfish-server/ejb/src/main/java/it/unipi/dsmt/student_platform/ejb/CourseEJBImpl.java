package it.unipi.dsmt.student_platform.ejb;

import it.unipi.dsmt.student_platform.dto.CourseCreationDTO;
import it.unipi.dsmt.student_platform.dto.CourseDTO;
import it.unipi.dsmt.student_platform.dto.ProfessorDTO;
import it.unipi.dsmt.student_platform.interfaces.CourseEJB;
import jakarta.ejb.Stateless;
import org.jetbrains.annotations.NotNull;

@Stateless
public class CourseEJBImpl implements CourseEJB {
	
	@Override
	public CourseDTO getCourse(@NotNull String id) {
		// TODO
		System.out.printf("CourseEJBImpl.getCourse(%s)%n", id);
		return new CourseDTO(
				"1",
				"DSMT",
				new ProfessorDTO("1", "Alessio", "Bechini"),
				"This is a description"
		);
	}
	
	@Override
	public boolean createCourse(@NotNull CourseCreationDTO course) {
		return true;
	}
	
}
