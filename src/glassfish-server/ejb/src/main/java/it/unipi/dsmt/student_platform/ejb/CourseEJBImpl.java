package it.unipi.dsmt.student_platform.ejb;

import it.unipi.dsmt.student_platform.dto.CourseDTO;
import it.unipi.dsmt.student_platform.interfaces.CourseEJB;
import jakarta.ejb.Stateless;
import org.jetbrains.annotations.NotNull;

@Stateless
public class CourseEJBImpl implements CourseEJB {
	
	@Override
	public CourseDTO getCourse(@NotNull String id) {
		System.out.printf("CourseEJBImpl.getCourse(%s)%n", id);
		return new CourseDTO();
	}
}
