package it.unipi.dsmt.student_platform.interfaces;

import it.unipi.dsmt.student_platform.dto.CourseCreationDTO;
import it.unipi.dsmt.student_platform.dto.CourseDTO;
import jakarta.ejb.Remote;
import org.jetbrains.annotations.NotNull;

@Remote
public interface CourseEJB {
	CourseDTO getCourse (int id);
	boolean createCourse (@NotNull CourseCreationDTO course);
}
