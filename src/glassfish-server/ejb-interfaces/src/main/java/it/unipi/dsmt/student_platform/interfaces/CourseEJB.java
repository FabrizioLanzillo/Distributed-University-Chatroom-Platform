package it.unipi.dsmt.student_platform.interfaces;

import it.unipi.dsmt.student_platform.dto.CourseCreationDTO;
import it.unipi.dsmt.student_platform.dto.CourseDTO;
import jakarta.ejb.Remote;
import org.jetbrains.annotations.NotNull;

import java.util.List;

@Remote
public interface CourseEJB {
	
	boolean createCourse(@NotNull CourseCreationDTO course);
	
	CourseDTO getCourse (int id);
	List<CourseDTO> getCourse (String name);
	List<CourseDTO> getAllCourses ();
	List<CourseDTO> getStarredCourses(String username);
	
	boolean addStarredCourse(@NotNull String studentId, int courseId);
	boolean removeStarredCourse(@NotNull String studentId, int courseId);
	
}
