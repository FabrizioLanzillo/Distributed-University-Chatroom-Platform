package it.unipi.dsmt.student_platform.interfaces;

import it.unipi.dsmt.student_platform.dto.CourseCreationDTO;
import it.unipi.dsmt.student_platform.dto.CourseDTO;
import jakarta.ejb.Remote;
import org.jetbrains.annotations.NotNull;

import java.util.List;

@Remote
public interface CourseEJB {
	CourseDTO getCourse (int id);
	public List<CourseDTO> getCourse (String name);
	public List<CourseDTO> getAllCourses ();
	public List<CourseDTO> getStarredCourses(String username);
	public boolean addStarredCourse(String student, String id);
	boolean createCourse (@NotNull CourseCreationDTO course);
}
