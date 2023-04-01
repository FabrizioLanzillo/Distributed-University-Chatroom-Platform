package it.unipi.dsmt.student_platform.interfaces;

import it.unipi.dsmt.student_platform.dto.CourseCreationDTO;
import it.unipi.dsmt.student_platform.dto.CourseDTO;
import it.unipi.dsmt.student_platform.dto.MinimalCourseDTO;
import jakarta.ejb.Remote;
import org.jetbrains.annotations.NotNull;

import java.util.List;

@Remote
public interface CourseEJB {
	
	boolean createCourse(@NotNull CourseCreationDTO course);
	
	CourseDTO getCourseDetails (int id, String userId);
	List<MinimalCourseDTO> searchCourses (String name);
	List<MinimalCourseDTO> searchCoursesForProfessor (String name, String professorId);
	List<MinimalCourseDTO> getAllCourses ();
	public List<MinimalCourseDTO> getAllCoursesForProfessor (String professorId);
	List<MinimalCourseDTO> getStarredCourses(String id);
	boolean addStarredCourse(@NotNull String studentId, int courseId);
	boolean removeStarredCourse(@NotNull String studentId, int courseId);
	boolean deleteCourse(int id);
}
