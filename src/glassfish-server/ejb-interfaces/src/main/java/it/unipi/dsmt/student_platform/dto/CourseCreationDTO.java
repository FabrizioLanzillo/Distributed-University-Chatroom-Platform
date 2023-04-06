package it.unipi.dsmt.student_platform.dto;

import org.jetbrains.annotations.NotNull;

import java.io.Serializable;

/**
 * DTO used for transferring the necessary data to create a new course
 * inside the database.
 */
public class CourseCreationDTO implements Serializable {
	private String name;
	private String professorId;
	private String description;
	
	public CourseCreationDTO (@NotNull String name,
	                          @NotNull String professorId,
	                          @NotNull String description)
	{
		this.name = name;
		this.professorId = professorId;
		this.description = description;
	}
	
	public String getName() {
		return name;
	}
	
	public void setName(String name) {
		this.name = name;
	}
	
	public String getProfessorId() {
		return professorId;
	}
	
	public void setProfessorId(String professorId) {
		this.professorId = professorId;
	}
	
	public String getDescription() {
		return description;
	}
	
	public void setDescription(String description) {
		this.description = description;
	}
}
