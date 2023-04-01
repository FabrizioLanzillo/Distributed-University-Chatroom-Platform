package it.unipi.dsmt.student_platform.dto;

import org.jetbrains.annotations.NotNull;

import java.io.Serializable;

public class CourseDTO implements Serializable {
	
	private int id;
	private String name;
	private ProfessorDTO professor;
	private String description;
	private boolean isStarred;
	
	public CourseDTO(int id,
	                 @NotNull String name,
	                 @NotNull ProfessorDTO professor,
	                 @NotNull String description) // TODO remove this constructor
	{
		this.id = id;
		this.name = name;
		this.professor = professor;
		this.description = description;
		this.isStarred = false;
	}
	
	public CourseDTO(int id,
	                 @NotNull String name,
	                 @NotNull ProfessorDTO professor,
	                 @NotNull String description,
	                 boolean isStarred)
	{
		this.id = id;
		this.name = name;
		this.professor = professor;
		this.description = description;
		this.isStarred = isStarred;
	}
	
	public int getId() {
		return id;
	}
	
	public void setId(int id) {
		this.id = id;
	}
	
	public String getName() {
		return name;
	}
	
	public void setName(String name) {
		this.name = name;
	}
	
	public ProfessorDTO getProfessor() {
		return professor;
	}
	
	public void setProfessor(ProfessorDTO professor) {
		this.professor = professor;
	}
	
	public String getDescription() {
		return description;
	}
	
	public void setDescription(String description) {
		this.description = description;
	}
	
	public boolean isStarred() {
		return isStarred;
	}
	
	public void setStarred(boolean starred) {
		isStarred = starred;
	}
	
	@Override
	public String toString() {
		return "CourseDTO{" +
				"id=" + id +
				", name='" + name + '\'' +
				", professor=" + professor +
				", description='" + description + '\'' +
				", isStarred=" + isStarred +
				'}';
	}
}
