package it.unipi.dsmt.student_platform.dto;

import org.jetbrains.annotations.NotNull;

import java.io.Serializable;
import java.time.LocalTime;

/**
 * DTO used for transferring the necessary data to create a new course
 * inside the database.
 */
public class CourseCreationDTO implements Serializable {
	
	private String name;
	private String professorId;
	private String description;
	private int weekday;
	private LocalTime startTime;
	private LocalTime endTime;
	
	public CourseCreationDTO (@NotNull String name,
	                          @NotNull String professorId,
	                          @NotNull String description,
	                          int weekday,
	                          @NotNull LocalTime startTime,
	                          @NotNull LocalTime endTime)
	{
		this.name = name;
		this.professorId = professorId;
		this.description = description;
		this.weekday = weekday;
		this.startTime = startTime;
		this.endTime = endTime;
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
	
	public int getWeekday() {
		return weekday;
	}
	
	public void setWeekday(int weekday) {
		this.weekday = weekday;
	}
	
	public LocalTime getStartTime() {
		return startTime;
	}
	
	public void setStartTime(LocalTime startTime) {
		this.startTime = startTime;
	}
	
	public LocalTime getEndTime() {
		return endTime;
	}
	
	public void setEndTime(LocalTime endTime) {
		this.endTime = endTime;
	}
	
	@Override
	public String toString() {
		return "CourseCreationDTO{" +
				"name='" + name + '\'' +
				", professorId='" + professorId + '\'' +
				", description='" + description + '\'' +
				", weekday=" + weekday +
				", startTime=" + startTime +
				", endTime=" + endTime +
				'}';
	}
	
}
