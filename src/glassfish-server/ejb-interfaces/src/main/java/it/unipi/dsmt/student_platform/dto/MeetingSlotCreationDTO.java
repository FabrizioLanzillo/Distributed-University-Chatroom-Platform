package it.unipi.dsmt.student_platform.dto;

import org.jetbrains.annotations.NotNull;

import java.io.Serializable;
import java.time.LocalTime;

public class MeetingSlotCreationDTO implements Serializable {
	private int weekday;
	private LocalTime time;
	
	public MeetingSlotCreationDTO (int weekday, @NotNull LocalTime time) {
		this.weekday = weekday;
		this.time = time;
	}
	
	public int getWeekday() {
		return weekday;
	}
	
	public void setWeekday(int weekday) {
		this.weekday = weekday;
	}
	
	public LocalTime getTime() {
		return time;
	}
	
	public void setTime(LocalTime time) {
		this.time = time;
	}
}
