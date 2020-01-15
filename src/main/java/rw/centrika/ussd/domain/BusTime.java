package rw.centrika.ussd.domain;

import rw.centrika.ussd.helpers.UTKit;

public class BusTime {

    private String startDate;
    private String startTime;

    public BusTime() {
    }
public BusTime(String selectedTime){
        String[] times = selectedTime.split(UTKit.BLANK);
        this.startDate = times[0];
        this.startTime = times[1];
}
    public BusTime(String startDate, String startTime) {
        this.startDate = startDate;
        this.startTime = startTime;
    }

    public String getStartDate() {
        return startDate;
    }

    public void setStartDate(String startDate) {
        this.startDate = startDate;
    }

    public String getStartTime() {
        return startTime;
    }

    public void setStartTime(String startTime) {
        this.startTime = startTime;
    }

    @Override
    public String toString() {
        return "BusTime{" +
                "startDate='" + startDate + '\'' +
                ", startTime='" + startTime + '\'' +
                '}';
    }
}
