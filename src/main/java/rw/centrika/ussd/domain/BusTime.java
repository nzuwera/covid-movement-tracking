package rw.centrika.ussd.domain;

public class BusTime {

    private String startDate;
    private String startTime;

    public BusTime() {
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
