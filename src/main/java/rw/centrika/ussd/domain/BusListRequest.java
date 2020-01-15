package rw.centrika.ussd.domain;

import com.fasterxml.jackson.annotation.JsonProperty;

public class BusListRequest {
    @JsonProperty("CityIn")
    private String cityIn;
    @JsonProperty("CityOut")
    private String cityOut;
    @JsonProperty("StartDate")
    private String startDate;
    @JsonProperty("StartTime")
    private String startTime;
    @JsonProperty("EndDate")
    private String endDate;
    @JsonProperty("EndTime")
    private String endTime;
    @JsonProperty("IsRoundTrip")
    private String isRoundTrip;
    @JsonProperty("BusClass")
    private String busClass;
    @JsonProperty("Currency")
    private String currency;

    public BusListRequest() {
        //
    }

    public BusListRequest(String cityIn, String cityOut, String startDate, String startTime) {
        this.cityIn = cityIn;
        this.cityOut = cityOut;
        this.startDate = startDate;
        this.startTime = startTime;
        this.endDate = startDate;
        this.endTime = startTime;
        this.isRoundTrip = "false";
        this.busClass = "Economy";
        this.currency = "RWF";
    }

    public String getCityIn() {
        return cityIn;
    }

    public void setCityIn(String cityIn) {
        this.cityIn = cityIn;
    }

    public String getCityOut() {
        return cityOut;
    }

    public void setCityOut(String cityOut) {
        this.cityOut = cityOut;
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

    public String getEndDate() {
        return endDate;
    }

    public void setEndDate(String endDate) {
        this.endDate = endDate;
    }

    public String getEndTime() {
        return endTime;
    }

    public void setEndTime(String endTime) {
        this.endTime = endTime;
    }

    public String getIsRoundTrip() {
        return isRoundTrip;
    }

    public void setIsRoundTrip(String isRoundTrip) {
        this.isRoundTrip = isRoundTrip;
    }

    public String getBusClass() {
        return busClass;
    }

    public void setBusClass(String busClass) {
        this.busClass = busClass;
    }

    public String getCurrency() {
        return currency;
    }

    public void setCurrency(String currency) {
        this.currency = currency;
    }

    @Override
    public String toString() {
        return "BusListRequest{" +
                "cityIn='" + cityIn + '\'' +
                ", cityOut='" + cityOut + '\'' +
                ", startDate='" + startDate + '\'' +
                ", startTime='" + startTime + '\'' +
                ", endDate='" + endDate + '\'' +
                ", endTime='" + endTime + '\'' +
                ", isRoundTrip='" + isRoundTrip + '\'' +
                ", busClass='" + busClass + '\'' +
                ", currency='" + currency + '\'' +
                '}';
    }
}
