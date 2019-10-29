package com.goltd.agrigoussd.helpers;

import java.util.Objects;

public class UssdRequest {
    private String cellid;
    private String msisdn;
    private String sessionid;
    private String newRequest;
    private String input;

    public UssdRequest() {
    }

    public UssdRequest(String cellid, String msisdn, String sessionid, String newRequest, String input) {
        this.cellid = cellid;
        this.msisdn = msisdn;
        this.sessionid = sessionid;
        this.newRequest = newRequest;
        this.input = input;
    }

    public String getCellid() {
        return cellid;
    }

    public void setCellid(String cellid) {
        this.cellid = cellid;
    }

    public String getMsisdn() {
        return msisdn;
    }

    public void setMsisdn(String msisdn) {
        this.msisdn = msisdn;
    }

    public String getSessionid() {
        return sessionid;
    }

    public void setSessionid(String sessionid) {
        this.sessionid = sessionid;
    }

    public String getNewRequest() {
        return newRequest;
    }

    public void setNewRequest(String newRequest) {
        this.newRequest = newRequest;
    }

    public String getInput() {
        return input;
    }

    public void setInput(String input) {
        this.input = input;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        UssdRequest that = (UssdRequest) o;
        return Objects.equals(cellid, that.cellid) &&
                Objects.equals(msisdn, that.msisdn) &&
                Objects.equals(sessionid, that.sessionid) &&
                Objects.equals(newRequest, that.newRequest) &&
                Objects.equals(input, that.input);
    }

    @Override
    public int hashCode() {
        return Objects.hash(cellid, msisdn, sessionid, newRequest, input);
    }

    @Override
    public String toString() {
        return "UssdRequest{" +
                "previousState='" + cellid + '\'' +
                ", msisdn='" + msisdn + '\'' +
                ", sessionid='" + sessionid + '\'' +
                ", newRequest='" + newRequest + '\'' +
                ", input='" + input + '\'' +
                '}';
    }
}
