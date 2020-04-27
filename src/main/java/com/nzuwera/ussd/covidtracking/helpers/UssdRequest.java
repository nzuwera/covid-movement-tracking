package com.nzuwera.ussd.covidtracking.helpers;

import org.apache.commons.lang.StringEscapeUtils;

public class UssdRequest {
    private String cellid;
    private String msisdn;
    private String sessionid;
    private String newRequest;
    private String input;

    public UssdRequest() {
        //
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
        return StringEscapeUtils.escapeHtml(input);
    }

    public void setInput(String input) {
        this.input = input;
    }

    @Override
    public String toString() {
        return "UssdRequest{" +
                "cellid='" + cellid + '\'' +
                ", msisdn='" + msisdn + '\'' +
                ", sessionid='" + sessionid + '\'' +
                ", newRequest='" + newRequest + '\'' +
                ", input='" + input + '\'' +
                '}';
    }
}
