package com.nzuwera.ussd.covidtracking.helpers;

import com.nzuwera.ussd.covidtracking.helpers.enums.Freeflow;

public class UssdResponse {
    private String message;
    private Freeflow freeflow;

    public UssdResponse() {
        // Empty constructor
    }

    public UssdResponse(String message, Freeflow freeflow) {
        this.message = message;
        this.freeflow = freeflow;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public Freeflow getFreeflow() {
        return freeflow;
    }

    public void setFreeflow(Boolean isLeaf) {
        this.freeflow = (Boolean.TRUE.equals(isLeaf)) ? Freeflow.FB : Freeflow.FC;
    }

    @Override
    public String toString() {
        return "UssdResponse{" +
                "message='" + message + '\'' +
                ", freeflow=" + freeflow +
                '}';
    }
}
