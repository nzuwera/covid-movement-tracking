package com.goltd.agrigoussd.helpers;

import com.goltd.agrigoussd.helpers.enums.Freeflow;

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

    public void setFreeflow(Freeflow freeflow) {
        this.freeflow = freeflow;
    }

    @Override
    public String toString() {
        return "UssdResponse{" +
                "message='" + message + '\'' +
                ", freeflow=" + freeflow +
                '}';
    }
}
