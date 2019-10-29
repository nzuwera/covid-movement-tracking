package com.goltd.agrigoussd.helpers;

import java.util.Objects;

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
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        UssdResponse that = (UssdResponse) o;
        return Objects.equals(message, that.message) &&
                freeflow == that.freeflow;
    }

    @Override
    public int hashCode() {
        return Objects.hash(message, freeflow);
    }

    @Override
    public String toString() {
        return "UssdResponse{" +
                "message='" + message + '\'' +
                ", freeflow=" + freeflow +
                '}';
    }
}
