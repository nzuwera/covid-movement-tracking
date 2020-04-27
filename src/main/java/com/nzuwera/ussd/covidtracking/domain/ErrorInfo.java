package com.nzuwera.ussd.covidtracking.domain;

public class ErrorInfo {
    private String error;
    private String message;

    public ErrorInfo() {
    }

    public ErrorInfo(String error, String message) {
        this.error = error;
        this.message = message;
    }

    public String getError() {
        return error;
    }

    public void setError(String error) {
        this.error = error;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    @Override
    public String toString() {
        return "ErrorInfo{" +
                "error='" + error + '\'' +
                ", message='" + message + '\'' +
                '}';
    }
}
