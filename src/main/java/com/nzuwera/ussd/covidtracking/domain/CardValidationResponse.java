package com.nzuwera.ussd.covidtracking.domain;

import com.fasterxml.jackson.annotation.JsonProperty;

public class CardValidationResponse {

    @JsonProperty("result")
    private CardInfo result;
    private String status;
    @JsonProperty("error")
    private ErrorInfo error;

    public CardValidationResponse() {
    }

    public CardValidationResponse(CardInfo result, String status, ErrorInfo error) {
        this.result = result;
        this.status = status;
        this.error = error;
    }

    public CardInfo getResult() {
        return result;
    }

    public void setName(CardInfo result) {
        this.result = result;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public ErrorInfo getError() {
        return error;
    }

    public void setError(ErrorInfo error) {
        this.error = error;
    }

    @Override
    public String toString() {
        return "CardValidationResponse{" +
                "result=" + result +
                ", status='" + status + '\'' +
                ", error=" + error +
                '}';
    }
}
