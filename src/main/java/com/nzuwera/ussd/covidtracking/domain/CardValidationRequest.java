package com.nzuwera.ussd.covidtracking.domain;

import com.fasterxml.jackson.annotation.JsonProperty;

public class CardValidationRequest {

    @JsonProperty("CardSerialNumber")
    private String cardSerialNumber;

    public CardValidationRequest() {
    }

    public CardValidationRequest(String cardSerialNumber) {
        this.cardSerialNumber = cardSerialNumber;
    }

    public String getCardSerialNumber() {
        return cardSerialNumber;
    }

    public void setCardSerialNumber(String cardSerialNumber) {
        this.cardSerialNumber = cardSerialNumber;
    }

    @Override
    public String toString() {
        return "CardValidationRequest{" +
                "cardSerialNumber='" + cardSerialNumber + '\'' +
                '}';
    }
}
