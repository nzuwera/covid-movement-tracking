package rw.centrika.ussd.domain;

import com.fasterxml.jackson.annotation.JsonProperty;

public class CardPaymentResponse {

    @JsonProperty("result")
    private CardPaymentInfo result;
    private String status;
    @JsonProperty("error")
    private ErrorInfo error;

    public CardPaymentResponse() {
    }

    public CardPaymentResponse(CardPaymentInfo result, String status, ErrorInfo error) {
        this.result = result;
        this.status = status;
        this.error = error;
    }

    public CardPaymentInfo getResult() {
        return result;
    }

    public void setResult(CardPaymentInfo result) {
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
