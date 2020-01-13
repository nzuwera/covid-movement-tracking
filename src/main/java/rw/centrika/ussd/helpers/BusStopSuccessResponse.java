package rw.centrika.ussd.helpers;

import com.fasterxml.jackson.annotation.JsonProperty;
import rw.centrika.ussd.domain.BusStop;

import java.util.List;

public class BusStopSuccessResponse {
    @JsonProperty("result")
    private List<BusStop> result;
    private String status;
    private String error;

    public BusStopSuccessResponse() {
        //
    }

    public BusStopSuccessResponse(List<BusStop>  result, String status, String error) {
        this.result = result;
        this.status = status;
        this.error = error;
    }

    public List<BusStop> getResult() {
        return result;
    }

    public void setResult(List<BusStop>  result) {
        this.result = result;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getError() {
        return error;
    }

    public void setError(String error) {
        this.error = error;
    }

    @Override
    public String toString() {
        return "BusResponse{" +
                "result=" + result +
                ", status='" + status + '\'' +
                ", error='" + error + '\'' +
                '}';
    }
}
