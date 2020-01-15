package rw.centrika.ussd.helpers;

import com.fasterxml.jackson.annotation.JsonProperty;
import rw.centrika.ussd.domain.BusList;

import java.util.List;

public class BusListSuccess {
    @JsonProperty("result")
    private List<BusList> result;
    private String status;
    private String error;

    public BusListSuccess() {
        //
    }

    public BusListSuccess(List<BusList>  result, String status, String error) {
        this.result = result;
        this.status = status;
        this.error = error;
    }

    public List<BusList> getResult() {
        return result;
    }

    public void setResult(List<BusList>  result) {
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
