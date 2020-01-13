package rw.centrika.ussd.helpers;

public class BusStopResponseObject {
    private Boolean status;
    private String message;

    public BusStopResponseObject() {
        //
    }

    public BusStopResponseObject(Boolean status, String message) {
        this.status = status;
        this.message = message;
    }

    public Boolean getStatus() {
        return status;
    }

    public void setStatus(Boolean status) {
        this.status = status;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    @Override
    public String toString() {
        return "BusStopResponseObject{" +
                "status=" + status +
                ", message='" + message + '\'' +
                '}';
    }
}
