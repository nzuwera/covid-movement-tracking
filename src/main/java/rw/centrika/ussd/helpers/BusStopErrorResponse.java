package rw.centrika.ussd.helpers;

public class BusStopErrorResponse {
    private String message;

    public BusStopErrorResponse() {
        //
    }

    public BusStopErrorResponse(String error) {
        this.message = error;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    @Override
    public String toString() {
        return "BusResponse{" +
                ", message='" + message + '\'' +
                '}';
    }
}
