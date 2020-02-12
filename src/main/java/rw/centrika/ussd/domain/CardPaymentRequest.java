package rw.centrika.ussd.domain;

import com.fasterxml.jackson.annotation.JsonProperty;

public class CardPaymentRequest {
    @JsonProperty("ClientId")
    private String clientId;
    @JsonProperty("IDSale")
    private int idSale;
    @JsonProperty("UserName")
    private String username;
    @JsonProperty("Password")
    private String passworx;
    @JsonProperty("DEPTDATE")
    private String departureDate;
    @JsonProperty("DEPTTIME")
    private String departureTime;
    @JsonProperty("CityIn")
    private String cityIn;
    @JsonProperty("CityOut")
    private String cityOut;
    @JsonProperty("CardNo")
    private String cardNumber;
    @JsonProperty("Lat")
    private String latitude;
    @JsonProperty("Long")
    private String longitude;
    @JsonProperty("WebServiceName")
    private String webServiceName;
    @JsonProperty("TicketType")
    private String ticketType;
    @JsonProperty("Creator")
    private String creator;


    public CardPaymentRequest() {
    }

    public CardPaymentRequest(int idSale, String departureDate, String departureTime, String cityIn, String cityOut, String cardNumber, String creator) {
        this.clientId = "3";
        this.idSale = idSale;
        this.username = "1003";
        this.passworx = "1234";
        this.departureDate = departureDate;
        this.departureTime = departureTime;
        this.cityIn = cityIn;
        this.cityOut = cityOut;
        this.cardNumber = cardNumber;
        this.latitude = "";
        this.longitude = "";
        this.webServiceName = "GetSafariBusCardTicket";
        this.ticketType = "USSD";
        this.creator = creator;
    }

    public String getClientId() {
        return clientId;
    }

    public void setClientId(String clientId) {
        this.clientId = clientId;
    }

    public int getIdSale() {
        return idSale;
    }

    public void setIdSale(int idSale) {
        this.idSale = idSale;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getPassworx() {
        return passworx;
    }

    public void setPassworx(String passworx) {
        this.passworx = passworx;
    }

    public String getDepartureDate() {
        return departureDate;
    }

    public void setDepartureDate(String departureDate) {
        this.departureDate = departureDate;
    }

    public String getDepartureTime() {
        return departureTime;
    }

    public void setDepartureTime(String departureTime) {
        this.departureTime = departureTime;
    }

    public String getCityIn() {
        return cityIn;
    }

    public void setCityIn(String cityIn) {
        this.cityIn = cityIn;
    }

    public String getCityOut() {
        return cityOut;
    }

    public void setCityOut(String cityOut) {
        this.cityOut = cityOut;
    }

    public String getCardNumber() {
        return cardNumber;
    }

    public void setCardNumber(String cardNumber) {
        this.cardNumber = cardNumber;
    }

    public String getLatitude() {
        return latitude;
    }

    public void setLatitude(String latitude) {
        this.latitude = latitude;
    }

    public String getLongitude() {
        return longitude;
    }

    public void setLongitude(String longitude) {
        this.longitude = longitude;
    }

    public String getWebServiceName() {
        return webServiceName;
    }

    public void setWebServiceName(String webServiceName) {
        this.webServiceName = webServiceName;
    }

    public String getTicketType() {
        return ticketType;
    }

    public void setTicketType(String ticketType) {
        this.ticketType = ticketType;
    }

    public String getCreator() {
        return creator;
    }

    public void setCreator(String creator) {
        this.creator = creator;
    }

    @Override
    public String toString() {
        return "CardPaymentRequest{" +
                "clientId='" + clientId + '\'' +
                ", idSale=" + idSale +
                ", username='" + username + '\'' +
                ", passworx='" + passworx + '\'' +
                ", departureDate='" + departureDate + '\'' +
                ", departureTime='" + departureTime + '\'' +
                ", cityIn='" + cityIn + '\'' +
                ", cityOut='" + cityOut + '\'' +
                ", cardNumber='" + cardNumber + '\'' +
                ", latitude='" + latitude + '\'' +
                ", longitude='" + longitude + '\'' +
                ", webServiceName='" + webServiceName + '\'' +
                ", ticketType='" + ticketType + '\'' +
                ", creator='" + creator + '\'' +
                '}';
    }
}
