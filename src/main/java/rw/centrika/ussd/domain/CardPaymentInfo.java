package rw.centrika.ussd.domain;

import com.fasterxml.jackson.annotation.JsonProperty;

public class CardPaymentInfo {

    @JsonProperty("TktID")
    private int ticketId;
    @JsonProperty("Cct")
    private String firstName;
    @JsonProperty("Rte")
    private String route;
    @JsonProperty("Date")
    private String departureDate;
    @JsonProperty("Hr")
    private String departureHour;
    @JsonProperty("CIn")
    private String cityIn;
    @JsonProperty("COut")
    private String cityOut;
    @JsonProperty("TPhone")
    private String phoneNumber;
    @JsonProperty("Traveller")
    private String traveller;
    @JsonProperty("CCode")
    private String cCode;
    @JsonProperty("Price")
    private String price;
    @JsonProperty("Disc")
    private String description;
    @JsonProperty("Ttl")
    private String total;
    @JsonProperty("Pnts")
    private String points;
    @JsonProperty("Pos")
    private String pos;
    @JsonProperty("RNum")
    private String routeNumber;
    @JsonProperty("RDate")
    private String routeDate;
    @JsonProperty("Msg")
    private String message;
    @JsonProperty("hash")
    private String hash;
    @JsonProperty("SNo")
    private String serialNumber;
    @JsonProperty("DTime")
    private String departuretime;
    @JsonProperty("DocID")
    private String documentId;
    @JsonProperty("IsTapNGoTapDone")
    private String isTapAndGoTopDone;
    @JsonProperty("PLATENO")
    private String plateNumber;
    @JsonProperty("FullName")
    private String fullName;
    @JsonProperty("CompanyId")
    private String companyId;
    @JsonProperty("CardWalletAmount")
    private String cardWalletAmount;
    @JsonProperty("Currency")
    private String currency;

    public CardPaymentInfo() {
        //
    }

    public int getTicketId() {
        return ticketId;
    }

    public void setTicketId(int ticketId) {
        this.ticketId = ticketId;
    }

    public String getFirstName() {
        return firstName;
    }

    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }

    public String getRoute() {
        return route;
    }

    public void setRoute(String route) {
        this.route = route;
    }

    public String getDepartureDate() {
        return departureDate;
    }

    public void setDepartureDate(String departureDate) {
        this.departureDate = departureDate;
    }

    public String getDepartureHour() {
        return departureHour;
    }

    public void setDepartureHour(String departureHour) {
        this.departureHour = departureHour;
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

    public String getPhoneNumber() {
        return phoneNumber;
    }

    public void setPhoneNumber(String phoneNumber) {
        this.phoneNumber = phoneNumber;
    }

    public String getTraveller() {
        return traveller;
    }

    public void setTraveller(String traveller) {
        this.traveller = traveller;
    }

    public String getcCode() {
        return cCode;
    }

    public void setcCode(String cCode) {
        this.cCode = cCode;
    }

    public String getPrice() {
        return price;
    }

    public void setPrice(String price) {
        this.price = price;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getTotal() {
        return total;
    }

    public void setTotal(String total) {
        this.total = total;
    }

    public String getPoints() {
        return points;
    }

    public void setPoints(String points) {
        this.points = points;
    }

    public String getPos() {
        return pos;
    }

    public void setPos(String pos) {
        this.pos = pos;
    }

    public String getRouteNumber() {
        return routeNumber;
    }

    public void setRouteNumber(String routeNumber) {
        this.routeNumber = routeNumber;
    }

    public String getRouteDate() {
        return routeDate;
    }

    public void setRouteDate(String routeDate) {
        this.routeDate = routeDate;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public String getHash() {
        return hash;
    }

    public void setHash(String hash) {
        this.hash = hash;
    }

    public String getSerialNumber() {
        return serialNumber;
    }

    public void setSerialNumber(String serialNumber) {
        this.serialNumber = serialNumber;
    }

    public String getDeparturetime() {
        return departuretime;
    }

    public void setDeparturetime(String departuretime) {
        this.departuretime = departuretime;
    }

    public String getDocumentId() {
        return documentId;
    }

    public void setDocumentId(String documentId) {
        this.documentId = documentId;
    }

    public String getIsTapAndGoTopDone() {
        return isTapAndGoTopDone;
    }

    public void setIsTapAndGoTopDone(String isTapAndGoTopDone) {
        this.isTapAndGoTopDone = isTapAndGoTopDone;
    }

    public String getPlateNumber() {
        return plateNumber;
    }

    public void setPlateNumber(String plateNumber) {
        this.plateNumber = plateNumber;
    }

    public String getFullName() {
        return fullName;
    }

    public void setFullName(String fullName) {
        this.fullName = fullName;
    }

    public String getCompanyId() {
        return companyId;
    }

    public void setCompanyId(String companyId) {
        this.companyId = companyId;
    }

    public String getCardWalletAmount() {
        return cardWalletAmount;
    }

    public void setCardWalletAmount(String cardWalletAmount) {
        this.cardWalletAmount = cardWalletAmount;
    }

    public String getCurrency() {
        return currency;
    }

    public void setCurrency(String currency) {
        this.currency = currency;
    }


    @Override
    public String toString() {
        return "CardPaymentInfo{" +
                "ticketId=" + ticketId +
                ", firstName='" + firstName + '\'' +
                ", route='" + route + '\'' +
                ", departureDate='" + departureDate + '\'' +
                ", departureHour='" + departureHour + '\'' +
                ", cityIn='" + cityIn + '\'' +
                ", cityOut='" + cityOut + '\'' +
                ", phoneNumber='" + phoneNumber + '\'' +
                ", traveller='" + traveller + '\'' +
                ", cCode='" + cCode + '\'' +
                ", price='" + price + '\'' +
                ", description='" + description + '\'' +
                ", total='" + total + '\'' +
                ", points='" + points + '\'' +
                ", pos='" + pos + '\'' +
                ", routeNumber='" + routeNumber + '\'' +
                ", routeDate='" + routeDate + '\'' +
                ", message='" + message + '\'' +
                ", hash='" + hash + '\'' +
                ", serialNumber='" + serialNumber + '\'' +
                ", departuretime='" + departuretime + '\'' +
                ", documentId='" + documentId + '\'' +
                ", isTapAndGoTopDone='" + isTapAndGoTopDone + '\'' +
                ", plateNumber='" + plateNumber + '\'' +
                ", fullName='" + fullName + '\'' +
                ", companyId='" + companyId + '\'' +
                ", cardWalletAmount='" + cardWalletAmount + '\'' +
                ", currency='" + currency + '\'' +
                '}';
    }
}
