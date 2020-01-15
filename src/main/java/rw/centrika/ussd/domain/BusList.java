package rw.centrika.ussd.domain;

import org.json.simple.JSONObject;

public class BusList {


    private int companyId;
    private String companyName;
    private String saleId;
    private String trajetId;
    private String status;
    private String plateNumber;
    private String departureTime;
    private String name;
    private String price;
    private String cityIn;
    private String cityOut;
    private String targetDate;
    private String busCategory;
    private String offerType;
    private String offerInPercentage;
    private String offerDescription;
    private String discount;
    private String totalAmount;
    private String currency;
    private String aboutTrip1;
    private String aboutTrip2;
    private String aboutTrip3;
    private String rating;
    private String logoUrl;

    public BusList() {
    }

    public BusList(JSONObject name) {
        this.companyId = Integer.parseInt(name.get("CompanyId").toString());
        this.companyName = name.get("CompanyName").toString();
        this.saleId = name.get("IDSALE").toString();
        this.trajetId = name.get("IDTRAJET").toString();
        this.status = name.get("STATUS").toString();
        this.plateNumber = name.get("PLATENO").toString();
        this.departureTime = name.get("DEPTTIME").toString();
        this.name = name.get("NAME").toString();
        this.price = name.get("PRICE").toString();
        this.cityIn = name.get("CityIn").toString();
        this.cityOut = name.get("CityOut").toString();
        this.targetDate = name.get("TargetDate").toString();
        this.offerType = name.get("OFFERTYPE").toString();
        this.offerInPercentage = name.get("OFFERINPERCENTAGE").toString();
        this.offerDescription = name.get("OFFERDESCRIPTION").toString();
        this.discount = name.get("DISCOUNT").toString();
        this.totalAmount = name.get("TOTALAMT").toString();
        this.currency = name.get("CURRENCY").toString();
        this.aboutTrip1 = name.get("ABOUTTRIP1").toString();
        this.aboutTrip2 = name.get("ABOUTTRIP2").toString();
        this.aboutTrip3 = name.get("ABOUTTRIP3").toString();
        this.rating = name.get("RATING").toString();
        this.logoUrl = name.get("LogoUrl").toString();
    }

    public int getCompanyId() {
        return companyId;
    }

    public void setCompanyId(int companyId) {
        this.companyId = companyId;
    }

    public String getCompanyName() {
        return companyName;
    }

    public void setCompanyName(String companyName) {
        this.companyName = companyName;
    }

    public String getSaleId() {
        return saleId;
    }

    public void setSaleId(String saleId) {
        this.saleId = saleId;
    }

    public String getTrajetId() {
        return trajetId;
    }

    public void setTrajetId(String trajetId) {
        this.trajetId = trajetId;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getPlateNumber() {
        return plateNumber;
    }

    public void setPlateNumber(String plateNumber) {
        this.plateNumber = plateNumber;
    }

    public String getDepartureTime() {
        return departureTime;
    }

    public void setDepartureTime(String departureTime) {
        this.departureTime = departureTime;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getPrice() {
        return price;
    }

    public void setPrice(String price) {
        this.price = price;
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

    public String getTargetDate() {
        return targetDate;
    }

    public void setTargetDate(String targetDate) {
        this.targetDate = targetDate;
    }

    public String getBusCategory() {
        return busCategory;
    }

    public void setBusCategory(String busCategory) {
        this.busCategory = busCategory;
    }

    public String getOfferType() {
        return offerType;
    }

    public void setOfferType(String offerType) {
        this.offerType = offerType;
    }

    public String getOfferInPercentage() {
        return offerInPercentage;
    }

    public void setOfferInPercentage(String offerInPercentage) {
        this.offerInPercentage = offerInPercentage;
    }

    public String getOfferDescription() {
        return offerDescription;
    }

    public void setOfferDescription(String offerDescription) {
        this.offerDescription = offerDescription;
    }

    public String getDiscount() {
        return discount;
    }

    public void setDiscount(String discount) {
        this.discount = discount;
    }

    public String getTotalAmount() {
        return totalAmount;
    }

    public void setTotalAmount(String totalAmount) {
        this.totalAmount = totalAmount;
    }

    public String getCurrency() {
        return currency;
    }

    public void setCurrency(String currency) {
        this.currency = currency;
    }

    public String getAboutTrip1() {
        return aboutTrip1;
    }

    public void setAboutTrip1(String aboutTrip1) {
        this.aboutTrip1 = aboutTrip1;
    }

    public String getAboutTrip2() {
        return aboutTrip2;
    }

    public void setAboutTrip2(String aboutTrip2) {
        this.aboutTrip2 = aboutTrip2;
    }

    public String getAboutTrip3() {
        return aboutTrip3;
    }

    public void setAboutTrip3(String aboutTrip3) {
        this.aboutTrip3 = aboutTrip3;
    }

    public String getRating() {
        return rating;
    }

    public void setRating(String rating) {
        this.rating = rating;
    }

    public String getLogoUrl() {
        return logoUrl;
    }

    public void setLogoUrl(String logoUrl) {
        this.logoUrl = logoUrl;
    }

    @Override
    public String toString() {
        return "BusList{" +
                "companyId=" + companyId +
                ", companyName='" + companyName + '\'' +
                ", saleId='" + saleId + '\'' +
                ", trajetId='" + trajetId + '\'' +
                ", status='" + status + '\'' +
                ", plateNumber='" + plateNumber + '\'' +
                ", departureTime='" + departureTime + '\'' +
                ", name='" + name + '\'' +
                ", price='" + price + '\'' +
                ", cityIn='" + cityIn + '\'' +
                ", cityOut='" + cityOut + '\'' +
                ", targetDate='" + targetDate + '\'' +
                ", busCategory='" + busCategory + '\'' +
                ", offerType='" + offerType + '\'' +
                ", offerInPercentage='" + offerInPercentage + '\'' +
                ", offerDescription='" + offerDescription + '\'' +
                ", discount='" + discount + '\'' +
                ", totalAmount='" + totalAmount + '\'' +
                ", currency='" + currency + '\'' +
                ", aboutTrip1='" + aboutTrip1 + '\'' +
                ", aboutTrip2='" + aboutTrip2 + '\'' +
                ", aboutTrip3='" + aboutTrip3 + '\'' +
                ", rating='" + rating + '\'' +
                ", logoUrl='" + logoUrl + '\'' +
                '}';
    }
}
