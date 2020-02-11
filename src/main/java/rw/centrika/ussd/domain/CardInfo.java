package rw.centrika.ussd.domain;

import com.fasterxml.jackson.annotation.JsonProperty;

public class CardInfo {
    /*
        "Id": 161,
        "FirstName": "SHEETAL",
        "LastName": "Patil",
        "CardSerialNumber": "CENT190701000514",
        "Balance": 78872,
        "Currency": "RWF"
    */
    @JsonProperty("Id")
    private int id;
    @JsonProperty("FirstName")
    private String firstName;
    @JsonProperty("LastName")
    private String lastName;
    @JsonProperty("CardSerialNumber")
    private String cardSerialNumber;
    @JsonProperty("Balance")
    private double balance;
    @JsonProperty("Currency")
    private String currency;

    public CardInfo() {
        //
    }

    public CardInfo(int id, String firstName, String lastName, String cardSerialNumber, double balance, String currency) {
        this.id = id;
        this.firstName = firstName;
        this.lastName = lastName;
        this.cardSerialNumber = cardSerialNumber;
        this.balance = balance;
        this.currency = currency;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public String getFirstName() {
        return firstName;
    }

    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }

    public String getLastName() {
        return lastName;
    }

    public void setLastName(String lastName) {
        this.lastName = lastName;
    }

    public String getCardSerialNumber() {
        return cardSerialNumber;
    }

    public void setCardSerialNumber(String cardSerialNumber) {
        this.cardSerialNumber = cardSerialNumber;
    }

    public double getBalance() {
        return balance;
    }

    public void setBalance(double balance) {
        this.balance = balance;
    }

    public String getCurrency() {
        return currency;
    }

    public void setCurrency(String currency) {
        this.currency = currency;
    }

    @Override
    public String toString() {
        return "CardInfo{" +
                "id=" + id +
                ", firstName='" + firstName + '\'' +
                ", lastName='" + lastName + '\'' +
                ", cardSerialNumber='" + cardSerialNumber + '\'' +
                ", balance=" + balance +
                ", currency='" + currency + '\'' +
                '}';
    }
}
