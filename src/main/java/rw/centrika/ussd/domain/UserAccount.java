package rw.centrika.ussd.domain;

import javax.persistence.*;
import javax.validation.constraints.NotNull;


@Table(name = "USER_ACCOUNT", uniqueConstraints = {
        @UniqueConstraint(columnNames = "MSISDN", name = "CONSTRAINT_USER_ACCOUNT_MSISDN"),
        @UniqueConstraint(columnNames = "CARD_NUMBER", name = "CONSTRAINT_USER_ACCOUNT_CARD_NUMBER")
})
@Entity
public class UserAccount extends AbstractEntity {

    @Column(name = "MSISDN", nullable = false, columnDefinition = "varchar(15) not null")
    @NotNull
    private String msisdn;

    @Column(name = "CARD_NUMBER", columnDefinition = "varchar(50) default 'CENT000000000000'")
    private String cardNumber;

    @Column(name = "LANGUAGE", nullable = false, columnDefinition = "varchar(10) default 'KIN'")
    @Enumerated(EnumType.STRING)
    private Language language;

    public UserAccount() {
        // Empty Constructor
    }

    public UserAccount(@NotNull String msisdn) {
        this.cardNumber = "CENT000000000000";
        this.msisdn = msisdn;
        this.language = Language.KIN;
    }

    public String getMsisdn() {
        return msisdn;
    }

    public void setMsisdn(String msisdn) {
        this.msisdn = msisdn;
    }

    public String getCardNumber() {
        return cardNumber;
    }

    public void setCardNumber(String cardNumber) {
        this.cardNumber = cardNumber;
    }

    public Language getLanguage() {
        return language;
    }

    public void setLanguage(Language language) {
        this.language = language;
    }

    @Override
    public String toString() {
        return "UserAccount{" +
                "msisdn='" + msisdn + '\'' +
                ", cardNumber='" + cardNumber + '\'' +
                ", language=" + language +
                '}';
    }
}
