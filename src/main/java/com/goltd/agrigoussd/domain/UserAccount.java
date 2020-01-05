package com.goltd.agrigoussd.domain;

import com.goltd.agrigoussd.helpers.UTKit;
import com.goltd.agrigoussd.helpers.enums.AccountState;
import com.goltd.agrigoussd.helpers.enums.Gender;
import org.springframework.format.annotation.DateTimeFormat;

import javax.persistence.*;
import javax.validation.constraints.NotNull;
import java.util.Date;


@Table(name = "USER_ACCOUNT", uniqueConstraints = {
        @UniqueConstraint(columnNames = "MSISDN", name = "CONSTRAINT_USER_ACCOUNT_MSISDN")
})
@Entity
public class UserAccount extends AbstractEntity {

    @Column(name = "MSISDN")
    @NotNull
    private String msisdn;

    @Column(name = "FULL_NAME")
    private String fullname;

    @Column(name = "AGE")
    private int age;

    @Column(name = "GENDER", nullable = false, columnDefinition = "varchar(10) default 'MALE'")
    @Enumerated(EnumType.STRING)
    private Gender gender;

    @Column(name = "ACCCOUNT_STATE", nullable = false, columnDefinition = "varchar(50) default 'PENDING_SUBSCRIPTION'")
    @Enumerated(EnumType.STRING)
    private AccountState accountState;

    @Column(name = "EXPIRE_DATE", nullable = false)
    @DateTimeFormat(pattern = "yyyy-MM-dd")
    private Date expireDate;

    @Column(name = "VILLAGE_CODE", nullable = false, columnDefinition = "varchar(10) not null")
    private String villageCode;

    @Column(name = "PIN")
    private String pin;

    @Column(name = "IN_ASSOCIATION", columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean inAssociation;

    @Column(name = "HAS_LAND", columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean hasLand;

    public UserAccount() {
        // Empty Constructor
    }

    public String getMsisdn() {
        return msisdn;
    }

    public void setMsisdn(String msisdn) {
        this.msisdn = msisdn;
    }

    public String getFullname() {
        return fullname;
    }

    public void setFullname(String fullname) {
        this.fullname = fullname;
    }

    public int getAge() {
        return age;
    }

    public void setAge(int age) {
        this.age = age;
    }

    public Gender getGender() {
        return gender;
    }

    public void setGender(Gender gender) {
        this.gender = gender;
    }

    public AccountState getAccountState() {
        return accountState;
    }

    public void setAccountState(AccountState accountState) {
        this.accountState = accountState;
    }

    public Date getExpireDate() {
        return expireDate;
    }

    public void setExpireDate(Date expireDate) {
        this.expireDate = expireDate;
    }

    public String getVillageCode() {
        return villageCode;
    }

    public void setVillageCode(String villageCode) {
        this.villageCode = villageCode;
    }

    public String getPin() {
        return pin;
    }

    public void setPin(String pin) {
        this.pin = UTKit.securePassword(pin);
    }

    public Boolean getInAssociation() {
        return inAssociation;
    }

    public void setInAssociation(Boolean inAssociation) {
        this.inAssociation = inAssociation;
    }

    public Boolean getHasLand() {
        return hasLand;
    }

    public void setHasLand(Boolean hasLand) {
        this.hasLand = hasLand;
    }

    @Override
    public String toString() {
        return "UserAccount{" +
                "msisdn='" + msisdn + '\'' +
                ", fullname='" + fullname + '\'' +
                ", age=" + age +
                ", gender=" + gender +
                ", accountState=" + accountState +
                ", expireDate=" + expireDate +
                ", villageCode='" + villageCode + '\'' +
                ", pin='" + pin + '\'' +
                ", inAssociation=" + inAssociation +
                ", hasLand=" + hasLand +
                '}';
    }
}
