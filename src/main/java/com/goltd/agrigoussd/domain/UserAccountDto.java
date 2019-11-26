package com.goltd.agrigoussd.domain;

import com.goltd.agrigoussd.helpers.enums.AccountState;
import com.goltd.agrigoussd.helpers.enums.Gender;

import javax.validation.constraints.NotNull;

public class UserAccountDto {

    @NotNull
    private String msisdn;
    @NotNull
    private String fullName;
    @NotNull
    private int age;
    @NotNull
    private Gender gender;

    @NotNull
    private AccountState accountState;
    @NotNull
    private String villageCode;
    @NotNull
    private String pin;

    public UserAccountDto() {
        // Empty Constructor
    }

    public String getMsisdn() {
        return msisdn;
    }

    public void setMsisdn(String msisdn) {
        this.msisdn = msisdn;
    }

    public String getFullName() {
        return fullName;
    }

    public void setFullName(String fullName) {
        this.fullName = fullName;
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
        this.pin = pin;
    }

    @Override
    public String toString() {
        return "UserAccount{" +
                ", msisdn='" + msisdn + '\'' +
                ", fullName='" + fullName + '\'' +
                ", age=" + age +
                ", gender=" + gender +
                ", accountState=" + accountState +
                ", villageCode='" + villageCode + '\'' +
                '}';
    }
}
