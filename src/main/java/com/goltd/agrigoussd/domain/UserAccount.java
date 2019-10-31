package com.goltd.agrigoussd.domain;

import com.goltd.agrigoussd.audit.Auditable;
import org.hibernate.annotations.Type;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import javax.persistence.*;
import javax.validation.constraints.NotNull;
import java.util.UUID;


@Table(name = "USER_ACCOUNT", uniqueConstraints = {
        @UniqueConstraint(columnNames = "MSISDN", name = "CONSTRAINT_USER_ACCOUNT_MSISDN")
})
@Entity
@EntityListeners(AuditingEntityListener.class)
public class UserAccount extends Auditable<String> {

    @Id
    @Type(type = "pg-uuid")
    @NotNull
    @Column(name = "ID")
    private UUID id;

    @Column(name = "MSISDN")
    @NotNull
    private String msisdn;

    @Column(name = "REGISTERED")
    private Boolean registered;

    @Column(name = "FULL_NAME")
    private String fullname;

    @Column(name = "PIN")
    private String pin;

    public UserAccount() {
        // Empty Constructor
    }

    public UUID getId() {
        return id;
    }

    public void setId(UUID id) {
        this.id = id;
    }

    public String getMsisdn() {
        return msisdn;
    }

    public void setMsisdn(String msisdn) {
        this.msisdn = msisdn;
    }

    public Boolean getRegistered() {
        return registered;
    }

    public void setRegistered(Boolean registered) {
        this.registered = registered;
    }

    public String getFullname() {
        return fullname;
    }

    public void setFullname(String fullname) {
        this.fullname = fullname;
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
                "id=" + id +
                ", msisdn='" + msisdn + '\'' +
                ", registered=" + registered +
                ", fullname='" + fullname + '\'' +
                ", pin='" + pin + '\'' +
                '}';
    }
}
