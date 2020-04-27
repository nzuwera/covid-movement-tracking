package com.nzuwera.ussd.covidtracking.domain;

import com.nzuwera.ussd.covidtracking.helpers.enums.TransactionType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;

import javax.persistence.*;
import javax.validation.constraints.NotNull;
import java.util.Date;


@Table(name = "TRANSACTION", uniqueConstraints = {
        @UniqueConstraint(columnNames = {"DRIVER_PHONE", "CUSTOMER_PHONE", "SESSION_ID"}, name = "CONSTRAINT_TRANSACTION_DRIVER_PHONE_CUSTOMER_PHONE_SESSION_ID")
})
@Entity
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Transaction extends AbstractEntity {

    @Column(name = "DRIVER_PHONE", nullable = false, columnDefinition = "varchar(12) not null")
    @NotNull
    private String driverPhone;
    @Column(name = "CUSTOMER_PHONE", columnDefinition = "varchar(12) not null")
    private String customerPhone;
    @Column(name = "TRANSACTION_TYPE", nullable = false, columnDefinition = "varchar(10) default 'MOVEMENT'")
    @Enumerated(EnumType.STRING)
    private TransactionType language;
    @Column(name = "AMOUNT")
    private int amount;
    @Column(name = "DEPARTURE", nullable = false, columnDefinition = "VARCHAR(200) NOT NULL")
    private String departure;
    @Column(name = "DESTINATION", nullable = false, columnDefinition = "VARCHAR(200) NOT NULL")
    private String destination;
    @Column(name = "MOMO_TRX_REFERENCE", nullable = false, columnDefinition = "VARCHAR(200) NOT NULL")
    private String momoTrxReference;
    @Column(name = "APP_TRX_REFERENCE", nullable = false, columnDefinition = "VARCHAR(200) NOT NULL")
    private String appTrxReference;
    @Column(name = "SESSION_ID", nullable = false, columnDefinition = "varchar(255)")
    private String sessionId;
    @Column(name = "TRANSACTION_DATETIME", nullable = false)
    @DateTimeFormat(pattern = "yyyy-MM-dd hh:mm:ss")
    private Date transactionDatetime;

}
