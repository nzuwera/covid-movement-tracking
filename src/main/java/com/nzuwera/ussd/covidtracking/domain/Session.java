package com.nzuwera.ussd.covidtracking.domain;

import com.nzuwera.ussd.covidtracking.helpers.enums.Question;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;

import javax.persistence.*;
import java.util.Date;

@Entity
@Table(name = "SESSION", uniqueConstraints = {
        @UniqueConstraint(columnNames = "MSISDN", name = "CONSTRAINT_SESSION_MSISDN")
})
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Session extends AbstractEntity {

    @Column(name = "MSISDN")
    private String msisdn;
    @Column(name = "TRANSACTION_DATETIME", nullable = false)
    @DateTimeFormat(pattern = "yyyy-MM-dd hh:mm:ss")
    private Date transactionDatetime;
    @Column(name = "LAST_INPUT")
    private String lastInput;
    @Column(name = "QUESTION", nullable = false, columnDefinition = "varchar(255) default 'START'")
    @Enumerated(EnumType.STRING)
    private Question question;
    @Column(name = "PREVIOUS_QUESTION", columnDefinition = "varchar(255) default 'START'")
    @Enumerated(EnumType.STRING)
    private Question previousQuestion;
    @Column(name = "IS_LEAF", columnDefinition = "BOOLEAN default FALSE")
    private Boolean isLeaf;
    @Column(name = "LANGUAGE", nullable = false, columnDefinition = "varchar(3) default 'KIN'")
    private String language;
    @Column(name = "SESSION_ID", nullable = false, columnDefinition = "varchar(255)")
    private String sessionId;

}
