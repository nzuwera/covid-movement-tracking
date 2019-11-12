package com.goltd.agrigoussd.domain;

import com.goltd.agrigoussd.helpers.enums.Question;
import com.goltd.agrigoussd.helpers.enums.Questionnaire;
import org.hibernate.annotations.Type;
import org.springframework.format.annotation.DateTimeFormat;

import javax.persistence.*;
import javax.validation.constraints.NotNull;
import java.util.Date;
import java.util.UUID;

@Entity
@Table(name = "SESSION", uniqueConstraints = {
        @UniqueConstraint(columnNames = "MSISDN", name = "CONSTRAINT_SESSION_MSISDN")
})
public class Session {


    @Id
    @Type(type = "pg-uuid")
    @NotNull
    @Column(name = "ID")
    private UUID id;

    @Column(name = "MSISDN")
    private String msisdn;

    @Enumerated(EnumType.STRING)
    @Column(name = "PREVIOUS_QUESTION", nullable = false, columnDefinition = "varchar(255) default 'MAIN'")
    private Question previousQuestion;

    @Enumerated(EnumType.STRING)
    @Column(name = "QUESTIONNAIRE", nullable = false, columnDefinition = "varchar(255) default 'MAIN'")
    private Questionnaire questionnaire;

    @Column(name = "TRANSACTION_DATETIME", nullable = false)
    @DateTimeFormat(pattern = "yyyy-MM-dd hh:mm:ss")
    private Date transactionDatetime;

    @Column(name = "LAST_INPUT")
    private String lastInput;

    @Column(name = "START_SERVICE", nullable = false, columnDefinition = "BOOLEAN default FALSE")
    private Boolean startService;

    @Column(name = "QUESTION", nullable = false, columnDefinition = "varchar(255) default 'MAIN'")
    @Enumerated(EnumType.STRING)
    private Question question;

    @Column(name = "LOGGED_IN", nullable = false, columnDefinition = "BOOLEAN default FALSE")
    private Boolean loggedIn;

    @Column(name = "IS_LEAF", nullable = false, columnDefinition = "BOOLEAN default FALSE")
    private Boolean isLeaf;

    public Session() {
        // Default Constructor
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

    public Question getPreviousQuestion() {
        return previousQuestion;
    }

    public void setPreviousQuestion(Question previousQuestion) {
        this.previousQuestion = previousQuestion;
    }

    public Questionnaire getQuestionnaire() {
        return questionnaire;
    }

    public void setQuestionnaire(Questionnaire questionnaire) {
        this.questionnaire = questionnaire;
    }

    public Date getTransactionDatetime() {
        return transactionDatetime;
    }

    public void setTransactionDatetime(Date transactionDatetime) {
        this.transactionDatetime = transactionDatetime;
    }

    public String getLastInput() {
        return lastInput;
    }

    public void setLastInput(String lastInput) {
        this.lastInput = lastInput;
    }

    public Boolean getStartService() {
        return startService;
    }

    public void setStartService(Boolean startService) {
        this.startService = startService;
    }

    public Question getQuestion() {
        return question;
    }

    public void setQuestion(Question question) {
        this.question = question;
    }

    public Boolean getLoggedIn() {
        return loggedIn;
    }

    public void setLoggedIn(Boolean loggedIn) {
        this.loggedIn = loggedIn;
    }


    public Boolean getLeaf() {
        return isLeaf;
    }

    public void setLeaf(Boolean leaf) {
        isLeaf = leaf;
    }

    @Override
    public String toString() {
        return "Session{" +
                "id=" + id +
                ", msisdn='" + msisdn + '\'' +
                ", previousQuestion=" + previousQuestion +
                ", questionnaire=" + questionnaire +
                ", transactionDatetime=" + transactionDatetime +
                ", lastInput='" + lastInput + '\'' +
                ", startService=" + startService +
                ", question=" + question +
                ", loggedIn=" + loggedIn +
                ", isLeaf=" + isLeaf +
                '}';
    }

}
