package com.goltd.agrigoussd.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.goltd.agrigoussd.helpers.enums.Question;
import com.goltd.agrigoussd.helpers.enums.QuestionType;
import com.goltd.agrigoussd.helpers.enums.Questionnaire;
import com.goltd.agrigoussd.helpers.enums.Visibility;

import javax.persistence.*;
import javax.validation.constraints.NotNull;
import java.util.HashSet;
import java.util.Set;

@Entity
@Table(name = "USSD_MENU")
public class UssdMenu extends AbstractEntity {

    @Column(name = "TITLE_ENG")
    private String titleEng;

    @Column(name = "TITLE_KIN")
    private String titleKin;

    @NotNull
    @Column(name = "QUESTIONNAIRE", nullable = false, columnDefinition = "varchar(50) default 'MAIN'")
    @Enumerated(EnumType.STRING)
    private Questionnaire questionnaire;

    @NotNull
    @Column(name = "QUESTION", nullable = false, columnDefinition = "varchar(50) default 'LOGIN'")
    @Enumerated(EnumType.STRING)
    private Question question;

    @Column(name = "IS_LEAF", nullable = false, columnDefinition = "BOOLEAN default FALSE")
    private Boolean isLeaf;

    @Column(name = "QUESTION_TYPE", nullable = false, columnDefinition = "varchar(50) default 'LIST'")
    @Enumerated(EnumType.STRING)
    private QuestionType questionType;

    @Column(name = "SERVICE_START", nullable = false, columnDefinition = "BOOLEAN default FALSE")
    private Boolean serviceStart;

    @Column(name = "PRIORITY")
    private int priority;

    @Column(name = "VISIBILITY", nullable = false, columnDefinition = "varchar(50) default 'UNREGISTERED'")
    @Enumerated(EnumType.STRING)
    private Visibility visibility;

    @ManyToOne()
    @JoinColumn(name = "PARENT_ID",referencedColumnName = "ID")
    private UssdMenu parentMenu;

    @OneToMany(mappedBy = "parentMenu", fetch = FetchType.LAZY)
    @JsonIgnore
    private Set<UssdMenu> children = new HashSet<>();


    public UssdMenu() {
        // Empty constructor
    }

    public String getTitleEng() {
        return titleEng;
    }

    public void setTitleEng(String titleEng) {
        this.titleEng = titleEng;
    }

    public String getTitleKin() {
        return titleKin;
    }

    public void setTitleKin(String titleKin) {
        this.titleKin = titleKin;
    }

    public Questionnaire getQuestionnaire() {
        return questionnaire;
    }

    public void setQuestionnaire(Questionnaire questionnaire) {
        this.questionnaire = questionnaire;
    }

    public Question getQuestion() {
        return question;
    }

    public void setQuestion(Question question) {
        this.question = question;
    }

    public Boolean getLeaf() {
        return isLeaf;
    }

    public void setLeaf(Boolean leaf) {
        isLeaf = leaf;
    }

    public QuestionType getQuestionType() {
        return questionType;
    }

    public void setQuestionType(QuestionType questionType) {
        this.questionType = questionType;
    }

    public Boolean getServiceStart() {
        return serviceStart;
    }

    public void setServiceStart(Boolean serviceStart) {
        this.serviceStart = serviceStart;
    }

    public int getPriority() {
        return priority;
    }

    public void setPriority(int priority) {
        this.priority = priority;
    }

    public Visibility getVisibility() {
        return visibility;
    }

    public void setVisibility(Visibility visibility) {
        this.visibility = visibility;
    }

    public UssdMenu getParentMenu() {
        return parentMenu;
    }

    public void setParentMenu(UssdMenu parentMenu) {
        this.parentMenu = parentMenu;
    }

    public Set<UssdMenu> getChildren() {
        return children;
    }

    public void setChildren(Set<UssdMenu> children) {
        this.children = children;
    }

    @Override
    public String toString() {
        return "UssdMenu{" +
                ", titleEng='" + titleEng + '\'' +
                ", titleKin='" + titleKin + '\'' +
                ", registration=" + questionnaire +
                ", question=" + question +
                ", isLeaf=" + isLeaf +
                ", questionType=" + questionType +
                ", serviceStart=" + serviceStart +
                ", priority=" + priority +
                ", visibility=" + visibility +
                ", parentMenu=" + parentMenu +
                '}';
    }
}
