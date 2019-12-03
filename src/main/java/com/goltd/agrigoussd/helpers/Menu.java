package com.goltd.agrigoussd.helpers;

import com.goltd.agrigoussd.domain.UssdMenu;
import com.goltd.agrigoussd.helpers.enums.Question;
import com.goltd.agrigoussd.helpers.enums.QuestionType;
import com.goltd.agrigoussd.helpers.enums.Questionnaire;
import com.goltd.agrigoussd.helpers.enums.Visibility;

import java.util.UUID;

public class Menu {
    private UUID id;
    private String titleEng;
    private String titleKin;
    private Questionnaire questionnaire;
    private Question question;
    private Boolean isLeaf;
    private QuestionType questionType;
    private Boolean serviceStart;
    private int priority;
    private Visibility visibility;
    private UUID parentId;

    public Menu(UssdMenu ussdMenu) {
        this.id = ussdMenu.getId();
        this.isLeaf = ussdMenu.getLeaf();
        this.parentId = ussdMenu.getParentId().getId();
        this.question = ussdMenu.getQuestion();
        this.questionnaire = ussdMenu.getQuestionnaire();
        this.questionType = ussdMenu.getQuestionType();
        this.serviceStart = ussdMenu.getServiceStart();
        this.titleEng = ussdMenu.getTitleEng();
        this.titleKin = ussdMenu.getTitleKin();
        this.priority = ussdMenu.getPriority();
        this.visibility = ussdMenu.getVisibility();
    }

    public UUID getId() {
        return id;
    }

    public void setId(UUID id) {
        this.id = id;
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

    public UUID getParentId() {
        return parentId;
    }

    public void setParentId(UUID parentId) {
        this.parentId = parentId;
    }

    @Override
    public String toString() {
        return "Menu{" +
                "id=" + id +
                ", titleEng='" + titleEng + '\'' +
                ", titleKin='" + titleKin + '\'' +
                ", questionnaire=" + questionnaire +
                ", question=" + question +
                ", isLeaf=" + isLeaf +
                ", questionType=" + questionType +
                ", serviceStart=" + serviceStart +
                ", priority=" + priority +
                ", visibility=" + visibility +
                ", parentId=" + parentId +
                '}';
    }
}
