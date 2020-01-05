package com.goltd.agrigoussd.helpers;

import com.goltd.agrigoussd.helpers.enums.Question;
import com.goltd.agrigoussd.helpers.enums.Questionnaire;

public class ResponseObject {
    private Boolean hasError;
    private Boolean isLeaf;
    private Question selectedQuestion;
    private Question previousQuestion;
    private String displayMessage;
    private Questionnaire questionnaire;

    public ResponseObject() {
        //
    }

    public Boolean getHasError() {
        return hasError;
    }

    public void setHasError(Boolean hasError) {
        this.hasError = hasError;
    }

    public Boolean getLeaf() {
        return isLeaf;
    }

    public void setLeaf(Boolean leaf) {
        isLeaf = leaf;
    }

    public Question getSelectedQuestion() {
        return selectedQuestion;
    }

    public void setSelectedQuestion(Question selectedQuestion) {
        this.selectedQuestion = selectedQuestion;
    }

    public Question getPreviousQuestion() {
        return previousQuestion;
    }

    public void setPreviousQuestion(Question previousQuestion) {
        this.previousQuestion = previousQuestion;
    }

    public String getDisplayMessage() {
        return displayMessage;
    }

    public void setDisplayMessage(String displayMessage) {
        this.displayMessage = displayMessage;
    }

    public Questionnaire getQuestionnaire() {
        return questionnaire;
    }

    public void setQuestionnaire(Questionnaire questionnaire) {
        this.questionnaire = questionnaire;
    }

    @Override
    public String toString() {
        return "ResponseObject{" +
                "hasError=" + hasError +
                ", isLeaf=" + isLeaf +
                ", selectedQuestion=" + selectedQuestion +
                ", previousQuestion=" + previousQuestion +
                ", displayMessage='" + displayMessage + '\'' +
                ", questionnaire=" + questionnaire +
                '}';
    }
}
