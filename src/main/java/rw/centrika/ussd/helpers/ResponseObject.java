package rw.centrika.ussd.helpers;

import rw.centrika.ussd.helpers.enums.Question;
import rw.centrika.ussd.helpers.enums.Questionnaire;

public class ResponseObject {
    private Boolean hasError;
    private Boolean isLeaf;
    private Question selectedQuestion;
    private Question previousQuestion;
    private String displayMessage;
    private Questionnaire questionnaire;
    private String lastInput;

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

    public String getLastInput() {
        return lastInput;
    }

    public void setLastInput(String lastInput) {
        this.lastInput = lastInput;
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
                ", lastInput='" + lastInput + '\'' +
                '}';
    }
}
