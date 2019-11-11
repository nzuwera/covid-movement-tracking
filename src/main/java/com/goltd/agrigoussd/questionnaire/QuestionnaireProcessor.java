package com.goltd.agrigoussd.questionnaire;

import com.goltd.agrigoussd.domain.Session;
import com.goltd.agrigoussd.helpers.UssdRequest;
import com.goltd.agrigoussd.questionnaire.processors.RegistrationQuestionnaire;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class QuestionnaireProcessor {

    private RegistrationQuestionnaire registrationQuestionnaire;

    @Autowired
    public QuestionnaireProcessor(RegistrationQuestionnaire registrationQuestionnaire) {
        this.registrationQuestionnaire = registrationQuestionnaire;
    }

    public String handleQuestionnaire(Session session, UssdRequest request) {
        String message = "";
        switch (session.getQuestionnaire()) {
            case REGISTRATION:
                StringBuilder ussdMenu = registrationQuestionnaire.buildMenu(session, request);
                message = ussdMenu.toString();
                break;
            case MAIN:
                break;
            default:
                message = session.getQuestionnaire().toString();
                break;
        }
        return message;
    }
}
