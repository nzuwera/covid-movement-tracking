package com.goltd.agrigoussd.questionnaire;

import com.goltd.agrigoussd.domain.Session;
import com.goltd.agrigoussd.helpers.UssdRequest;
import com.goltd.agrigoussd.questionnaire.processors.MainQuestionnaire;
import com.goltd.agrigoussd.questionnaire.processors.RegistrationQuestionnaire;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class QuestionnaireProcessor {

    private RegistrationQuestionnaire registrationQuestionnaire;
    private MainQuestionnaire mainQuestionnaire;

    @Autowired
    public QuestionnaireProcessor(RegistrationQuestionnaire registrationQuestionnaire, MainQuestionnaire mainQuestionnaire) {
        this.registrationQuestionnaire = registrationQuestionnaire;
        this.mainQuestionnaire = mainQuestionnaire;
    }
    public String handleQuestionnaire(Session session, UssdRequest request) {
        String message = "";
        StringBuilder ussdMenus;
        switch (session.getQuestionnaire()) {
            case REGISTRATION:
                ussdMenus = registrationQuestionnaire.buildMenu(session, request);
                message = ussdMenus.toString();
                break;
            case MAIN:
                ussdMenus = mainQuestionnaire.buildMenu(session, request);
                message = ussdMenus.toString();
                break;
            default:
                message = session.getQuestionnaire().toString();
                break;
        }
        return message;
    }
}
