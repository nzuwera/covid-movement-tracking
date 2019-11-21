package com.goltd.agrigoussd.questionnaire;

import com.goltd.agrigoussd.domain.Session;
import com.goltd.agrigoussd.helpers.UssdRequest;
import com.goltd.agrigoussd.questionnaire.processors.AssociationQuestionnaire;
import com.goltd.agrigoussd.questionnaire.processors.MainQuestionnaire;
import com.goltd.agrigoussd.questionnaire.processors.RegistrationQuestionnaire;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class QuestionnaireProcessor {

    private RegistrationQuestionnaire registration;
    private MainQuestionnaire main;
    private AssociationQuestionnaire association;

    @Autowired
    public QuestionnaireProcessor(RegistrationQuestionnaire registration, MainQuestionnaire main, AssociationQuestionnaire association) {
        this.registration = registration;
        this.main = main;
        this.association = association;
    }

    public String handleQuestionnaire(Session session, UssdRequest request) {
        String message = "";
        StringBuilder ussdMenus;
        switch (session.getQuestionnaire()) {
            case REGISTRATION:
                ussdMenus = registration.buildMenu(session, request);
                message = ussdMenus.toString();
                break;
            case MAIN:
                ussdMenus = main.buildMenu(session, request);
                message = ussdMenus.toString();
                break;
            case ASSOCIATION:
                ussdMenus = association.buildMenu(session, request);
                message = ussdMenus.toString();
                break;
            default:
                message = session.getQuestionnaire().toString();
                break;
        }
        return message;
    }
}
