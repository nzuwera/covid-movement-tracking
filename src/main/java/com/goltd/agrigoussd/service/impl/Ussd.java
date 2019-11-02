package com.goltd.agrigoussd.service.impl;

import com.goltd.agrigoussd.domain.Session;
import com.goltd.agrigoussd.helpers.UssdRequest;
import com.goltd.agrigoussd.helpers.enums.Question;
import com.goltd.agrigoussd.helpers.enums.Questionnaire;
import com.goltd.agrigoussd.service.interfaces.IUssd;
import com.goltd.agrigoussd.service.interfaces.ISessionService;
import com.goltd.agrigoussd.service.interfaces.IUserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.UUID;

@Service
public class Ussd implements IUssd {
    private ISessionService sessionService;
    private IUserService userService;

    @Autowired
    public Ussd(ISessionService sessionService, IUserService userService) {
        this.sessionService = sessionService;
        this.userService = userService;
    }

    @Override
    public void initialize(UssdRequest request) {


        Session session = new Session();

        session.setId(UUID.randomUUID());
        session.setLoggedIn(false);
        session.setMsisdn(request.getMsisdn());
        session.setLastInput(request.getInput());
        session.setStartService(false);
        session.setTransactionDatetime(new Date());

        if (userService.exists(request.getMsisdn())) {
            session.setQuestionnaire(Questionnaire.REGISTRATION);
            session.setQuestion(Question.REGISTRATION_ENTER_FULL_NAME);
            session.setPreviousQuestion(Question.REGISTRATION_ENTER_FULL_NAME);
        } else {
            session.setQuestionnaire(Questionnaire.MAIN);
            session.setQuestion(Question.MAIN_ENTER_PIN);
            session.setPreviousQuestion(Question.MAIN_ENTER_PIN);
        }

        sessionService.create(session);

    }
}
