package com.goltd.agrigoussd.controller;


import com.goltd.agrigoussd.domain.Session;
import com.goltd.agrigoussd.domain.UssdMenu;
import com.goltd.agrigoussd.helpers.UTKit;
import com.goltd.agrigoussd.helpers.UssdRequest;
import com.goltd.agrigoussd.helpers.enums.Freeflow;
import com.goltd.agrigoussd.helpers.enums.Question;
import com.goltd.agrigoussd.helpers.enums.Questionnaire;
import com.goltd.agrigoussd.service.interfaces.IMenuService;
import com.goltd.agrigoussd.service.interfaces.ISessionService;
import com.goltd.agrigoussd.service.interfaces.IUserService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.servlet.http.HttpServletResponse;
import java.util.Date;
import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping(value = "/ussd")
public class UssdEndpoint {

    private static final Logger logger = LoggerFactory.getLogger(UssdEndpoint.class);


    private IUserService userService;
    private ISessionService sessionService;
    private IMenuService menuService;

    @Autowired
    public UssdEndpoint(IUserService userService, ISessionService sessionService, IMenuService menuService) {
        this.userService = userService;
        this.sessionService = sessionService;
        this.menuService = menuService;
    }

    private String ussdResponse;

    @GetMapping(value = "/agrigo")
    public String getUssdResponse(UssdRequest request, HttpServletResponse httpServletResponse) {

        // Initialize session
        Session session = new Session();
        session.setId(UUID.randomUUID());
        session.setLoggedIn(false);
        session.setMsisdn(request.getMsisdn());
        session.setLastInput(request.getInput());
        session.setStartService(false);
        session.setTransactionDatetime(new Date());
        session.setLeaf(false);

        // Check if user have account
        if (userService.exists(request.getMsisdn())) {
            session.setQuestionnaire(Questionnaire.MAIN);
            session.setQuestion(Question.MAIN_ENTER_PIN);
            session.setPreviousQuestion(Question.MAIN_ENTER_PIN);
        } else {
            session.setQuestionnaire(Questionnaire.REGISTRATION);
            session.setQuestion(Question.REGISTRATION_ENTER_FULL_NAME);
            session.setPreviousQuestion(Question.REGISTRATION_ENTER_FULL_NAME);
        }

        // check if session exists on first request and reset it else initialize
        if (sessionService.exists(request.getMsisdn())) {
            httpServletResponse.setHeader(UTKit.FREE_FLOW_HEADER, Freeflow.FC.name());
            if (request.getNewRequest().equals("1")) {

                // is Expired delete and initialize else resume
                if (sessionService.getElapsedTime(request.getMsisdn()) > 5) {
                    sessionService.delete(sessionService.getByMsisdn(request.getMsisdn()));
                    sessionService.create(session);

                    // get the current session
                    Session currentSession = sessionService.getByMsisdn(request.getMsisdn());
                    UssdMenu currentMenu = menuService.getByQuestion(currentSession.getQuestion());
                    ussdResponse = currentMenu.getTitleKin();
                } else {
                    // get current session :  previous question and menu
                    Session currentSession = sessionService.getByMsisdn(request.getMsisdn());
                    Question previousQuestion = currentSession.getQuestion();
                    UssdMenu previousMenu = menuService.getByQuestion(previousQuestion);

                    // get next menu
                    List<UssdMenu> nextMenus = menuService.getByParentId(previousMenu);
                    Question nextQuestion = nextMenus.get(0).getQuestion();

                    // build next
                    UssdMenu nextMenu = nextMenus.get(0);
                    ussdResponse = nextMenu.getTitleKin();

                    // save session
                    currentSession.setPreviousQuestion(previousMenu.getQuestion());
                    currentSession.setQuestion(nextQuestion);
                    currentSession.setLastInput(UTKit.getNewBackwardInput(currentSession.getLastInput()));
                    currentSession.setTransactionDatetime(new Date());
                    Session updatedSession = sessionService.update(currentSession);
                    String saveSession = updatedSession.toString();
                    logger.info(saveSession);
                }


            } else if (request.getNewRequest().equals("0")) {

                // get current session :  previous question and menu
                Session currentSession = sessionService.getByMsisdn(request.getMsisdn());

                // Validate input base on request and currentSession
                // TODO validate user input

                // Build next menu
                Question previousQuestion = currentSession.getQuestion();
                UssdMenu previousMenu = menuService.getByQuestion(previousQuestion);
                List<UssdMenu> nextMenus = menuService.getByParentId(previousMenu);
                Question nextQuestion = nextMenus.get(0).getQuestion();
                UssdMenu nextMenu = nextMenus.get(0);

                ussdResponse = nextMenu.getTitleKin();

                // save session
                currentSession.setPreviousQuestion(previousMenu.getQuestion());
                currentSession.setQuestion(nextQuestion);
                currentSession.setLastInput(currentSession.getLastInput() + UTKit.JOINER + request.getInput());
                currentSession.setTransactionDatetime(new Date());
                currentSession.setLeaf(nextMenu.getLeaf());
                Session updatedSession = sessionService.update(currentSession);
                String saveSession = updatedSession.toString();
                logger.info(saveSession);

                if (Boolean.FALSE.equals(currentSession.getLeaf())) {
                    httpServletResponse.setHeader(UTKit.FREE_FLOW_HEADER, Freeflow.FC.name());
                } else {
                    httpServletResponse.setHeader(UTKit.FREE_FLOW_HEADER, Freeflow.FB.name());
                }
            }
        } else {
            httpServletResponse.setHeader(UTKit.FREE_FLOW_HEADER, Freeflow.FC.name());
            sessionService.create(session);
            // get the current session
            Session currentSession = sessionService.getByMsisdn(request.getMsisdn());
            UssdMenu currentMenu = menuService.getByQuestion(currentSession.getQuestion());
            ussdResponse = currentMenu.getTitleKin();
        }

        logger.info(ussdResponse);
        return ussdResponse;
    }
}
