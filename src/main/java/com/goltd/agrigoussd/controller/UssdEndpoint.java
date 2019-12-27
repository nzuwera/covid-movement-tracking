package com.goltd.agrigoussd.controller;


import com.goltd.agrigoussd.domain.Session;
import com.goltd.agrigoussd.helpers.UTKit;
import com.goltd.agrigoussd.helpers.UssdRequest;
import com.goltd.agrigoussd.helpers.UssdResponse;
import com.goltd.agrigoussd.helpers.enums.Freeflow;
import com.goltd.agrigoussd.helpers.enums.Question;
import com.goltd.agrigoussd.helpers.enums.Questionnaire;
import com.goltd.agrigoussd.helpers.enums.Visibility;
import com.goltd.agrigoussd.service.interfaces.INavigationManager;
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
import java.util.UUID;

@RestController
@RequestMapping(value = "/ussd")
public class UssdEndpoint {

    private static final Logger LOGGER = LoggerFactory.getLogger(UssdEndpoint.class);

    private IUserService userService;
    private ISessionService sessionService;
    private INavigationManager navigationManager;

    @Autowired
    public UssdEndpoint(IUserService userService, ISessionService sessionService, INavigationManager navigationManager) {
        this.userService = userService;
        this.sessionService = sessionService;
        this.navigationManager = navigationManager;
    }

    @GetMapping(value = "/agrigo")
    public String ussdHandler(UssdRequest request, HttpServletResponse httpResponse) {

        UssdResponse ussdResponse;
        String ussdMessage;
        Session session;
        Session currentSession;

        /*
         * Check if user has dialed *909# or is continuing an open session
         */
        if (request.getNewRequest().equals("1") && request.getInput().startsWith("*") && request.getInput().endsWith("#")) {
            /*
             * Set User default language to KIN
             */
            session = new Session();
            session.setId(UUID.randomUUID());
            session.setMsisdn(request.getMsisdn());
            session.setLastInput(request.getInput());
            session.setLeaf(false);
            session.setLoggedIn(false);
            session.setTransactionDatetime(new Date());
            if (Boolean.FALSE.equals(userService.exists(request.getMsisdn()))) {
                // Start User Registration
                //
                session.setQuestionnaire(Questionnaire.REGISTRATION);
                session.setPreviousQuestion(Question.REGISTRATION_START);
                session.setQuestion(Question.REGISTRATION_START);
                session.setStartService(true);

            } else {
                session.setQuestionnaire(Questionnaire.MAIN);
                // session.setPreviousQuestion(Question.MAIN_ENTER_PIN);
                session.setQuestion(Question.MAIN_ENTER_PIN);
                session.setStartService(false);
            }
            /*
             * Check if a ussd session already exists
             */
            if (Boolean.TRUE.equals(sessionService.exists(request.getMsisdn()))) {
                session = sessionService.getByMsisdn(request.getMsisdn());
                /*
                 * USSD Session resume:
                 *  - Must Not be the last menu
                 *  - Must not have expired
                 */
                if (session.getLeaf().equals(true) || Boolean.TRUE.equals(UTKit.isExpired(session.getTransactionDatetime()))) {
                    sessionService.delete(session);
                    sessionService.create(session);
                }
                /*
                 * Build next USSD menu
                 */
                ussdResponse = navigationManager.buildMenu(request, session);
                ussdMessage = navigationManager.sendUssdResponse(ussdResponse, httpResponse);
            } else {
                /*
                 * Initialize USSD session
                 */
                // sessionService.create(session);
                ussdResponse = navigationManager.buildMenu(request, session);
                ussdMessage = navigationManager.sendUssdResponse(ussdResponse, httpResponse);
            }
        } else if (request.getNewRequest().equals("0")) {
            /*
             * Continue USSD Navigation
             */
            session = sessionService.getByMsisdn(request.getMsisdn());

            if (request.getInput().equals("0") && !session.getQuestion().equals(Question.MAIN_LOGIN) && !session.getQuestion().equals(Question.REGISTRATION_START)) {
                /*
                 * USSD Backward navigation:
                 * - 0 Go Back
                 */
                session = navigationManager.backward(request);
                ussdResponse = navigationManager.buildMenu(request, session);
            } else if (request.getInput().equals("99") && !session.getQuestion().equals(Question.MAIN_LOGIN) && !session.getQuestion().equals(Question.REGISTRATION_START)) {
                /*
                 * USSD Backward navigation:
                 * - 99 Go to main menu
                 */
                Visibility visibility = (session.getQuestionnaire().equals(Questionnaire.REGISTRATION) ? Visibility.UNREGISTERED : Visibility.REGISTERED);
                session = navigationManager.toMainMenu(request, visibility);
                ussdResponse = navigationManager.buildMenu(request, session);
            } else {
                /*
                 * USSD Forward navigation:
                 *
                 * Change User's prefered language.
                 */
                ussdResponse = navigationManager.buildMenu(request, session);
            }
            ussdMessage = navigationManager.sendUssdResponse(ussdResponse, httpResponse);

        } else {
            ussdMessage = navigationManager.sendUssdResponse(new UssdResponse("General Error: " + request.toString(), Freeflow.FB), httpResponse);
        }
        /*
         * Display USSD Message
         */
        LOGGER.info("ussdResponse \n{}", ussdMessage);
        return ussdMessage;
    }
}
