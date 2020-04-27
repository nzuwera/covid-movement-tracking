package com.nzuwera.ussd.covidtracking.controller;


import com.nzuwera.ussd.covidtracking.domain.Session;
import com.nzuwera.ussd.covidtracking.helpers.UTKit;
import com.nzuwera.ussd.covidtracking.helpers.UssdRequest;
import com.nzuwera.ussd.covidtracking.helpers.UssdResponse;
import com.nzuwera.ussd.covidtracking.helpers.enums.Freeflow;
import com.nzuwera.ussd.covidtracking.helpers.enums.Question;
import com.nzuwera.ussd.covidtracking.service.interfaces.INavigationManager;
import com.nzuwera.ussd.covidtracking.service.interfaces.ISessionService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.servlet.http.HttpServletResponse;
import java.util.Date;

@RestController
@RequestMapping(value = "/ussd")
public class UssdEndpoint {

    private static final Logger LOGGER = LoggerFactory.getLogger(UssdEndpoint.class);
    @Value("${application.short-code}")
    private String shortCode;
    private ISessionService sessionService;
    private INavigationManager navigationManager;

    @Autowired
    public UssdEndpoint(ISessionService sessionService, INavigationManager navigationManager) {
        this.sessionService = sessionService;
        this.navigationManager = navigationManager;
    }

    /**
     * USSD handler
     * @param request ussd request normally get query parameters
     * @param httpResponse http response with headers containing Freeflow
     * @return USSD response
     */
    @GetMapping(value = "/covidTracking")
    public String ussdHandler(UssdRequest request, HttpServletResponse httpResponse) {

        UssdResponse ussdResponse;
        String ussdMessage;
        Session session;
        Session currentSession;

        /*
         * Check if user has dialed *909# or is continuing an open session
         */
        if (request.getNewrequest().equals("1")) {
            /*
             * Set User default language to KIN
             */
            session = new Session();
            session.setMsisdn(request.getMsisdn());
            session.setLastInput(request.getInput());
            session.setIsLeaf(false);
            session.setTransactionDatetime(new Date());
            session.setPreviousQuestion(Question.START);
            session.setQuestion(Question.START);
            session.setLanguage("KIN");
            session.setSessionId(request.getSessionID());
            /*
             * Check if a ussd session already exists
             */
            if (Boolean.TRUE.equals(sessionService.exists(request.getMsisdn()))) {
                currentSession = sessionService.getByMsisdn(request.getMsisdn());
                /*
                 * USSD Session resume:
                 *  - Must Not be the last menu
                 *  - Must not have expired
                 */
                if (currentSession.getIsLeaf().equals(true) || Boolean.TRUE.equals(UTKit.isExpired(currentSession.getTransactionDatetime()))) {
                    sessionService.delete(currentSession);
                    sessionService.create(session);
                } else {
                    session = currentSession;
                    request.setInput(UTKit.getLastInput(session.getLastInput()));
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
                ussdResponse = navigationManager.buildMenu(request, session);
                ussdMessage = navigationManager.sendUssdResponse(ussdResponse, httpResponse);
            }
        } else if (request.getNewrequest().equals("0")) {
            /*
             * Continue USSD Navigation
             */
            session = sessionService.getByMsisdn(request.getMsisdn());

            if (request.getInput().equals("0") && !session.getQuestion().equals(Question.START)) {
                /*
                 * USSD Backward navigation:
                 * - 0 Go Back
                 */
                session = navigationManager.backward(request);
                ussdResponse = navigationManager.buildMenu(request, session);
            } else if (request.getInput().equals("99") && !session.getQuestion().equals(Question.START)) {
                /*
                 * USSD Backward navigation:
                 * - 99 Go to main menu
                 */
                session.setPreviousQuestion(Question.START);
                session.setQuestion(Question.START);
                session.setLastInput(UTKit.EMPTY);
                sessionService.update(session);
                request.setNewrequest("1");
                request.setInput(shortCode);
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
