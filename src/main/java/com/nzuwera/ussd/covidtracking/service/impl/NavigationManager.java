package com.nzuwera.ussd.covidtracking.service.impl;

import com.nzuwera.ussd.covidtracking.domain.Session;
import com.nzuwera.ussd.covidtracking.domain.UssdMenu;
import com.nzuwera.ussd.covidtracking.helpers.ResponseObject;
import com.nzuwera.ussd.covidtracking.helpers.UTKit;
import com.nzuwera.ussd.covidtracking.helpers.UssdRequest;
import com.nzuwera.ussd.covidtracking.helpers.UssdResponse;
import com.nzuwera.ussd.covidtracking.helpers.enums.Question;
import com.nzuwera.ussd.covidtracking.helpers.enums.Visibility;
import com.nzuwera.ussd.covidtracking.service.interfaces.IMenuService;
import com.nzuwera.ussd.covidtracking.service.interfaces.INavigationManager;
import com.nzuwera.ussd.covidtracking.service.interfaces.ISessionService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import javax.servlet.http.HttpServletResponse;
import java.util.List;

@Service
public class NavigationManager implements INavigationManager {
    private static final Logger LOGGER = LoggerFactory.getLogger(NavigationManager.class);

    @Value("${application.short-code}")
    private String shortCode;

    private ISessionService sessionService;
    private IMenuService menuService;

    @Autowired
    public NavigationManager(ISessionService sessionService, IMenuService menuService) {
        this.sessionService = sessionService;
        this.menuService = menuService;
    }

    private Session session;
    private Question previousQuestion;
    private Question selectedQuestion;
    private Boolean leaf;

    @Override
    public Session backward(UssdRequest ussdRequest) {
        session = sessionService.getByMsisdn(ussdRequest.getMsisdn());
        String newInput = UTKit.getNewBackwardInput(session.getLastInput());
        previousQuestion = session.getPreviousQuestion();
        List<UssdMenu> previousMenus = menuService.getNextMenus(previousQuestion);
        UssdMenu ussdMenu = menuService.getByQuestion(previousMenus.get(0).getQuestion());
        UssdMenu parentMenu = ussdMenu.getParentMenu();
        Question parentQuestion = parentMenu.getQuestion();
        session.setLastInput(newInput);
        session.setMsisdn(ussdRequest.getMsisdn());
        session.setPreviousQuestion(parentQuestion);
        session.setQuestion(previousQuestion);
        return sessionService.update(session);
    }

    @Override
    public Session toMainMenu(UssdRequest ussdRequest, Visibility visibility) {
        session = sessionService.getByMsisdn(ussdRequest.getMsisdn());
        session.setLastInput(shortCode);
        session.setPreviousQuestion(Question.START);
        session.setQuestion(Question.START);
        session.setIsLeaf(false);
        return sessionService.update(session);
    }

    @Override
    public UssdResponse buildMenu(UssdRequest ussdRequest, Session currentSession) {
        UssdResponse response = new UssdResponse();
        session = currentSession;
        Question currentQuestion = session.getQuestion();
        UssdMenu currentMenu = menuService.getByQuestion(currentQuestion);
        ResponseObject responseObject = this.prepareDisplayMessage(ussdRequest, currentMenu);
        leaf = responseObject.getLeaf();
        String mainMenuString = "99. Aha banza";
        String displayMessage = (session.getQuestion() == Question.START || Boolean.TRUE.equals(leaf)) ? responseObject.getDisplayMessage() : responseObject.getDisplayMessage() + UTKit.EOL + UTKit.EOL + mainMenuString;

        selectedQuestion = responseObject.getSelectedQuestion();
        previousQuestion = responseObject.getPreviousQuestion();


        session.setIsLeaf(leaf);
        session.setPreviousQuestion(previousQuestion);
        session.setQuestion(selectedQuestion);
        session.setLastInput(responseObject.getLastInput());
        if (Boolean.FALSE.equals(responseObject.getHasError())) {
            sessionService.update(session);
        }
        if (Boolean.TRUE.equals(leaf)) {
            sessionService.delete(session);
        }

        response.setFreeflow(leaf);
        response.setMessage(displayMessage);
        return response;
    }

    @Override
    public ResponseObject prepareDisplayMessage(UssdRequest request, UssdMenu currentMenu) {
        leaf = false;
        LOGGER.info("currentMenu {}", currentMenu);
        List<UssdMenu> nexMenus = menuService.getNextMenus(currentMenu);
        StringBuilder stringBuilder = new StringBuilder();
        Boolean hasError = false;
        String lastInput = (request.getNewrequest().equals("1") ? request.getInput() : session.getLastInput() + UTKit.JOINER + request.getInput());

        previousQuestion = nexMenus.get(0).getParentMenu().getQuestion();
        selectedQuestion = currentMenu.getQuestion();
        if (selectedQuestion == Question.START) {
            previousQuestion = selectedQuestion;
            selectedQuestion = nexMenus.get(0).getQuestion();
            leaf = currentMenu.getIsLeaf();
            stringBuilder.append("Murakaza neza kuri COVID-19.");
            stringBuilder.append(UTKit.EOL);
            stringBuilder.append(nexMenus.get(0).getTitleKin());
        } else {
            LOGGER.info("=================== access last else ===================");
            currentMenu = menuService.getByQuestion(selectedQuestion);
            nexMenus = menuService.getNextMenus(currentMenu);
            stringBuilder.append(nexMenus.get(0).getTitleKin());
            leaf = nexMenus.get(0).getIsLeaf();
            previousQuestion = selectedQuestion;
            selectedQuestion = nexMenus.get(0).getQuestion();
        }
        ResponseObject responseObject = new ResponseObject();
        responseObject.setDisplayMessage(stringBuilder.toString());
        responseObject.setHasError(hasError);
        responseObject.setLeaf(leaf);
        responseObject.setLastInput(lastInput);
        responseObject.setPreviousQuestion(previousQuestion);
        responseObject.setSelectedQuestion(selectedQuestion);
        LOGGER.info("responseObject {}", responseObject);
        return responseObject;
    }

    @Override
    public String sendUssdResponse(UssdResponse ussdResponse, HttpServletResponse httpServletResponse) {
        httpServletResponse.setHeader(UTKit.FREE_FLOW_HEADER, ussdResponse.getFreeflow().name());
        return ussdResponse.getMessage();
    }
}
