package com.goltd.agrigoussd.service.impl;

import com.goltd.agrigoussd.domain.Session;
import com.goltd.agrigoussd.domain.UssdMenu;
import com.goltd.agrigoussd.helpers.ResponseObject;
import com.goltd.agrigoussd.helpers.UTKit;
import com.goltd.agrigoussd.helpers.UssdRequest;
import com.goltd.agrigoussd.helpers.UssdResponse;
import com.goltd.agrigoussd.helpers.enums.Question;
import com.goltd.agrigoussd.helpers.enums.Questionnaire;
import com.goltd.agrigoussd.helpers.enums.Visibility;
import com.goltd.agrigoussd.service.interfaces.*;
import com.goltd.agrigoussd.validators.QuestionValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.servlet.http.HttpServletResponse;
import java.util.List;

@Service
public class NavigationManager implements INavigationManager {
    private static final Logger LOGGER = LoggerFactory.getLogger(NavigationManager.class);
    private static final String SHORT_CODE = "*123#";

    private ISessionService sessionService;
    private IMenuService menuService;
    private ILocationService locationService;
    private IUserService userService;
    private AssociationService associationService;

    @Autowired
    public NavigationManager(ISessionService sessionService, IMenuService menuService, ILocationService locationService, IUserService userService, AssociationService associationService) {
        this.sessionService = sessionService;
        this.menuService = menuService;
        this.locationService = locationService;
        this.userService = userService;
        this.associationService = associationService;
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
        if (previousQuestion.equals(Question.MAIN_LOGIN)) {
            session = this.toMainMenu(ussdRequest, Visibility.REGISTERED);
        } else if (previousQuestion.equals(Question.REGISTRATION_START)) {
            session = this.toMainMenu(ussdRequest, Visibility.UNREGISTERED);
        } else {
            List<UssdMenu> previousMenus = menuService.getNextMenus(previousQuestion);
            UssdMenu ussdMenu = menuService.getByQuestion(previousMenus.get(0).getQuestion());
            UssdMenu parentMenu = ussdMenu.getParentMenu();
            Question parentQuestion = parentMenu.getQuestion();
            session.setLastInput(newInput);
            session.setMsisdn(ussdRequest.getMsisdn());
            session.setPreviousQuestion(parentQuestion);
            session.setQuestion(previousQuestion);
        }
        return sessionService.update(session);
    }

    @Override
    public Session toMainMenu(UssdRequest ussdRequest, Visibility visibility) {
        session = sessionService.getByMsisdn(ussdRequest.getMsisdn());
        session.setLastInput(SHORT_CODE);
        if (visibility.equals(Visibility.REGISTERED)) {
            session.setPreviousQuestion(Question.MAIN_LOGIN);
            session.setQuestion(Question.MAIN_LOGIN);
            session.setQuestionnaire(Questionnaire.MAIN);
        } else {
            session.setPreviousQuestion(Question.REGISTRATION_START);
            session.setQuestion(Question.REGISTRATION_START);
            session.setQuestionnaire(Questionnaire.REGISTRATION);
        }
        session.setStartService(false);
        session.setLoggedIn(true);
        session.setLeaf(false);
        return sessionService.update(session);
    }

    @Override
    public UssdResponse buildMenu(UssdRequest ussdRequest, Session session) {
        UssdResponse response = new UssdResponse();
        Question currentQuestion = session.getQuestion();
        UssdMenu currentMenu = menuService.getByQuestion(currentQuestion);
        List<UssdMenu> nextMenus = menuService.getNextMenus(currentMenu);
        ResponseObject responseObject = this.prepareDisplayMessage(ussdRequest, nextMenus);
        String displayMessage = responseObject.getDisplayMessage();
        leaf = responseObject.getLeaf();
        selectedQuestion = responseObject.getSelectedQuestion();
        previousQuestion = responseObject.getPreviousQuestion();
        Questionnaire questionnaire = responseObject.getQuestionnaire();


        String lastInput = (session.getLastInput().equals(SHORT_CODE) && ussdRequest.getNewRequest().equals("1") ? session.getLastInput() : session.getLastInput() + UTKit.JOINER + ussdRequest.getInput());
        session.setLeaf(leaf);
        session.setPreviousQuestion(previousQuestion);
        session.setQuestion(selectedQuestion);
        session.setLastInput(lastInput);
        session.setQuestionnaire(questionnaire);
        if (Boolean.FALSE.equals(responseObject.getHasError())) {
            sessionService.update(session);
        }
        response.setFreeflow(leaf);
        response.setMessage(displayMessage);
        return response;
    }

    @Override
    public ResponseObject prepareDisplayMessage(UssdRequest request, List<UssdMenu> menus) {
        LOGGER.info("menus {}", menus);
        UssdMenu currentMenu;
        List<UssdMenu> siblings;
        List<UssdMenu> nexMenus;
        StringBuilder stringBuilder = new StringBuilder();
        Boolean hasError = false;
        leaf = menus.get(0).getLeaf();
        Questionnaire questionnaire = menus.get(0).getQuestionnaire();
        previousQuestion = menus.get(0).getParentMenu().getQuestion();
        selectedQuestion = menus.get(0).getQuestion();
        if (selectedQuestion == Question.MAIN_SELECT_SERVICE) {
            // validate pin
            if (userService.isValidPin(request.getMsisdn(), request.getInput())) {
                List<UssdMenu> children = menuService.getNextMenus(selectedQuestion);
                stringBuilder.append(previousQuestion);
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(selectedQuestion);
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(request.getInput());
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(UTKit.listMenus(children));
            } else {
                hasError = true;
                stringBuilder.append("Invalid PIN");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(menus.get(0).getParentMenu().getTitleKin());
            }

        } else if (selectedQuestion == Question.MAIN_MENU_ASSOCIATIONS
                || selectedQuestion == Question.REPORT_MANAGEMENT_SUMMARY_OF_COST
                || selectedQuestion == Question.LAND_MANAGEMENT_REGISTER_PLOT
                || selectedQuestion == Question.ASSOCIATION_MANAGEMENT_JOIN) {

            currentMenu = menuService.getByQuestion(selectedQuestion);
            siblings = menuService.getNextMenus(currentMenu.getParentMenu());
            // validate user choice
            if (QuestionValidator.validateMenus(request.getInput(), siblings)) {
                selectedQuestion = siblings.get(Integer.parseInt(request.getInput()) - 1).getQuestion();
                previousQuestion = siblings.get(Integer.parseInt(request.getInput()) - 1).getParentMenu().getQuestion();
                List<UssdMenu> selectedMenus = menuService.getNextMenus(selectedQuestion);
                leaf = selectedMenus.get(0).getLeaf();
                stringBuilder.append(previousQuestion);
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(selectedQuestion);
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(request.getInput());
                stringBuilder.append(UTKit.EOL);
                // if question = association show association list
                // else if question = land show land list
                if (selectedQuestion == Question.ASSOCIATION_MANAGEMENT_VIEW
                        || selectedQuestion == Question.ASSOCIATION_MANAGEMENT_LEAVE) {
                    stringBuilder.append(UTKit.listMenus(selectedMenus));
                    stringBuilder.append(UTKit.EOL);
                    stringBuilder.append(associationService.showAssociation());
                } else {
                    stringBuilder.append(UTKit.listMenus(selectedMenus));
                }


                LOGGER.info("List of things {}", stringBuilder);
            } else {
                hasError = true;
                stringBuilder.append("Invalid Input");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(currentMenu.getParentMenu().getTitleKin());
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(UTKit.listMenus(siblings));
            }
        } else if (selectedQuestion == Question.ASSOCIATIONS_ENTER_ASSOCIATION_CODE) {
            // validate association code
            currentMenu = menuService.getByQuestion(selectedQuestion);
            nexMenus = menuService.getNextMenus(currentMenu);
            if (UTKit.validateAssociationCode(request.getInput())) {
                selectedQuestion = nexMenus.get(0).getQuestion();
                previousQuestion = nexMenus.get(0).getParentMenu().getQuestion();
                leaf = nexMenus.get(0).getLeaf();
                String associationCode = request.getInput();
                // API to check if association exists or not
                stringBuilder.append(UTKit.listMenus(nexMenus));
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(associationService.joinAssociation(associationCode));
            } else {
                hasError = true;
                stringBuilder.append("Invalid Association Code");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(currentMenu.getParentMenu().getTitleKin());
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(currentMenu.getTitleKin());
            }
        } else if (selectedQuestion == Question.ASSOCIATIONS_LEAVE_ASSOCIATION) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            nexMenus = menuService.getNextMenus(currentMenu);
            if (Integer.parseInt(request.getInput()) <= associationService.getAssociations().length) { // check if input is among association list
                selectedQuestion = nexMenus.get(0).getQuestion();
                previousQuestion = nexMenus.get(0).getParentMenu().getQuestion();
                leaf = nexMenus.get(0).getLeaf();
                String deletedAssociation = request.getInput(); // this is input which is <= association length
                // API to check if association exists or not
                stringBuilder.append(UTKit.listMenus(nexMenus));
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(associationService.deleteAssociation(deletedAssociation));
            } else {
                hasError = true;
                stringBuilder.append("Invalid choice");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(currentMenu.getParentMenu().getTitleKin());
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(currentMenu.getTitleKin());
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(associationService.showAssociation());
            }
        } else {
            LOGGER.info("=================== access last else ===================");
            selectedQuestion = menus.get(0).getQuestion();
            leaf = menus.get(0).getLeaf();
            stringBuilder.append(previousQuestion);
            stringBuilder.append(UTKit.EOL);
            stringBuilder.append(selectedQuestion);
            stringBuilder.append(UTKit.EOL);
            stringBuilder.append(request.getInput());
            stringBuilder.append(UTKit.EOL);
            stringBuilder.append(UTKit.listMenus(menus));
        }
        ResponseObject responseObject = new ResponseObject();
        responseObject.setDisplayMessage(stringBuilder.toString());
        responseObject.setHasError(hasError);
        responseObject.setLeaf(leaf);
        responseObject.setPreviousQuestion(previousQuestion);
        responseObject.setSelectedQuestion(selectedQuestion);
        responseObject.setQuestionnaire(questionnaire);
        LOGGER.info("responseObject {}", responseObject);
        return responseObject;
    }

    @Override
    public String sendUssdResponse(UssdResponse ussdResponse, HttpServletResponse httpServletResponse) {
        httpServletResponse.setHeader(UTKit.FREE_FLOW_HEADER, ussdResponse.getFreeflow().name());
        return ussdResponse.getMessage();
    }
}
