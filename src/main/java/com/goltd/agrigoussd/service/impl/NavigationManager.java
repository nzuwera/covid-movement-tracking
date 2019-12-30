package com.goltd.agrigoussd.service.impl;

import com.goltd.agrigoussd.domain.Session;
import com.goltd.agrigoussd.domain.UserAccount;
import com.goltd.agrigoussd.domain.UssdMenu;
import com.goltd.agrigoussd.helpers.UTKit;
import com.goltd.agrigoussd.helpers.UssdRequest;
import com.goltd.agrigoussd.helpers.UssdResponse;
import com.goltd.agrigoussd.helpers.enums.*;
import com.goltd.agrigoussd.helpers.formatter.EnumFormatter;
import com.goltd.agrigoussd.helpers.formatter.ListFormatter;
import com.goltd.agrigoussd.service.interfaces.*;
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

    @Autowired
    public NavigationManager(ISessionService sessionService, IMenuService menuService, ILocationService locationService, IUserService userService) {
        this.sessionService = sessionService;
        this.menuService = menuService;
        this.locationService = locationService;
        this.userService = userService;
    }

    private Session session;
    private Question previousQuestion;
    private Question selectedQuestion;
    private Boolean leaf;

    @Override
    public Session forward(Session session, UssdRequest request) {

        QuestionType questionType;
        UssdMenu selectedMenu;
        List<UssdMenu> nextMenus;

        /*
         * Get Current Session
         */
        Question currentQuestion = session.getQuestion();
        UssdMenu currentMenu = menuService.getByQuestion(currentQuestion);
        LOGGER.info("currentQuestion {}", currentQuestion);
        previousQuestion = session.getPreviousQuestion();
        /*
         * Get Previous Menu
         * Get Parent Menu
         */
        List<UssdMenu> previousMenus = menuService.getNextMenus(currentQuestion);
        questionType = currentMenu.getQuestionType();
        LOGGER.info("questionType {}", questionType);
        LOGGER.info("previousMenus {}", previousMenus);
        if (questionType == QuestionType.LIST) {
            // Check if children is not list
            currentMenu = menuService.getByQuestion(currentQuestion);
            if (menuService.getNextMenus(currentMenu).get(0).getQuestionType() == QuestionType.LIST) {
                selectedMenu = previousMenus.get(Integer.parseInt(request.getInput()) - 1);
                LOGGER.info("selectedMenu {}", selectedMenu);
                nextMenus = menuService.getNextMenus(selectedMenu);
                LOGGER.info("nextMenus {}", nextMenus);
                selectedQuestion = nextMenus.get(0).getQuestion();
                leaf = nextMenus.get(0).getLeaf();
            } else {
                LOGGER.info("questionType {}", questionType);
                nextMenus = menuService.getNextMenus(currentQuestion);
                LOGGER.info("nextMenus {}", nextMenus.get(0).getTitleKin());
                selectedQuestion = nextMenus.get(0).getQuestion();
                leaf = nextMenus.get(0).getLeaf();
            }
        } else if (questionType == QuestionType.DYNAMIC_LIST) {
            selectedMenu = menuService.getByQuestion(currentQuestion);
            nextMenus = menuService.getNextMenus(selectedMenu.getQuestion());
            selectedQuestion = nextMenus.get(0).getQuestion();
            leaf = nextMenus.get(0).getLeaf();
        } else if (questionType == QuestionType.ENUM || questionType == QuestionType.MESSAGE || questionType == QuestionType.FORM_INPUT) {

            if (previousMenus.get(0).getQuestionType() == QuestionType.LIST) {
                selectedMenu = previousMenus.get(Integer.parseInt(request.getInput()) - 1);
                nextMenus = menuService.getNextMenus(selectedMenu.getParentMenu());
                selectedQuestion = nextMenus.get(Integer.parseInt(request.getInput()) - 1).getQuestion();
                leaf = nextMenus.get(Integer.parseInt(request.getInput()) - 1).getLeaf();
            } else {
                selectedMenu = menuService.getByQuestion(currentQuestion);
                nextMenus = menuService.getNextMenus(selectedMenu.getQuestion());
                selectedQuestion = nextMenus.get(0).getQuestion();
                leaf = nextMenus.get(0).getLeaf();
            }
            LOGGER.info("selectedQuestion {}, Type {}", selectedQuestion, questionType);
        }
        /*
         * Update session
         */
        session.setLastInput(session.getLastInput() + UTKit.JOINER + request.getInput());
        session.setPreviousQuestion(session.getQuestion());
        session.setQuestion(selectedQuestion);
        session.setLeaf(leaf);
        return sessionService.update(session);
    }

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
            LOGGER.info("===== prevState {}", previousQuestion);
        }
        LOGGER.info("===== prevParentState {}", previousQuestion);
        LOGGER.info("===== backward session {}", session);
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
        selectedQuestion = nextMenus.get(0).getQuestion();
        previousQuestion = menuService.getByQuestion(selectedQuestion).getParentMenu().getQuestion();
        leaf = nextMenus.get(0).getLeaf();
        String displayMessage = UTKit.listMenus(nextMenus);


        String lastInput = (session.getLastInput().equals(SHORT_CODE) && ussdRequest.getNewRequest().equals("1") ? session.getLastInput() : session.getLastInput() + UTKit.JOINER + ussdRequest.getInput());
        session.setLeaf(leaf);
        session.setPreviousQuestion(previousQuestion);
        session.setQuestion(selectedQuestion);
        session.setLastInput(lastInput);
        session.setQuestionnaire(nextMenus.get(0).getQuestionnaire());
        session.setStartService(nextMenus.get(0).getServiceStart());
        sessionService.update(session);
        response.setFreeflow(leaf);
        response.setMessage(displayMessage);
        return response;
    }

    @Override
    public String formatMenu(String input, List<UssdMenu> menus) {
        StringBuilder listMessage = new StringBuilder();
        String locationCode = null;
        switch (menus.get(0).getQuestion()) {
            case REGISTRATION_SELECT_GENDER:
                listMessage.append(menus.get(0).getTitleKin());
                listMessage.append(UTKit.EOL);
                listMessage.append(EnumFormatter.format(Gender.class));
                break;
            case REGISTRATION_SELECT_LOCATION_PROVINCE:
                listMessage.append(menus.get(0).getTitleKin());
                listMessage.append(UTKit.EOL);
                listMessage.append(ListFormatter.formatLocations(locationService.getProvinces()));
                break;
            case REGISTRATION_SELECT_LOCATION_DISTRICT:
                locationCode = UTKit.getLocationCode(input, "province");
                listMessage.append(menus.get(0).getTitleKin());
                listMessage.append(UTKit.EOL);
                listMessage.append(ListFormatter.formatLocations(locationService.getDistricts(locationCode)));
                break;
            case REGISTRATION_SELECT_LOCATION_SECTOR:
                locationCode = UTKit.getLocationCode(input, "district");
                listMessage.append(menus.get(0).getTitleKin());
                listMessage.append(UTKit.EOL);
                listMessage.append(ListFormatter.formatLocations(locationService.getSectors(locationCode)));
                break;
            case REGISTRATION_SELECT_LOCATION_CELL:
                locationCode = UTKit.getLocationCode(input, "sector");
                listMessage.append(menus.get(0).getTitleKin());
                listMessage.append(UTKit.EOL);
                listMessage.append(ListFormatter.formatLocations(locationService.getCells(locationCode)));
                break;
            case REGISTRATION_SELECT_LOCATION_VILLAGE:
                locationCode = UTKit.getLocationCode(input, "cell");
                listMessage.append(menus.get(0).getTitleKin());
                listMessage.append(UTKit.EOL);
                listMessage.append(ListFormatter.formatLocations(locationService.getVillages(locationCode)));
                break;
            default:
                listMessage.append(UTKit.listMenus(menus));
                break;
        }

        return listMessage.toString();
    }

    @Override
    public StringBuilder formatMenu(UssdRequest ussdRequest, List<UssdMenu> menus) {
        int menuSize = menus.size();
        session = sessionService.getByMsisdn(ussdRequest.getMsisdn());
        StringBuilder listMessage = new StringBuilder();
        if (menuSize >= 1 && menus.get(0).getQuestionType() == QuestionType.LIST) {
            for (int i = 0; i < menus.size(); i++) {
                listMessage.append(i + 1);
                listMessage.append(UTKit.DOT + UTKit.BLANK);
                listMessage.append(menus.get(i).getTitleKin());
                listMessage.append(UTKit.EOL);
            }
        } else {
            switch (menus.get(0).getQuestion()) {
                case REGISTRATION_ENTER_PIN:
                    listMessage.append(menus.get(0).getTitleKin());
                    break;
                case REGISTRATION_VERIFY_PIN:
                    listMessage.append(menus.get(0).getTitleKin());
                    break;
                case REGISTRATION_COMPLETED:
                    String lastInput = session.getLastInput();
                    UserAccount userAccount = UTKit.getUserDetailsFromLastInput(ussdRequest.getMsisdn(), lastInput);
                    try {
                        userService.create(userAccount);
                        listMessage.append(menus.get(0).getTitleKin());
                    } catch (Exception ex) {
                        listMessage.append("Registration failed: ");
                    }
                    break;
                default:
                    listMessage.append(menus.get(0).getTitleKin());
                    break;
            }
        }
        return listMessage;
    }

    @Override
    public String sendUssdResponse(UssdResponse ussdResponse, HttpServletResponse httpServletResponse) {
        httpServletResponse.setHeader(UTKit.FREE_FLOW_HEADER, ussdResponse.getFreeflow().name());
        return ussdResponse.getMessage();
    }


    @Override
    public String traverseForward(Session session, UssdRequest request) {
        return null;
    }
}
