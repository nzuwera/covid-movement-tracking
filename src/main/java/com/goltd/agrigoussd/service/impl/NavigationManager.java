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


        /*
         * Get Current Session
         */
        Question currentQuestion = session.getQuestion();
        previousQuestion = session.getPreviousQuestion();
        /*
         * Get Previous Menu
         * Get Parent Menu
         */
        UssdMenu parentMenu = menuService.getByQuestion(previousQuestion);
        LOGGER.info("parentMenu {}", parentMenu);
        List<UssdMenu> previousMenus = menuService.getByParentId(parentMenu);
        LOGGER.info("previousMenus {}", previousMenus);
        if (previousMenus.get(0).getQuestionType() == QuestionType.LIST) {
            /*
             * Get Selected Menu
             */
            UssdMenu selectedMenu = previousMenus.get(Integer.parseInt(request.getInput()) - 1);
            LOGGER.info("selectedMenu {}", selectedMenu);
            /*
             * Get NextMenus
             */
            List<UssdMenu> nextMenus = menuService.getByParentId(selectedMenu);
            for (UssdMenu menu : nextMenus) {
                LOGGER.info("nextMenus question {}", menu.getQuestion());
            }
            /*
             * Get Next state
             */
            selectedQuestion = nextMenus.get(0).getQuestion();
            leaf = nextMenus.get(0).getLeaf();
        } else if (previousMenus.get(0).getQuestionType() == QuestionType.DYNAMIC_LIST) {
            /*
             * Get Selected Menu
             */
            UssdMenu selectedMenu = menuService.getByQuestion(currentQuestion);
            /*
             * Get NextMenus
             */
            List<UssdMenu> nextMenus = menuService.getChildrenByQuestion(selectedMenu.getQuestion());
            /*
             * Get Next state
             */
            selectedQuestion = nextMenus.get(0).getQuestion();
            leaf = nextMenus.get(0).getLeaf();
            LOGGER.info("previousMenus {}", previousMenus);
            LOGGER.info("selectedQuestion {}", selectedQuestion);
        } else if (previousMenus.get(0).getQuestionType() == QuestionType.ENUM || previousMenus.get(0).getQuestionType() == QuestionType.MESSAGE || previousMenus.get(0).getQuestionType() == QuestionType.FORM_INPUT) {
            /*
             * Get Selected Menu
             */
            UssdMenu selectedMenu = menuService.getByQuestion(currentQuestion);
            /*
             * Get NextMenus
             */
            List<UssdMenu> nextMenus = menuService.getChildrenByQuestion(selectedMenu.getQuestion());
            /*
             * Get Next state
             */
            selectedQuestion = nextMenus.get(0).getQuestion();
            leaf = nextMenus.get(0).getLeaf();
            LOGGER.info("selectedQuestion {}", selectedQuestion);
        }
        /*
         * Update session
         */
        session.setLastInput(session.getLastInput() + UTKit.JOINER + request.getInput());
        session.setPreviousQuestion(session.getQuestion());
        session.setQuestion(selectedQuestion);
        session.setLeaf(leaf);
        LOGGER.info("currentSession {}", session);
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
            List<UssdMenu> previousMenus = menuService.getChildrenByQuestion(previousQuestion);
            UssdMenu ussdMenu = menuService.getByQuestion(previousMenus.get(0).getQuestion());
            UssdMenu parentMenu = ussdMenu.getParentId();
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
        session.setLastInput("*123#");
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
    public UssdResponse buildMenu(UssdRequest ussdRequest, Question question) {
        UssdResponse response = new UssdResponse();
        List<UssdMenu> menus = menuService.getChildrenByQuestion(question);
        response.setFreeflow(menus.get(0).getLeaf());
        response.setMessage(this.formatMenu(ussdRequest, menus).toString());
        return response;
    }

    @Override
    public StringBuilder formatMenu(UssdRequest ussdRequest, List<UssdMenu> menus) {
        session = sessionService.getByMsisdn(ussdRequest.getMsisdn());
        StringBuilder listMessage = new StringBuilder();
        String locationCode = null;
        if (menus.size() > 1) {
            for (int i = 0; i < menus.size(); i++) {
                listMessage.append(i + 1);
                listMessage.append(UTKit.DOT + UTKit.BLANK);
                listMessage.append(menus.get(i).getTitleKin());
                listMessage.append(UTKit.EOL);
            }
        } else {
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
                    locationCode = UTKit.getLocationCode(session.getLastInput(), "province");
                    listMessage.append(menus.get(0).getTitleKin());
                    listMessage.append(UTKit.EOL);
                    listMessage.append(ListFormatter.formatLocations(locationService.getDistricts(locationCode)));
                    break;
                case REGISTRATION_SELECT_LOCATION_SECTOR:
                    locationCode = UTKit.getLocationCode(session.getLastInput(), "district");
                    listMessage.append(menus.get(0).getTitleKin());
                    listMessage.append(UTKit.EOL);
                    listMessage.append(ListFormatter.formatLocations(locationService.getSectors(locationCode)));
                    break;
                case REGISTRATION_SELECT_LOCATION_CELL:
                    locationCode = UTKit.getLocationCode(session.getLastInput(), "sector");
                    listMessage.append(menus.get(0).getTitleKin());
                    listMessage.append(UTKit.EOL);
                    listMessage.append(ListFormatter.formatLocations(locationService.getCells(locationCode)));
                    LOGGER.info("locationCode {}", locationCode);
                    break;
                case REGISTRATION_SELECT_LOCATION_VILLAGE:
                    locationCode = UTKit.getLocationCode(session.getLastInput(), "cell");
                    listMessage.append(menus.get(0).getTitleKin());
                    listMessage.append(UTKit.EOL);
                    listMessage.append(ListFormatter.formatLocations(locationService.getVillages(locationCode)));
                    LOGGER.info("locationCode {}", locationCode);
                    break;
                case REGISTRATION_ENTER_PIN:
                    listMessage.append(menus.get(0).getTitleKin());
                    LOGGER.info("REGISTRATION_ENTER_PIN {}", ussdRequest.getInput());
                    break;
                case REGISTRATION_VERIFY_PIN:
                    listMessage.append(menus.get(0).getTitleKin());
                    LOGGER.info("REGISTRATION_VERIFY_PIN {}", ussdRequest.getInput());
                    break;
                case REGISTRATION_COMPLETED:
                    String lastInput = session.getLastInput();
                    UserAccount userAccount = UTKit.getUserDetailsFromLastInput(ussdRequest.getMsisdn(), lastInput);
                    try {
                        userService.create(userAccount);
                        listMessage.append(menus.get(0).getTitleKin());
                    } catch (Exception ex) {
                        LOGGER.error("Registration Error {}", ex.getMessage());
                        listMessage.append("Registration failed: ");
                    }
                    break;

                default:
                    listMessage.append(menus.get(0).getTitleKin());
                    break;
            }
        }
        session.setLeaf(menus.get(0).getLeaf());
        sessionService.update(session);
        LOGGER.info("currentInput {}", ussdRequest.getInput());
        LOGGER.info("lastInput {}", session.getLastInput());
        return listMessage;
    }

    @Override
    public String sendUssdResponse(UssdResponse ussdResponse, HttpServletResponse httpServletResponse) {
        httpServletResponse.setHeader(UTKit.FREE_FLOW_HEADER, ussdResponse.getFreeflow().name());
        return ussdResponse.getMessage();
    }


    @Override
    public String traverseForward(Session session, UssdRequest request) {
        Question currentQuestion = session.getQuestion();
        Question previousQuestion = session.getPreviousQuestion();

        List<UssdMenu> previousMenus = menuService.getMenusByQuestion(previousQuestion);
        List<UssdMenu> menus = menuService.getMenusByQuestion(currentQuestion);

        return null;
    }
}
