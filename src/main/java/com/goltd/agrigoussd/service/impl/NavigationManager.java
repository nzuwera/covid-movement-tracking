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
    private QuestionType questionType;

    @Override
    public Session forward(Session session, UssdRequest request) {
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
            LOGGER.info("selectedQuestion {}, Type {}", selectedQuestion, questionType);
        } else if (questionType == QuestionType.DYNAMIC_LIST) {
            selectedMenu = menuService.getByQuestion(currentQuestion);
            nextMenus = menuService.getNextMenus(selectedMenu.getQuestion());
            selectedQuestion = nextMenus.get(0).getQuestion();
            leaf = nextMenus.get(0).getLeaf();
            LOGGER.info("selectedQuestion {}, Type {}", selectedQuestion, questionType.name());
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
            LOGGER.info("selectedQuestion {}, Type {}", selectedQuestion, questionType.name());
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
        leaf = false;
        String ussdMessage = "";
        UssdMenu currentMenu = menuService.getByQuestion(session.getQuestion());
        Question nextQuestion = null;
        List<UssdMenu> nextMenus;
        UssdMenu parentMenu;
        List<UssdMenu> parentSiblings;

        nextMenus = menuService.getNextMenus(currentMenu);

        for (UssdMenu nextMenu : nextMenus) {
            LOGGER.info("nextMenus nextMenu.getQuestion() {}, nextMenu.getTitleKin() {}", nextMenu.getQuestion(), nextMenu.getTitleKin());
        }


        if (nextMenus.get(0).getQuestionType().equals(QuestionType.LIST)) {

            ussdMessage = UTKit.listMenus(nextMenus);

            leaf = nextMenus.get(0).getLeaf();

            if (nextMenus.get(0).getParentMenu().getQuestionType().equals(QuestionType.LIST)) {
                parentMenu = currentMenu.getParentMenu();
                LOGGER.info("nextMenus.get(0).getQuestionType().equals(QuestionType.LIST) | parenMenu {}", parentMenu);
                parentSiblings = menuService.getNextMenus(parentMenu);
                previousQuestion = parentSiblings.get(Integer.parseInt(ussdRequest.getInput()) - 1).getParentMenu().getQuestion();
                nextQuestion = parentSiblings.get(Integer.parseInt(ussdRequest.getInput()) - 1).getQuestion();
                LOGGER.info("QuestionType.LIST QuestionType.LIST nextQuestion {} input {} ", nextQuestion, ussdRequest.getInput());
            } else {
                // VALIDATE FORM_INPUT
                // VALIDATE
                previousQuestion = session.getPreviousQuestion();
                nextQuestion = session.getQuestion();
                LOGGER.info("QuestionType.LIST else nextQuestion {} input {} ", nextQuestion, ussdRequest.getInput());
            }

        } else if (nextMenus.get(0).getQuestionType().equals(QuestionType.ENUM)) {
            ussdMessage = UTKit.listEnums(nextMenus.get(0).getTitleKin(), nextMenus.get(0).getQuestion());
            leaf = nextMenus.get(0).getLeaf();
            if (nextMenus.get(0).getParentMenu().getQuestionType().equals(QuestionType.LIST)) {
                parentMenu = currentMenu.getParentMenu();
                LOGGER.info("nextMenus.get(0).getQuestionType().equals(QuestionType.ENUM) parenMenu {}", parentMenu);
                parentSiblings = menuService.getNextMenus(parentMenu);
                for (UssdMenu siblings : parentSiblings) {
                    LOGGER.info("Siblings question {}, title {} ", siblings.getQuestion(), siblings.getTitleKin());
                }
                previousQuestion = parentSiblings.get(Integer.parseInt(ussdRequest.getInput()) - 1).getParentMenu().getQuestion();
                nextQuestion = parentSiblings.get(Integer.parseInt(ussdRequest.getInput()) - 1).getQuestion();
                LOGGER.info("QuestionType.ENUM QuestionType.LIST nextQuestion {} input {} ", nextQuestion, ussdRequest.getInput());
                session.setQuestion(nextQuestion);
                sessionService.update(session);
            } else {
                previousQuestion = nextMenus.get(0).getParentMenu().getQuestion();
                nextQuestion = nextMenus.get(0).getQuestion();
                LOGGER.info("QuestionType.LIST else nextQuestion {} input {} ", nextQuestion, ussdRequest.getInput());
            }
        } else if (nextMenus.get(0).getQuestionType().equals(QuestionType.DYNAMIC_LIST)) {
            ussdMessage = this.formatMenu(session.getLastInput() + UTKit.JOINER + ussdRequest.getInput(), nextMenus);
            leaf = nextMenus.get(0).getLeaf();
            if (nextMenus.get(0).getParentMenu().getQuestionType().equals(QuestionType.LIST)) {
                parentMenu = currentMenu.getParentMenu();
                LOGGER.info("nextMenus.get(0).getQuestionType().equals(QuestionType.DYNAMIC_LIST) parenMenu {}", parentMenu);
                parentSiblings = menuService.getNextMenus(parentMenu);
                for (UssdMenu siblings : parentSiblings) {
                    LOGGER.info("Siblings question {}, title {}", siblings.getQuestion(), siblings.getTitleKin());
                }
                previousQuestion = parentMenu.getQuestion();
                nextQuestion = parentSiblings.get(Integer.parseInt(ussdRequest.getInput()) - 1).getQuestion();
                LOGGER.info("QuestionType.DYNAMIC_LIST QuestionType.LIST nextQuestion {} input {} ", nextQuestion, ussdRequest.getInput());
            } else {
                previousQuestion = nextMenus.get(0).getParentMenu().getQuestion();
                nextQuestion = nextMenus.get(0).getQuestion();
                LOGGER.info("QuestionType.DYNAMIC_LIST else nextQuestion {} input {} ", nextQuestion, ussdRequest.getInput());
            }
        } else {
            ussdMessage = nextMenus.get(0).getTitleKin();
            leaf = nextMenus.get(0).getLeaf();
            if (nextMenus.get(0).getParentMenu().getQuestionType().equals(QuestionType.LIST)) {
                parentMenu = currentMenu.getParentMenu();
                LOGGER.info("else parenMenu {}", parentMenu);
                parentSiblings = menuService.getNextMenus(parentMenu);
                for (UssdMenu siblings : parentSiblings) {
                    LOGGER.info("Siblings question {}, title {}", siblings.getQuestion(), siblings.getTitleKin());
                }
                previousQuestion = parentSiblings.get(Integer.parseInt(ussdRequest.getInput()) - 1).getParentMenu().getQuestion();
                nextQuestion = parentSiblings.get(Integer.parseInt(ussdRequest.getInput()) - 1).getQuestion();
                LOGGER.info("else QuestionType.LIST nextQuestion {} input {} ", nextQuestion, ussdRequest.getInput());
            } else {
                previousQuestion = nextMenus.get(0).getParentMenu().getQuestion();
                nextQuestion = nextMenus.get(0).getQuestion();
                LOGGER.info("else else nextQuestion {} input {} ", nextQuestion, ussdRequest.getInput());
            }
        }

        if (leaf.equals(true)) {
            switch (currentMenu.getQuestionnaire()) {
                case REGISTRATION:
                    UserAccount userAccount = UTKit.getUserDetailsFromLastInput(ussdRequest.getMsisdn(), session.getLastInput() + UTKit.JOINER + ussdRequest.getInput());
                    userService.create(userAccount);
                    break;
                case ASSOCIATION:
                    break;
                case LAND:
                    break;
                case HELP:
                    break;
                case ACCOUNT:
                    break;
                case ACTIVITY:
                    break;
                case REPORT:
                    break;
                case AIRTIME:
                    break;
                case MARKETPLACE:
                    break;
            }
        }

        String lastInput = (session.getLastInput().equals(SHORT_CODE) ? session.getLastInput() : session.getLastInput() + UTKit.JOINER + ussdRequest.getInput());
        session.setLeaf(leaf);
        session.setPreviousQuestion(previousQuestion);
        session.setQuestion(nextQuestion);
        session.setLastInput(lastInput);
        sessionService.update(session);
        response.setFreeflow(leaf);
        response.setMessage(ussdMessage);
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
        }

        return listMessage.toString();
    }

    @Override
    public StringBuilder formatMenu(UssdRequest ussdRequest, List<UssdMenu> menus) {
        session = sessionService.getByMsisdn(ussdRequest.getMsisdn());
        StringBuilder listMessage = new StringBuilder();
        if (menus.size() >= 1 && menus.get(0).getQuestionType() == QuestionType.LIST) {
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
