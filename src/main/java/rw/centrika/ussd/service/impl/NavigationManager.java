package rw.centrika.ussd.service.impl;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import rw.centrika.ussd.domain.Language;
import rw.centrika.ussd.domain.Session;
import rw.centrika.ussd.domain.UserAccount;
import rw.centrika.ussd.domain.UssdMenu;
import rw.centrika.ussd.helpers.ResponseObject;
import rw.centrika.ussd.helpers.UTKit;
import rw.centrika.ussd.helpers.UssdRequest;
import rw.centrika.ussd.helpers.UssdResponse;
import rw.centrika.ussd.helpers.enums.Question;
import rw.centrika.ussd.helpers.enums.Questionnaire;
import rw.centrika.ussd.helpers.enums.Visibility;
import rw.centrika.ussd.helpers.formatter.EnumFormatter;
import rw.centrika.ussd.service.interfaces.IMenuService;
import rw.centrika.ussd.service.interfaces.INavigationManager;
import rw.centrika.ussd.service.interfaces.ISessionService;
import rw.centrika.ussd.service.interfaces.IUserService;
import rw.centrika.ussd.validators.QuestionValidator;

import javax.servlet.http.HttpServletResponse;
import java.util.List;

@Service
public class NavigationManager implements INavigationManager {
    private static final Logger LOGGER = LoggerFactory.getLogger(NavigationManager.class);
    private static final String SHORT_CODE = "*123#";

    private ISessionService sessionService;
    private IMenuService menuService;
    private IUserService userService;

    @Autowired
    public NavigationManager(ISessionService sessionService, IMenuService menuService, IUserService userService) {
        this.sessionService = sessionService;
        this.menuService = menuService;
        this.userService = userService;
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
    public UssdResponse buildMenu(UssdRequest ussdRequest, Session currentSession) {
        UssdResponse response = new UssdResponse();
        session = currentSession;
        Question currentQuestion = session.getQuestion();
        UssdMenu currentMenu = menuService.getByQuestion(currentQuestion);
        List<UssdMenu> nextMenus = menuService.getNextMenus(currentMenu);
        ResponseObject responseObject = this.prepareDisplayMessage(ussdRequest, nextMenus);
        String displayMessage = responseObject.getDisplayMessage();
        leaf = responseObject.getLeaf();
        selectedQuestion = responseObject.getSelectedQuestion();
        previousQuestion = responseObject.getPreviousQuestion();
        Questionnaire questionnaire = responseObject.getQuestionnaire();


        session.setLeaf(leaf);
        session.setPreviousQuestion(previousQuestion);
        session.setQuestion(selectedQuestion);
        session.setLastInput(responseObject.getLastInput());
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
        Language prefferedLanguage = session.getLanguage();
        UssdMenu currentMenu;
        List<UssdMenu> siblings;
        List<UssdMenu> nexMenus;
        StringBuilder stringBuilder = new StringBuilder();
        Boolean hasError = false;
        String lastInput = (session.getLastInput().equals(SHORT_CODE) && request.getNewRequest().equals("1") ? session.getLastInput() : session.getLastInput() + UTKit.JOINER + request.getInput());
        leaf = menus.get(0).getLeaf();
        Questionnaire questionnaire = menus.get(0).getQuestionnaire();
        previousQuestion = menus.get(0).getParentMenu().getQuestion();
        selectedQuestion = menus.get(0).getQuestion();
        if (selectedQuestion == Question.MAIN_MENU) {
            List<UssdMenu> children = menuService.getNextMenus(selectedQuestion);
            stringBuilder.append(UTKit.setTitle(prefferedLanguage, children.get(0).getParentMenu()));
            stringBuilder.append(UTKit.EOL);
            stringBuilder.append(UTKit.listMenus(prefferedLanguage, children));
        } else if (selectedQuestion == Question.BUS_BOOKING) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            siblings = menuService.getNextMenus(currentMenu.getParentMenu());
            if (QuestionValidator.validateMenus(request.getInput(), siblings)) {
                selectedQuestion = siblings.get(Integer.parseInt(request.getInput()) - 1).getQuestion();
                previousQuestion = siblings.get(Integer.parseInt(request.getInput()) - 1).getParentMenu().getQuestion();
                nexMenus = menuService.getNextMenus(selectedQuestion);
                if (selectedQuestion.equals(Question.LANGUAGE)) {
                    stringBuilder.append(UTKit.listMenus(prefferedLanguage, nexMenus));
                    stringBuilder.append(UTKit.EOL);
                    stringBuilder.append(EnumFormatter.format(Language.class));
                } else {
                    stringBuilder.append(UTKit.listMenus(prefferedLanguage, nexMenus));
                }
            } else {
                hasError = true;
                stringBuilder.append("Invalid service");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(UTKit.setTitle(prefferedLanguage, currentMenu));
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(UTKit.listMenus(prefferedLanguage, siblings));
            }
        } else if (selectedQuestion == Question.SELECT_LANGUAGE) {
            // validate association code
            currentMenu = menuService.getByQuestion(selectedQuestion);
            nexMenus = menuService.getNextMenus(currentMenu);
            if (Boolean.TRUE.equals(QuestionValidator.validateEnum(request.getInput(), Language.class))) {
                selectedQuestion = nexMenus.get(0).getQuestion();
                previousQuestion = nexMenus.get(0).getParentMenu().getQuestion();
                leaf = nexMenus.get(0).getLeaf();
                try {
                    leaf = nexMenus.get(0).getLeaf();
                    UserAccount userAccount = userService.getUserByMsisdn(request.getMsisdn());
                    userAccount.setLanguage(Language.values()[Integer.parseInt(request.getInput()) - 1]);
                    userService.update(userAccount);
                    session.setLanguage(Language.values()[Integer.parseInt(request.getInput()) - 1]);
                    stringBuilder.append(UTKit.listMenus(prefferedLanguage, nexMenus));
                } catch (Exception ex) {
                    hasError = true;
                    leaf = nexMenus.get(0).getLeaf();
                    stringBuilder.append("Invalid Error happened while updating language");
                    stringBuilder.append(UTKit.EOL);
                    stringBuilder.append(UTKit.listMenus(prefferedLanguage, nexMenus));
                    stringBuilder.append(UTKit.EOL);
                    stringBuilder.append(EnumFormatter.format(Language.class));
                }
            } else {
                hasError = true;
                stringBuilder.append("Invalid Language");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(UTKit.setTitle(prefferedLanguage, currentMenu));
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(EnumFormatter.format(Language.class));
            }
        } else {
            LOGGER.info("=================== access last else ===================");
            selectedQuestion = menus.get(0).getQuestion();
            leaf = menus.get(0).getLeaf();
            stringBuilder.append(UTKit.listMenus(prefferedLanguage, menus));
        }
        ResponseObject responseObject = new ResponseObject();
        responseObject.setDisplayMessage(stringBuilder.toString());
        responseObject.setHasError(hasError);
        responseObject.setLeaf(leaf);
        responseObject.setLastInput(lastInput);
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
