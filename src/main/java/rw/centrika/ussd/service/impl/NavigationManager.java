package rw.centrika.ussd.service.impl;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import rw.centrika.ussd.domain.*;
import rw.centrika.ussd.helpers.*;
import rw.centrika.ussd.helpers.enums.Question;
import rw.centrika.ussd.helpers.enums.Questionnaire;
import rw.centrika.ussd.helpers.enums.Visibility;
import rw.centrika.ussd.helpers.formatter.EnumFormatter;
import rw.centrika.ussd.service.BookingService;
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

    @Value("${application.short-code}")
    private String shortCode;

    private ISessionService sessionService;
    private IMenuService menuService;
    private IUserService userService;
    private BookingService bookingService;

    @Autowired
    public NavigationManager(ISessionService sessionService, IMenuService menuService, IUserService userService, BookingService bookingService) {
        this.sessionService = sessionService;
        this.menuService = menuService;
        this.userService = userService;
        this.bookingService = bookingService;
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
        session.setQuestionnaire(Questionnaire.MAIN);
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
        String lastInput = (request.getNewRequest().equals("1") ? request.getInput() : session.getLastInput() + UTKit.JOINER + request.getInput());
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
        } else if (selectedQuestion == Question.DEPARTURE
                || selectedQuestion == Question.DESTINATION) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            nexMenus = menuService.getNextMenus(currentMenu);
            BusResponseObject stopResponseObject = bookingService.getStopByName(request.getInput());
            if (Boolean.FALSE.equals(stopResponseObject.getStatus())) {
                stringBuilder.append(UTKit.listMenus(prefferedLanguage, nexMenus));
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(stopResponseObject.getMessage());
            } else {
                hasError = true;
                stringBuilder.append("Selected bus stop not found");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(UTKit.setTitle(prefferedLanguage, currentMenu));
            }
            // Get busStops based on selected input
        } else if (selectedQuestion == Question.CONFIRM_DEPARTURE
                || selectedQuestion == Question.CONFIRM_DESTINATION) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            nexMenus = menuService.getNextMenus(currentMenu);
            String previousInput = UTKit.getLastInput(session.getLastInput());
            BusResponseObject stopResponseObject = bookingService.validateSelectedBus(request.getInput(), previousInput);
            if (Boolean.FALSE.equals(stopResponseObject.getStatus())) {
                lastInput = UTKit.replaceLastInput(session.getLastInput(), stopResponseObject.getMessage());
                if (selectedQuestion == Question.CONFIRM_DESTINATION) {
                    stringBuilder.append(UTKit.listMenus(prefferedLanguage, nexMenus));
                    stringBuilder.append(UTKit.EOL);
                    stringBuilder.append(UTKit.getBusTime());
                    // get trip departure time
                } else {
                    stringBuilder.append(UTKit.listMenus(prefferedLanguage, nexMenus));
                }
            } else {
                stopResponseObject = bookingService.getStopByName(previousInput);
                hasError = true;
                stringBuilder.append("Invalid selected option");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(UTKit.setTitle(prefferedLanguage, currentMenu));
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(stopResponseObject.getMessage());
            }

        } else if (selectedQuestion == Question.SHOW_DEPARTURE_TIME) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            nexMenus = menuService.getNextMenus(currentMenu);
            if (Boolean.FALSE.equals(bookingService.validateDepartureTime(request.getInput()).getStatus())) {
                try {
                    String selectedTime = bookingService.validateDepartureTime(request.getInput()).getMessage();
                    lastInput = session.getLastInput() + UTKit.JOINER + selectedTime;
                    String[] inputs = session.getLastInput().split(UTKit.JOINER);
                    String cityIn = inputs[inputs.length - 2];
                    String cityOut = inputs[inputs.length - 1];
                    LOGGER.info("location cityIn {} cityOut {} selectedTime {}", cityIn, cityOut, selectedTime);
                    BusTime busTime = new BusTime(selectedTime);
                    BusListRequest busListRequest = new BusListRequest(cityIn, cityOut, busTime.getStartDate(), busTime.getStartTime());
                    LOGGER.info("busListRequest {}", busListRequest);
                    BusListSuccess listSuccess = bookingService.getBusLists(busListRequest);
                    BusResponseObject busResponseObject = bookingService.showAvailableBuses(listSuccess);
                    if (Boolean.FALSE.equals(busResponseObject.getStatus())) {
                        stringBuilder.append(UTKit.listMenus(prefferedLanguage, nexMenus));
                        stringBuilder.append(UTKit.EOL);
                        stringBuilder.append(busResponseObject.getMessage());
                    } else {
                        hasError = true;
                        stringBuilder.append(busResponseObject.getMessage());
                        stringBuilder.append(UTKit.EOL);
                        stringBuilder.append(UTKit.setTitle(prefferedLanguage, currentMenu));
                        stringBuilder.append(UTKit.EOL);
                        stringBuilder.append(UTKit.getBusTime());
                    }
                } catch (Exception ex) {
                    LOGGER.error(ex.getCause().getMessage());
                    hasError = true;
                    stringBuilder.append("Error while getting available buses");
                    stringBuilder.append(UTKit.EOL);
                    stringBuilder.append(UTKit.setTitle(prefferedLanguage, currentMenu));
                    stringBuilder.append(UTKit.EOL);
                    stringBuilder.append(UTKit.getBusTime());
                }
            } else {
                hasError = true;
                stringBuilder.append("Invalid time selected");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(UTKit.setTitle(prefferedLanguage, currentMenu));
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(UTKit.getBusTime());
            }
        } else if (selectedQuestion == Question.SHOW_AVAILABLE_BUSES) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            nexMenus = menuService.getNextMenus(currentMenu);
            String[] sessionInputs = session.getLastInput().split(UTKit.JOINER);
            for (String sessionInput : sessionInputs) {
                LOGGER.info("sessionInputs {}", sessionInput);
            }
            String cityIn = sessionInputs[sessionInputs.length - 3];
            String cityOut = sessionInputs[sessionInputs.length - 2];
            String departureTime = sessionInputs[sessionInputs.length - 1];
            String startDate = departureTime.split(UTKit.BLANK)[0];
            String startTime = departureTime.split(UTKit.BLANK)[1];
            BusListRequest listRequest = new BusListRequest(cityIn, cityOut, startDate, startTime);
            BusListSuccess listSuccess = bookingService.getBusLists(listRequest);
            BusResponseObject availableBusResponse = bookingService.validateBusList(request.getInput(), listSuccess);
            if (Boolean.FALSE.equals(availableBusResponse.getStatus())) {
                String busName = listSuccess.getResult().get(Integer.parseInt(request.getInput()) - 1).getName().replace(UTKit.JOINER, UTKit.EMPTY);
                String amount = listSuccess.getResult().get(Integer.parseInt(request.getInput()) - 1).getTotalAmount();
                String currency = listSuccess.getResult().get(Integer.parseInt(request.getInput()) - 1).getCurrency();
                lastInput = session.getLastInput() + UTKit.BLANK + busName + UTKit.BLANK + amount + currency;
                stringBuilder.append(UTKit.listMenus(prefferedLanguage, nexMenus));
            } else {
                hasError = true;
                stringBuilder.append(availableBusResponse.getMessage());
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(UTKit.setTitle(prefferedLanguage, currentMenu));
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(bookingService.showAvailableBuses(listSuccess).getMessage());
            }
        } else if (selectedQuestion == Question.ENTER_BUS_CARD) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            nexMenus = menuService.getNextMenus(currentMenu);

        } else {
            LOGGER.info("=================== access last else ===================");
            selectedQuestion = menus.get(0).getQuestion();
            currentMenu = menuService.getByQuestion(selectedQuestion);
            nexMenus = menuService.getNextMenus(currentMenu);
            stringBuilder.append(UTKit.listMenus(prefferedLanguage, nexMenus));
            leaf = nexMenus.get(0).getLeaf();
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
