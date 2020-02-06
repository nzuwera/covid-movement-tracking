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
        leaf = responseObject.getLeaf();
        String mainMenuString = session.getLanguage() == Language.KIN ? "99. Aha banza" : "99. Main menu";
        String displayMessage = (session.getQuestion() == Question.START || Boolean.TRUE.equals(leaf)) ? responseObject.getDisplayMessage() : responseObject.getDisplayMessage() + UTKit.EOL + UTKit.EOL + mainMenuString;

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
        Language sessionLanguage = session.getLanguage();
        UssdMenu currentMenu;
        List<UssdMenu> nexMenus;
        StringBuilder stringBuilder = new StringBuilder();
        Boolean hasError = false;
        String lastInput = (request.getNewRequest().equals("1") ? request.getInput() : session.getLastInput() + UTKit.JOINER + request.getInput());
        leaf = menus.get(0).getLeaf();
        Questionnaire questionnaire = menus.get(0).getQuestionnaire();
        previousQuestion = menus.get(0).getParentMenu().getQuestion();
        selectedQuestion = menus.get(0).getQuestion();
        if (selectedQuestion == Question.LANGUAGE) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            stringBuilder.append("Murakaza neza kuri serivise za Safaribus.");
            stringBuilder.append(UTKit.EOL);
            stringBuilder.append(UTKit.setTitle(sessionLanguage, currentMenu));
            stringBuilder.append(UTKit.EOL);
            stringBuilder.append(EnumFormatter.format(Language.class));
        } else if (selectedQuestion == Question.SELECT_LANGUAGE) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            nexMenus = menuService.getNextMenus(currentMenu);
            if (Boolean.TRUE.equals(QuestionValidator.validateEnum(request.getInput(), Language.class))) {
                leaf = currentMenu.getLeaf();
                try {
                    UserAccount userAccount = userService.getUserByMsisdn(request.getMsisdn());
                    userAccount.setLanguage(Language.values()[Integer.parseInt(request.getInput()) - 1]);
                    userService.update(userAccount);
                    session.setLanguage(Language.values()[Integer.parseInt(request.getInput()) - 1]);
                    sessionLanguage = session.getLanguage();
                    stringBuilder.append(UTKit.setTitle(sessionLanguage, nexMenus.get(0)));
                } catch (Exception ex) {
                    hasError = true;
                    stringBuilder.append("Invalid Error happened while updating language");
                    stringBuilder.append(UTKit.EOL);
                    stringBuilder.append(UTKit.listMenus(sessionLanguage, nexMenus));
                    stringBuilder.append(UTKit.EOL);
                    stringBuilder.append(EnumFormatter.format(Language.class));
                }
            } else {
                hasError = true;
                stringBuilder.append("Invalid Language");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(UTKit.setTitle(sessionLanguage, currentMenu.getParentMenu()));
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(EnumFormatter.format(Language.class));
            }
        } else if (selectedQuestion == Question.DEPARTURE) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            nexMenus = menuService.getNextMenus(currentMenu);
            LOGGER.info("Departure request.getInput {}", request.getInput());
            BusResponseObject stopResponseObject = bookingService.getStopByName(request.getInput());
            if (Boolean.FALSE.equals(stopResponseObject.getStatus())) {
                stringBuilder.append(UTKit.listMenus(sessionLanguage, nexMenus));
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(stopResponseObject.getMessage());
            } else {
                hasError = true;
                stringBuilder.append("departure location not found");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(UTKit.setTitle(sessionLanguage, currentMenu));
            }
            // Get busStops based on selected input
        } else if (selectedQuestion == Question.CONFIRM_DEPARTURE) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            nexMenus = menuService.getNextMenus(currentMenu);
            String previousInput = UTKit.getLastInput(session.getLastInput());
            BusResponseObject stopResponseObject = bookingService.validateSelectedBusStop(request.getInput(), previousInput);
            LOGGER.info("Confirm departure {}", stopResponseObject);
            if (Boolean.FALSE.equals(stopResponseObject.getStatus())) {
                lastInput = UTKit.replaceLastInput(session.getLastInput(), stopResponseObject.getMessage());
                stringBuilder.append(UTKit.listMenus(sessionLanguage, nexMenus));
            } else {
                stopResponseObject = bookingService.getStopByName(previousInput);
                hasError = true;
                stringBuilder.append("Invalid departure");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(UTKit.setTitle(sessionLanguage, currentMenu));
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(stopResponseObject.getMessage());
            }
        } else if (selectedQuestion == Question.DESTINATION) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            nexMenus = menuService.getNextMenus(currentMenu);
            BusResponseObject stopResponseObject = bookingService.getStopByName(request.getInput());
            if (Boolean.FALSE.equals(stopResponseObject.getStatus())) {
                stringBuilder.append(UTKit.listMenus(sessionLanguage, nexMenus));
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(stopResponseObject.getMessage());
            } else {
                hasError = true;
                stringBuilder.append("Destination location found");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(UTKit.setTitle(sessionLanguage, currentMenu));
            }
            // Get busStops based on selected input
        } else if (selectedQuestion == Question.CONFIRM_DESTINATION) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            nexMenus = menuService.getNextMenus(currentMenu);
            String previousInput = UTKit.getLastInput(session.getLastInput());
            BusResponseObject stopResponseObject = bookingService.validateSelectedBusStop(request.getInput(), previousInput);
            if (Boolean.FALSE.equals(stopResponseObject.getStatus())) {
                lastInput = UTKit.replaceLastInput(session.getLastInput(), stopResponseObject.getMessage());
                stringBuilder.append(UTKit.listMenus(sessionLanguage, nexMenus));
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(UTKit.getDepartureDate(sessionLanguage));
            } else {
                stopResponseObject = bookingService.getStopByName(previousInput);
                hasError = true;
                stringBuilder.append("Invalid destination");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(UTKit.setTitle(sessionLanguage, currentMenu));
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(stopResponseObject.getMessage());
            }
        } else if (selectedQuestion == Question.DEPARTURE_DATE) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            nexMenus = menuService.getNextMenus(currentMenu);
            if (Boolean.TRUE.equals(UTKit.validateDepartureDate(request.getInput()))) {
                stringBuilder.append(UTKit.listMenus(sessionLanguage, nexMenus));
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(UTKit.getTimeOfTheDay(sessionLanguage));
            } else {
                hasError = true;
                stringBuilder.append("Invalid departure date");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(UTKit.setTitle(sessionLanguage, currentMenu));
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(UTKit.getDepartureDate(sessionLanguage));
            }
        } else if (selectedQuestion == Question.TIME_OF_DAY) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            nexMenus = menuService.getNextMenus(currentMenu);
            if (Boolean.TRUE.equals(UTKit.validateTimeOfTheDay(request.getInput()))) {
                String departureDate = UTKit.getLastInput(session.getLastInput());
                stringBuilder.append(UTKit.listMenus(sessionLanguage, nexMenus));
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(UTKit.showDepartureTime(departureDate, request.getInput()));
            } else {
                hasError = true;
                stringBuilder.append("Invalid time of day");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(UTKit.setTitle(sessionLanguage, currentMenu));
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(UTKit.getTimeOfTheDay(sessionLanguage));
            }
        } else if (selectedQuestion == Question.DEPARTURE_TIME) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            nexMenus = menuService.getNextMenus(currentMenu);
            String[] sessionInputs = session.getLastInput().split(UTKit.JOINER);
            String departureDate = sessionInputs[sessionInputs.length - 2];
            String timeOfTheDay = sessionInputs[sessionInputs.length - 1];
            BusResponseObject departureTimeResponseObject = bookingService.validateDepartureTime(request.getInput(), departureDate, timeOfTheDay);
            if (Boolean.FALSE.equals(departureTimeResponseObject.getStatus())) {
                try {
                    String selectedTime = departureTimeResponseObject.getMessage();
                    lastInput = session.getLastInput() + UTKit.JOINER + selectedTime;
                    String[] inputs = session.getLastInput().split(UTKit.JOINER);
                    String cityIn = inputs[inputs.length - 4];
                    String cityOut = inputs[inputs.length - 3];
                    LOGGER.info("location cityIn {} cityOut {} selectedTime {}", cityIn, cityOut, selectedTime);
                    BusTime busTime = new BusTime(selectedTime);
                    BusListRequest busListRequest = new BusListRequest(cityIn, cityOut, busTime.getStartDate(), busTime.getStartTime());
                    LOGGER.info("busListRequest {}", busListRequest);
                    BusListSuccess listSuccess = bookingService.getBusLists(busListRequest);
                    BusResponseObject responseObject = bookingService.showAvailableBuses(listSuccess);
                    if (Boolean.FALSE.equals(responseObject.getStatus())) {
                        stringBuilder.append(UTKit.listMenus(sessionLanguage, nexMenus));
                        stringBuilder.append(UTKit.EOL);
                        stringBuilder.append(responseObject.getMessage());
                    } else {
                        hasError = true;
                        stringBuilder.append(responseObject.getMessage());
                        stringBuilder.append(UTKit.EOL);
                        stringBuilder.append(UTKit.setTitle(sessionLanguage, currentMenu));
                        stringBuilder.append(UTKit.EOL);
                        stringBuilder.append(UTKit.showDepartureTime(departureDate,timeOfTheDay));
                    }
                } catch (Exception ex) {
                    LOGGER.error(ex.getCause().getMessage());
                    hasError = true;
                    stringBuilder.append("Error while getting available buses");
                    stringBuilder.append(UTKit.EOL);
                    stringBuilder.append(UTKit.setTitle(sessionLanguage, currentMenu));
                    stringBuilder.append(UTKit.EOL);
                    stringBuilder.append(UTKit.showDepartureTime(departureDate,timeOfTheDay));
                }
            } else {
                hasError = true;
                stringBuilder.append("Invalid time selected");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(UTKit.setTitle(sessionLanguage, currentMenu));
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(UTKit.showDepartureTime(departureDate,timeOfTheDay));
            }
        } else if (selectedQuestion == Question.SHOW_AVAILABLE_BUSES) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            nexMenus = menuService.getNextMenus(currentMenu);
            String[] sessionInputs = session.getLastInput().split(UTKit.JOINER);
            String cityIn = sessionInputs[2];
            String cityOut = sessionInputs[3];
            String departureTime = sessionInputs[6];
            String startDate = departureTime.split(UTKit.BLANK)[0];
            String startTime = departureTime.split(UTKit.BLANK)[1];
            BusListRequest listRequest = new BusListRequest(cityIn, cityOut, startDate, startTime);
            BusListSuccess listSuccess = bookingService.getBusLists(listRequest);
            BusResponseObject availableBusResponse = bookingService.validateBusList(request.getInput(), listSuccess);
            if (Boolean.FALSE.equals(availableBusResponse.getStatus())) {
                String companyName = listSuccess.getResult().get(Integer.parseInt(request.getInput()) - 1).getCompanyName();
                String busName = listSuccess.getResult().get(Integer.parseInt(request.getInput()) - 1).getName().replace(UTKit.JOINER, UTKit.EMPTY).replace(UTKit.BLANK + UTKit.BLANK, UTKit.BLANK);
                String amount = listSuccess.getResult().get(Integer.parseInt(request.getInput()) - 1).getTotalAmount();
                String currency = listSuccess.getResult().get(Integer.parseInt(request.getInput()) - 1).getCurrency();
                lastInput = session.getLastInput() + UTKit.JOINER + companyName + UTKit.BLANK + busName + UTKit.BLANK + amount + currency;
                stringBuilder.append(UTKit.listMenus(sessionLanguage, nexMenus));
            } else {
                hasError = true;
                stringBuilder.append(availableBusResponse.getMessage());
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(UTKit.setTitle(sessionLanguage, currentMenu));
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(bookingService.showAvailableBuses(listSuccess).getMessage());
            }
        } else if (selectedQuestion == Question.ENTER_BUS_CARD) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            nexMenus = menuService.getNextMenus(currentMenu);
            BusResponseObject cardValidation = bookingService.validateBusCard(request.getInput());
            if (Boolean.FALSE.equals(cardValidation.getStatus())) {
                String previousInput = UTKit.getLastInput(session.getLastInput());
                stringBuilder.append(UTKit.listMenus(sessionLanguage, nexMenus));
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(previousInput.replace(UTKit.BLANK, UTKit.EOL));
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(request.getInput());
                leaf = nexMenus.get(0).getLeaf();
            } else {
                leaf = false;
                hasError = true;
                stringBuilder.append(cardValidation.getMessage());
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(UTKit.setTitle(sessionLanguage, currentMenu));
            }
        } else {
            LOGGER.info("=================== access last else ===================");
            selectedQuestion = menus.get(0).getQuestion();
            currentMenu = menuService.getByQuestion(selectedQuestion);
            nexMenus = menuService.getNextMenus(currentMenu);
            stringBuilder.append(UTKit.listMenus(sessionLanguage, nexMenus));
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
