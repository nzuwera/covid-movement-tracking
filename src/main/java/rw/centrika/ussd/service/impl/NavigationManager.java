package rw.centrika.ussd.service.impl;

import rw.centrika.ussd.domain.Location;
import rw.centrika.ussd.domain.Session;
import rw.centrika.ussd.domain.UserAccount;
import rw.centrika.ussd.domain.UssdMenu;
import rw.centrika.ussd.helpers.ResponseObject;
import rw.centrika.ussd.helpers.UTKit;
import rw.centrika.ussd.helpers.UssdRequest;
import rw.centrika.ussd.helpers.UssdResponse;
import rw.centrika.ussd.helpers.enums.Gender;
import rw.centrika.ussd.helpers.enums.Question;
import rw.centrika.ussd.helpers.enums.Questionnaire;
import rw.centrika.ussd.helpers.enums.Visibility;
import rw.centrika.ussd.helpers.formatter.EnumFormatter;
import rw.centrika.ussd.helpers.formatter.ListFormatter;
import com.goltd.agrigoussd.service.interfaces.*;
import rw.centrika.ussd.service.interfaces.*;
import rw.centrika.ussd.validators.QuestionValidator;
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
    private LandService landService;
    private ActivityService activityService;

    @Autowired
    public NavigationManager(ActivityService activityService, ISessionService sessionService, IMenuService menuService, ILocationService locationService, IUserService userService, AssociationService associationService, LandService landService) {
        this.sessionService = sessionService;
        this.menuService = menuService;
        this.locationService = locationService;
        this.userService = userService;
        this.associationService = associationService;
        this.landService = landService;
        this.activityService = activityService;
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
        UssdMenu currentMenu;
        List<UssdMenu> siblings;
        List<UssdMenu> nexMenus;
        StringBuilder stringBuilder = new StringBuilder();
        Boolean hasError = false;
        String currentLocationCode = "RWA";
        String nextLocationCode = "";
        List<Location> locations;
        String lastInput = (session.getLastInput().equals(SHORT_CODE) && request.getNewRequest().equals("1") ? session.getLastInput() : session.getLastInput() + UTKit.JOINER + request.getInput());
        leaf = menus.get(0).getLeaf();
        Questionnaire questionnaire = menus.get(0).getQuestionnaire();
        previousQuestion = menus.get(0).getParentMenu().getQuestion();
        selectedQuestion = menus.get(0).getQuestion();
        if (selectedQuestion == Question.MAIN_SELECT_SERVICE) {
            // validate pin
            if (userService.isValidPin(request.getMsisdn(), request.getInput())) {
                List<UssdMenu> children = menuService.getNextMenus(selectedQuestion);
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
                || selectedQuestion == Question.ASSOCIATION_MANAGEMENT_JOIN
                || selectedQuestion == Question.ACCOUNT_RESET_PIN) {

            currentMenu = menuService.getByQuestion(selectedQuestion);
            siblings = menuService.getNextMenus(currentMenu.getParentMenu());
            // validate user choice
            if (QuestionValidator.validateMenus(request.getInput(), siblings)) {
                selectedQuestion = siblings.get(Integer.parseInt(request.getInput()) - 1).getQuestion();
                previousQuestion = siblings.get(Integer.parseInt(request.getInput()) - 1).getParentMenu().getQuestion();
                List<UssdMenu> selectedMenus = menuService.getNextMenus(selectedQuestion);
                leaf = selectedMenus.get(0).getLeaf();
                // if question = association show association list
                // else if question = land show land list
                if (selectedQuestion == Question.ASSOCIATION_MANAGEMENT_VIEW
                        || selectedQuestion == Question.ASSOCIATION_MANAGEMENT_LEAVE) {
                    stringBuilder.append(UTKit.listMenus(selectedMenus));
                    stringBuilder.append(UTKit.EOL);
                    stringBuilder.append(associationService.showAssociation());
                } else if (selectedQuestion == Question.LAND_MANAGEMENT_CROP_ALLOCATION
                        || selectedQuestion == Question.LAND_MANAGEMENT_VIEW_PLOTS) {
                    stringBuilder.append(UTKit.listMenus(selectedMenus));
                    stringBuilder.append(UTKit.EOL);
                    stringBuilder.append(landService.formatStringList(landService.getLands()));
                } else if (selectedQuestion == Question.MAIN_MENU_ACTIVITY_RECORDING) {
                    stringBuilder.append(UTKit.listMenus(selectedMenus));
                    stringBuilder.append(UTKit.EOL);
                    stringBuilder.append(activityService.showCategories());
                } else if (selectedQuestion == Question.REPORT_MANAGEMENT_LAST_RECORDED_ACTIVITY) {
                    stringBuilder.append(UTKit.listMenus(selectedMenus));
                } else if (selectedQuestion == Question.REPORT_MANAGEMENT_PREDICTED_HARVEST) {
                    stringBuilder.append(UTKit.listMenus(selectedMenus));
                } else if (selectedQuestion == Question.MAIN_MENU_MARKETPLACE) {
                    leaf = true;
                    stringBuilder.append("Coming soon");
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
                stringBuilder.append(currentMenu.getTitleKin());
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(associationService.showAssociation());
            }
        } else if (selectedQuestion == Question.REGISTRATION_ENTER_AGE) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            if (QuestionValidator.validateStringWord(request.getInput())) {
                selectedQuestion = menus.get(0).getQuestion();
                leaf = menus.get(0).getLeaf();
                stringBuilder.append(currentMenu.getTitleKin());
            } else {
                hasError = true;
                stringBuilder.append("Name must be only letters");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(currentMenu.getParentMenu().getTitleKin());
            }
        } else if (selectedQuestion == Question.REGISTRATION_SELECT_GENDER) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            if (QuestionValidator.validateAge(request.getInput())) {
                selectedQuestion = menus.get(0).getQuestion();
                leaf = menus.get(0).getLeaf();
                stringBuilder.append(currentMenu.getTitleKin());
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(EnumFormatter.format(Gender.class));
            } else {
                hasError = true;
                stringBuilder.append("Invalid age");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(currentMenu.getParentMenu().getTitleKin());
            }
        } else if (selectedQuestion == Question.REGISTRATION_SELECT_LOCATION_PROVINCE) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            if (QuestionValidator.validateGender(request.getInput())) {
                stringBuilder.append(currentMenu.getTitleKin());
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(ListFormatter.formatLocations(locationService.getlocationsByParentCode(currentLocationCode)));
            } else {
                hasError = true;
                stringBuilder.append("Incorrect gender selected");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(currentMenu.getParentMenu().getTitleKin());
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(EnumFormatter.format(Gender.class));
            }
        } else if (selectedQuestion == Question.REGISTRATION_SELECT_LOCATION_DISTRICT
                || selectedQuestion == Question.REGISTRATION_SELECT_LOCATION_SECTOR
                || selectedQuestion == Question.REGISTRATION_SELECT_LOCATION_CELL
                || selectedQuestion == Question.REGISTRATION_SELECT_LOCATION_VILLAGE) {
            currentMenu = menuService.getByQuestion(selectedQuestion);

            if (selectedQuestion != Question.REGISTRATION_SELECT_LOCATION_DISTRICT) { //
                currentLocationCode = UTKit.getLastInput(session.getLastInput());
            }

            locations = locationService.getlocationsByParentCode(currentLocationCode);

            if (QuestionValidator.validateLocations(request.getInput(), locations)) {
                nextLocationCode = locations.get(Integer.parseInt(request.getInput()) - 1).getCode();
                lastInput = session.getLastInput() + UTKit.JOINER + nextLocationCode;
                stringBuilder.append(currentMenu.getTitleKin());
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(ListFormatter.formatLocations(locationService.getlocationsByParentCode(nextLocationCode)));
            } else {
                hasError = true;
                stringBuilder.append("Invalid location selected");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(currentMenu.getParentMenu().getTitleKin());
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(ListFormatter.formatLocations(locationService.getlocationsByParentCode(currentLocationCode)));
            }
        } else if (selectedQuestion == Question.REGISTRATION_ENTER_PIN) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            String cellCode = UTKit.getLastInput(session.getLastInput());

            List<Location> villages = locationService.getlocationsByParentCode(cellCode);

            if (QuestionValidator.validateLocations(request.getInput(), villages)) {
                String villageCode = villages.get(Integer.parseInt(request.getInput()) - 1).getCode();
                lastInput = session.getLastInput() + UTKit.JOINER + villageCode;
                selectedQuestion = menus.get(0).getQuestion();
                leaf = currentMenu.getLeaf();
                stringBuilder.append(currentMenu.getTitleKin());
            } else {
                hasError = true;
                stringBuilder.append("Invalid Village");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(currentMenu.getParentMenu().getTitleKin());
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(ListFormatter.formatLocations(locationService.getlocationsByParentCode(cellCode)));
            }
        } else if (selectedQuestion == Question.REGISTRATION_VERIFY_PIN) {
            currentMenu = menuService.getByQuestion(selectedQuestion);

            if (QuestionValidator.validatePinFormat(request.getInput())) {
                lastInput = session.getLastInput() + UTKit.JOINER + request.getInput();
                selectedQuestion = menus.get(0).getQuestion();
                leaf = currentMenu.getLeaf();
                stringBuilder.append(currentMenu.getTitleKin());
            } else {
                hasError = true;
                stringBuilder.append("Invalid PIN format");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(currentMenu.getParentMenu().getTitleKin());
            }
        } else if (selectedQuestion == Question.REGISTRATION_COMPLETED) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            String pin = UTKit.getLastInput(session.getLastInput());
            String verifiedPin = request.getInput();

            if (QuestionValidator.validatePinFormat(verifiedPin) && pin.equals(verifiedPin)) {
                lastInput = session.getLastInput() + UTKit.JOINER + request.getInput();
                selectedQuestion = menus.get(0).getQuestion();
                leaf = currentMenu.getLeaf();

                // UserAccount
                UserAccount userAccount = new UserAccount(request.getMsisdn(), lastInput);
                userService.create(userAccount);
                stringBuilder.append(currentMenu.getTitleKin());
            } else {
                hasError = true;
                leaf = false;
                stringBuilder.append("Invalid PIN format or PIN don't match");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(currentMenu.getParentMenu().getTitleKin());
            }
        } else if (selectedQuestion == Question.LAND_PLOT_SIZE) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            previousQuestion = currentMenu.getParentMenu().getQuestion();
            nexMenus = menuService.getNextMenus(currentMenu);
            // validate plot size
            if (QuestionValidator.validateNumericalString(request.getInput()) && Integer.parseInt(request.getInput()) > 0) {
                stringBuilder.append(UTKit.listMenus(nexMenus));
            } else {
                hasError = true;
                leaf = false;
                stringBuilder.append("Plot size must great than 0");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(currentMenu.getParentMenu().getTitleKin());
            }
        } else if (selectedQuestion == Question.LAND_PLOT_UPI) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            previousQuestion = currentMenu.getParentMenu().getQuestion();
            nexMenus = menuService.getNextMenus(currentMenu);
            // validate plot size
            if (QuestionValidator.validateUPIFormat(request.getInput())) {
                locations = locationService.getlocationsByParentCode(currentLocationCode);
                stringBuilder.append(UTKit.listMenus(nexMenus));
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(ListFormatter.formatLocations(locations));
            } else {
                hasError = true;
                leaf = false;
                stringBuilder.append("Invalid UPI,... x/xx/xx/xx/xxxx");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(currentMenu.getParentMenu().getTitleKin());
            }
        } else if (selectedQuestion == Question.LAND_LOCATION_PROVINCE
                || selectedQuestion == Question.LAND_LOCATION_DISTRICT
                || selectedQuestion == Question.LAND_LOCATION_SECTOR
                || selectedQuestion == Question.LAND_LOCATION_CELL) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            previousQuestion = currentMenu.getParentMenu().getQuestion();
            nexMenus = menuService.getNextMenus(currentMenu);
            if (selectedQuestion != Question.LAND_LOCATION_PROVINCE) {
                currentLocationCode = UTKit.getLastInput(session.getLastInput());
            }
            locations = locationService.getlocationsByParentCode(currentLocationCode);
            // validate plot size
            if (QuestionValidator.validateLocations(request.getInput(), locations)) {
                nextLocationCode = locations.get(Integer.parseInt(request.getInput()) - 1).getCode();
                lastInput = session.getLastInput() + UTKit.JOINER + nextLocationCode;
                leaf = nexMenus.get(0).getLeaf();
                stringBuilder.append(UTKit.listMenus(nexMenus));
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(ListFormatter.formatLocations(locationService.getlocationsByParentCode(nextLocationCode)));
            } else {
                hasError = true;
                leaf = false;
                stringBuilder.append("Invalid location");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(currentMenu.getParentMenu().getTitleKin());
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(locationService.getlocationsByParentCode(currentLocationCode));
            }
        } else if (selectedQuestion == Question.LAND_LOCATION_VILLAGE) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            currentLocationCode = UTKit.getLastInput(session.getLastInput());

            locations = locationService.getlocationsByParentCode(currentLocationCode);

            if (QuestionValidator.validateLocations(request.getInput(), locations)) {
                nexMenus = menuService.getNextMenus(currentMenu);
                nextLocationCode = locations.get(Integer.parseInt(request.getInput()) - 1).getCode();
                lastInput = session.getLastInput() + UTKit.JOINER + nextLocationCode;
                selectedQuestion = nexMenus.get(0).getQuestion();
                leaf = nexMenus.get(0).getLeaf();
                // call API register land and handle network related issues
                stringBuilder.append(UTKit.listMenus(nexMenus));
            } else {
                hasError = true;
                stringBuilder.append("Invalid location");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(currentMenu.getParentMenu().getTitleKin());
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(ListFormatter.formatLocations(locationService.getlocationsByParentCode(currentLocationCode)));
            }
        } else if (selectedQuestion == Question.LAND_CROP_SELECT_PLOT) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            nexMenus = menuService.getNextMenus(currentMenu);
            if (landService.isValideLand(request.getInput(), landService.getLands())) {
                stringBuilder.append(UTKit.listMenus(nexMenus));
            } else {
                hasError = true;
                stringBuilder.append("Invalid Plot");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(currentMenu.getTitleKin());
            }
        } else if (selectedQuestion == Question.LAND_CROP_TYPE_OF_CROP
                || selectedQuestion == Question.LAND_CROP_NAME_OF_CROP
                || selectedQuestion == Question.LAND_CROP_SEED) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            nexMenus = menuService.getNextMenus(currentMenu);
            if (QuestionValidator.validateStringWord(request.getInput())) {
                leaf = nexMenus.get(0).getLeaf();
                stringBuilder.append(UTKit.listMenus(nexMenus));
            } else {
                hasError = true;
                stringBuilder.append("Invalid input, only string allowed");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(currentMenu.getTitleKin());
            }
        } else if (selectedQuestion == Question.ACTIVITY_SHOW_CATEGORY) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            nexMenus = menuService.getNextMenus(currentMenu);
            if (activityService.isValideCategory(request.getInput())) {
                stringBuilder.append(UTKit.listMenus(nexMenus));
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(activityService.showActivity(request.getInput()));
            } else {
                hasError = true;
                stringBuilder.append("Invalid Activity category");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(currentMenu.getTitleKin());
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(activityService.showCategories());
            }
        } else if (selectedQuestion == Question.ACTIVITY_SHOW_LIST) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            nexMenus = menuService.getNextMenus(currentMenu);
            LOGGER.info("activity_show_list lastinput {}", UTKit.getLastInput(session.getLastInput()));
            if (activityService.isValidaActivity(UTKit.getLastInput(session.getLastInput()), request.getInput())) {
                stringBuilder.append(UTKit.listMenus(nexMenus));
            } else {
                hasError = true;
                stringBuilder.append("Invalid Activity List");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(currentMenu.getTitleKin());
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(activityService.showActivity(UTKit.getLastInput(session.getLastInput())));
            }
        } else if (selectedQuestion == Question.ACTIVITY_QUANTITY
                || selectedQuestion == Question.ACTIVITY_COST_PER_UNIT
                || selectedQuestion == Question.AIRTIME_AMOUNT) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            nexMenus = menuService.getNextMenus(currentMenu);
            if (QuestionValidator.validateNumericalString(request.getInput())) {
                leaf = nexMenus.get(0).getLeaf();
                stringBuilder.append(UTKit.listMenus(nexMenus));
            } else {
                hasError = true;
                stringBuilder.append("Invalid input, only numeric allowed");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(currentMenu.getTitleKin());
            }
        } else if (selectedQuestion == Question.AIRTIME_BUYER_MSISDN) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            nexMenus = menuService.getNextMenus(currentMenu);
            if (QuestionValidator.validateMtnPhone(request.getInput())) {
                leaf = nexMenus.get(0).getLeaf();
                stringBuilder.append(UTKit.listMenus(nexMenus));
            } else {
                hasError = true;
                stringBuilder.append("Invalid phone number... 078XXXXXXX");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(currentMenu.getTitleKin());
            }
        } else if (selectedQuestion == Question.ACCOUNT_CURRENT_PIN) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            nexMenus = menuService.getNextMenus(currentMenu);
            if (userService.isValidPin(request.getMsisdn(), request.getInput())) {
                leaf = nexMenus.get(0).getLeaf();
                stringBuilder.append(UTKit.listMenus(nexMenus));
            } else {
                hasError = true;
                stringBuilder.append("Invalid current pin");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(currentMenu.getTitleKin());
            }
        } else if (selectedQuestion == Question.ACCOUNT_NEW_PIN) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            nexMenus = menuService.getNextMenus(currentMenu);
            if (QuestionValidator.validatePinFormat(request.getInput())) {
                lastInput = session.getLastInput() + UTKit.JOINER + request.getInput();
                selectedQuestion = menus.get(0).getQuestion();
                leaf = currentMenu.getLeaf();
                stringBuilder.append(UTKit.listMenus(nexMenus));
            } else {
                hasError = true;
                stringBuilder.append("Invalid PIN format");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(currentMenu.getParentMenu().getTitleKin());
            }
        } else if (selectedQuestion == Question.ACCOUNT_REPEAT_NEW_PIN) {
            currentMenu = menuService.getByQuestion(selectedQuestion);
            nexMenus = menuService.getNextMenus(currentMenu);
            String newPin = UTKit.getLastInput(session.getLastInput());
            String verifiedPin = request.getInput();
            if (QuestionValidator.validatePinFormat(request.getInput()) && newPin.equals(verifiedPin)) {
                try {
                    // updatePin
                    userService.updatePin(request.getMsisdn(),verifiedPin);
                    leaf = nexMenus.get(0).getLeaf();
                    stringBuilder.append(UTKit.listMenus(nexMenus));
                } catch (Exception ex) {
                    hasError = true;
                    LOGGER.error(ex.getMessage());
                    stringBuilder.append("Error happened while updating user pin");
                    stringBuilder.append(UTKit.EOL);
                    stringBuilder.append(currentMenu.getTitleKin());
                }
            } else {
                hasError = true;
                stringBuilder.append("Invalid pin format or Pin don't matches");
                stringBuilder.append(UTKit.EOL);
                stringBuilder.append(currentMenu.getTitleKin());
            }
        } else {
            LOGGER.info("=================== access last else ===================");
            selectedQuestion = menus.get(0).getQuestion();
            leaf = menus.get(0).getLeaf();
            stringBuilder.append(UTKit.listMenus(menus));
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
