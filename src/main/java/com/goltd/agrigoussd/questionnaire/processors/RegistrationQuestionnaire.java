package com.goltd.agrigoussd.questionnaire.processors;

import com.goltd.agrigoussd.domain.Location;
import com.goltd.agrigoussd.domain.Session;
import com.goltd.agrigoussd.domain.UserAccount;
import com.goltd.agrigoussd.domain.UssdMenu;
import com.goltd.agrigoussd.helpers.UssdRequest;
import com.goltd.agrigoussd.helpers.enums.AccountState;
import com.goltd.agrigoussd.helpers.enums.Gender;
import com.goltd.agrigoussd.helpers.enums.Question;
import com.goltd.agrigoussd.helpers.formatter.EnumFormatter;
import com.goltd.agrigoussd.helpers.formatter.ListFormatter;
import com.goltd.agrigoussd.questionnaire.validators.QuestionValidator;
import com.goltd.agrigoussd.service.interfaces.ILocationService;
import com.goltd.agrigoussd.service.interfaces.IMenuService;
import com.goltd.agrigoussd.service.interfaces.ISessionService;
import com.goltd.agrigoussd.service.interfaces.IUserService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;
import java.util.UUID;

import static com.goltd.agrigoussd.helpers.UTKit.JOINER;
import static com.goltd.agrigoussd.helpers.UTKit.getLastInput;

@Service
public class RegistrationQuestionnaire implements IAbstractQuestionnaireProcessor {

    private static final Logger logger = LoggerFactory.getLogger(RegistrationQuestionnaire.class);

    private IMenuService menuService;
    private ILocationService locationService;
    private ISessionService sessionService;
    private IUserService userService;

    @Autowired
    public RegistrationQuestionnaire(IMenuService menuService, ILocationService locationService, ISessionService sessionService, IUserService userService) {
        this.menuService = menuService;
        this.locationService = locationService;
        this.sessionService = sessionService;
        this.userService = userService;

    }

    @Override
    public StringBuilder buildMenu(Session session, UssdRequest request) {
        Question previousQuestion = session.getQuestion();
        UssdMenu previousMenu = menuService.getByQuestion(previousQuestion);
        List<UssdMenu> nextMenus = menuService.getByParentId(previousMenu);
        Question nextQuestion = nextMenus.get(0).getQuestion();
        UssdMenu nextMenu = nextMenus.get(0);
        String ussdHeader = nextMenu.getTitleKin();
        StringBuilder ussdMessage = new StringBuilder();
        switch (session.getQuestion()) {
            case REGISTRATION_ENTER_FULL_NAME:
                if (QuestionValidator.validateFullName(request.getInput())) {
                    // Display enter age
                    session.setLastInput(session.getLastInput() + JOINER + request.getInput());
                    ussdMessage.append(nextMenus.get(0).getTitleKin());

                    // save session
                    session.setPreviousQuestion(previousMenu.getQuestion());
                    session.setQuestion(nextQuestion);
                    session.setTransactionDatetime(new Date());
                    session.setLeaf(nextMenu.getLeaf());
                    sessionService.update(session);
                } else {
                    ussdMessage.append(previousMenu.getTitleKin());
                }
                break;

            case REGISTRATION_ENTER_AGE:
                if (QuestionValidator.validateAge(request.getInput())) {
                    // Display gender
                    session.setLastInput(session.getLastInput() + JOINER + request.getInput());
                    ussdMessage.append(EnumFormatter.format(nextMenus.get(0).getTitleKin(), Gender.class));


                    // save session
                    session.setPreviousQuestion(previousMenu.getQuestion());
                    session.setQuestion(nextQuestion);
                    session.setTransactionDatetime(new Date());
                    session.setLeaf(nextMenu.getLeaf());
                    sessionService.update(session);
                } else {
                    ussdMessage.append(previousMenu.getTitleKin());
                }
                break;

            case REGISTRATION_SELECT_GENDER:
                if (QuestionValidator.validateGender(request.getInput())) {
                    // Display provinces
                    session.setLastInput(session.getLastInput() + JOINER + request.getInput());
                    ussdMessage = ListFormatter.formatLocations(ussdHeader, locationService.getProvinces());


                    // save session
                    session.setPreviousQuestion(previousMenu.getQuestion());
                    session.setQuestion(nextQuestion);
                    session.setTransactionDatetime(new Date());
                    session.setLeaf(nextMenu.getLeaf());
                    sessionService.update(session);
                } else {
                    ussdMessage.append(EnumFormatter.format(previousMenu.getTitleKin(), Gender.class));
                }
                break;
            case REGISTRATION_SELECT_LOCATION_PROVINCE:
                // Get selected province
                List<Location> prevProvinces = locationService.getProvinces();
                if (QuestionValidator.validateLocations(request.getInput(), prevProvinces)) {
                    String provinceCode = prevProvinces.get(Integer.parseInt(request.getInput()) - 1).getCode();
                    session.setLastInput(session.getLastInput() + JOINER + provinceCode);
                    // Display districts
                    ussdMessage = ListFormatter.formatLocations(ussdHeader, locationService.getDistricts(provinceCode));


                    // save session
                    session.setPreviousQuestion(previousMenu.getQuestion());
                    session.setQuestion(nextQuestion);
                    session.setTransactionDatetime(new Date());
                    session.setLeaf(nextMenu.getLeaf());
                    sessionService.update(session);
                } else {
                    ussdMessage = ListFormatter.formatLocations(previousMenu.getTitleKin(), locationService.getProvinces());
                }
                break;
            case REGISTRATION_SELECT_LOCATION_DISTRICT:
                // Get selected district
                List<Location> districts = locationService.getDistricts(getLastInput(session.getLastInput()));
                if (QuestionValidator.validateLocations(request.getInput(), districts)) {
                    String districtCode = districts.get(Integer.parseInt(request.getInput()) - 1).getCode();
                    session.setLastInput(session.getLastInput() + JOINER + districtCode);
                    // Display sectors
                    ussdMessage = ListFormatter.formatLocations(ussdHeader, locationService.getSectors(districtCode));


                    // save session
                    session.setPreviousQuestion(previousMenu.getQuestion());
                    session.setQuestion(nextQuestion);
                    session.setTransactionDatetime(new Date());
                    session.setLeaf(nextMenu.getLeaf());
                    sessionService.update(session);
                } else {
                    ussdMessage = ListFormatter.formatLocations(previousMenu.getTitleKin(), locationService.getDistricts(getLastInput(session.getLastInput())));
                }
                break;
            case REGISTRATION_SELECT_LOCATION_SECTOR:
                // Get selected sector
                List<Location> sectors = locationService.getSectors(getLastInput(session.getLastInput()));

                if (QuestionValidator.validateLocations(request.getInput(), sectors)) {
                    String sectorCode = sectors.get(Integer.parseInt(request.getInput()) - 1).getCode();
                    session.setLastInput(session.getLastInput() + JOINER + sectorCode);
                    // Display cells
                    ussdMessage = ListFormatter.formatLocations(ussdHeader, locationService.getCells(sectorCode));


                    // save session
                    session.setPreviousQuestion(previousMenu.getQuestion());
                    session.setQuestion(nextQuestion);
                    session.setTransactionDatetime(new Date());
                    session.setLeaf(nextMenu.getLeaf());
                    sessionService.update(session);
                } else {
                    ussdMessage = ListFormatter.formatLocations(previousMenu.getTitleKin(), locationService.getSectors(getLastInput(session.getLastInput())));
                }
                break;
            case REGISTRATION_SELECT_LOCATION_CELL:
                // Get selected cell
                List<Location> cells = locationService.getCells(getLastInput(session.getLastInput()));

                if (QuestionValidator.validateLocations(request.getInput(), cells)) {
                    String cellCode = cells.get(Integer.parseInt(request.getInput()) - 1).getCode();
                    session.setLastInput(session.getLastInput() + JOINER + cellCode);
                    // Display village
                    ussdMessage = ListFormatter.formatLocations(ussdHeader, locationService.getVillages(cellCode));


                    // save session
                    session.setPreviousQuestion(previousMenu.getQuestion());
                    session.setQuestion(nextQuestion);
                    session.setTransactionDatetime(new Date());
                    session.setLeaf(nextMenu.getLeaf());
                    sessionService.update(session);
                } else {
                    ussdMessage = ListFormatter.formatLocations(previousMenu.getTitleKin(), locationService.getCells(getLastInput(session.getLastInput())));
                }
                break;
            case REGISTRATION_SELECT_LOCATION_VILLAGE:
                // Get selected Village
                List<Location> villages = locationService.getVillages(getLastInput(session.getLastInput()));

                if (QuestionValidator.validateLocations(request.getInput(), villages)) {
                    String villageCode = villages.get(Integer.parseInt(request.getInput()) - 1).getCode();
                    session.setLastInput(session.getLastInput() + JOINER + villageCode);
                    // Display enter pin
                    ussdMessage.append(nextMenu.getTitleKin());


                    // save session
                    session.setPreviousQuestion(previousMenu.getQuestion());
                    session.setQuestion(nextQuestion);
                    session.setTransactionDatetime(new Date());
                    session.setLeaf(nextMenu.getLeaf());
                    sessionService.update(session);
                } else {
                    ussdMessage = ListFormatter.formatLocations(previousMenu.getTitleKin(), locationService.getVillages(getLastInput(session.getLastInput())));
                }
                break;
            case REGISTRATION_ENTER_PIN:
                // Display verify pin
                if (QuestionValidator.validatePIN(request.getInput())) {
                    ussdMessage.append(nextMenu.getTitleKin());
                    session.setLastInput(session.getLastInput() + JOINER + request.getInput());
                    // save session
                    session.setPreviousQuestion(previousMenu.getQuestion());
                    session.setQuestion(nextQuestion);
                    session.setTransactionDatetime(new Date());
                    session.setLeaf(nextMenu.getLeaf());
                    sessionService.update(session);
                } else {
                    ussdMessage.append(previousMenu.getTitleKin());
                }
                break;
            case REGISTRATION_VERIFY_PIN:
                // End Registration
                if (QuestionValidator.validatePIN(request.getInput())) {
                    String pin = getLastInput(session.getLastInput());
                    if (pin.equals(request.getInput())) {

                        session.setLastInput(session.getLastInput() + JOINER + request.getInput());
                        String[] userDetails = session.getLastInput().split(JOINER);
                        String fullName = userDetails[1];
                        String age = userDetails[2];
                        Gender gender = (userDetails[3].equals("1")) ? Gender.MALE : Gender.FEMALE;
                        String villageCode = userDetails[8];
                        UserAccount userAccount = new UserAccount();
                        userAccount.setId(UUID.randomUUID());
                        userAccount.setMsisdn(request.getMsisdn());
                        userAccount.setPin(pin);
                        userAccount.setGender(gender);
                        userAccount.setAge(Integer.parseInt(age));
                        userAccount.setFullname(fullName);
                        userAccount.setAccountState(AccountState.PENDING_SUBSCRIPTION);
                        userAccount.setVillageCode(villageCode);
                        userAccount.setExpireDate(new Date());

                        userService.create(userAccount);

                        // String errorMessage = messages[1]
                        ussdMessage.append(nextMenu.getTitleKin());
                        // save session
                        session.setPreviousQuestion(previousMenu.getQuestion());
                        session.setQuestion(nextQuestion);
                        session.setTransactionDatetime(new Date());
                        session.setLeaf(nextMenu.getLeaf());
                        sessionService.update(session);
                    } else {
                        ussdMessage.append(previousMenu.getTitleKin());
                    }
                } else {
                    ussdMessage.append(previousMenu.getTitleKin());
                }
                break;
            case REGISTRATION_COMPLETED:
                ussdMessage.append(nextMenu.getTitleKin());
                break;
            default:
                ussdMessage.append(ussdHeader);
                break;
        }
        logger.info("RegistrationQuestionnaire : {}", ussdMessage);
        return ussdMessage;
    }
}
