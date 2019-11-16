package com.goltd.agrigoussd.questionnaire.processors;

import com.goltd.agrigoussd.domain.Session;
import com.goltd.agrigoussd.domain.UssdMenu;
import com.goltd.agrigoussd.helpers.UTKit;
import com.goltd.agrigoussd.helpers.UssdRequest;
import com.goltd.agrigoussd.helpers.enums.Question;
import com.goltd.agrigoussd.questionnaire.validators.RegistrationValidator;
import com.goltd.agrigoussd.service.interfaces.IMenuService;
import com.goltd.agrigoussd.service.interfaces.ISessionService;
import com.goltd.agrigoussd.service.interfaces.IUserService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;

import static com.goltd.agrigoussd.helpers.UTKit.JOINER;
import static com.goltd.agrigoussd.helpers.enums.Question.MAIN_SELECT_SERVICE;

@Service
public class MainQuestionnaire implements IAbstractQuestionnaireProcessor {

    private static final Logger logger = LoggerFactory.getLogger(MainQuestionnaire.class);

    private IMenuService menuService;
    private ISessionService sessionService;
    private IUserService userService;

    @Autowired
    public MainQuestionnaire(IMenuService menuService, ISessionService sessionService, IUserService userService) {
        this.menuService = menuService;
        this.sessionService = sessionService;
        this.userService = userService;

    }

    @Override
    public StringBuilder buildMenu(Session session, UssdRequest request) {
        Question previousQuestion = session.getQuestion();
        UssdMenu previousMenu = menuService.getByQuestion(previousQuestion);
        List<UssdMenu> nextMenus = menuService.getByParentId(previousMenu);
        UssdMenu nextMenu = nextMenus.get(0);
        String ussdHeader = nextMenu.getTitleKin();
        StringBuilder ussdMessage = new StringBuilder();
        switch (previousQuestion) {
            case MAIN_ENTER_PIN:

                if (userService.isValidPin(request.getMsisdn(), request.getInput())) {
                    String services = UTKit.listMenus(menuService.getByParentId(nextMenu));
                    ussdMessage.append(ussdHeader);
                    ussdMessage.append(UTKit.EOL);
                    ussdMessage.append(services);

                    // save session
                    session.setPreviousQuestion(previousMenu.getQuestion());
                    session.setQuestion(MAIN_SELECT_SERVICE);
                    session.setTransactionDatetime(new Date());
                    session.setLeaf(nextMenu.getLeaf());
                    session.setLastInput(session.getLastInput() + JOINER + request.getInput());
                    sessionService.update(session);
                } else {
                    ussdMessage.append(previousMenu.getTitleKin());
                }
                break;

            case MAIN_SELECT_SERVICE:
                // 1. Get services menus where MAIN_SELECT_SERVICE is parent
                List<UssdMenu> previousMenus = menuService.getChildrenByQuestion(previousQuestion);
                if(RegistrationValidator.validateMenus(request.getInput(),previousMenus)){
                    nextMenu = previousMenus.get(Integer.parseInt(request.getInput()));

                    ussdMessage.append(nextMenu.getTitleKin());
                }

                // logger.info("Children by ParentQuestion {}",services);
                // 2. Check if input is valid number and in services menus if not show services
                // 3. Get selected service for next question
                // 4. Get next selected menu
                // 5.
                break;
            case MAIN_ASSOCIATIONS:
                ussdMessage.append(nextMenu.getTitleKin());
                break;
            case MAIN_LAND_MANAGEMENT:
                ussdMessage.append(nextMenu.getTitleKin());
                break;
            case MAIN_ACTIVITY_RECORDING:
                ussdMessage.append(nextMenu.getTitleKin());
                break;
            case MAIN_MINI_REPORT:
                ussdMessage.append(nextMenu.getTitleKin());
                break;
            case MAIN_MARKETPLACE:
                ussdMessage.append(nextMenu.getTitleKin());
                break;
            case MAIN_BUY_AIRTIME:
                ussdMessage.append(nextMenu.getTitleKin());
                break;
            case MAIN_MY_ACCOUNT:
                ussdMessage.append(nextMenu.getTitleKin());
                break;
            case MAIN_HELP:
                ussdMessage.append(nextMenu.getTitleKin());
                break;
            default:
                ussdMessage.append(ussdHeader);
                break;
        }
        logger.info("MainQuestionnaire : {}", ussdMessage);
        return ussdMessage;
    }
}
