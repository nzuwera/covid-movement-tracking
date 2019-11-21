package com.goltd.agrigoussd.questionnaire.processors;

import com.goltd.agrigoussd.domain.Session;
import com.goltd.agrigoussd.domain.UssdMenu;
import com.goltd.agrigoussd.helpers.UssdRequest;
import com.goltd.agrigoussd.helpers.enums.Question;
import com.goltd.agrigoussd.helpers.formatter.ListFormatter;
import com.goltd.agrigoussd.questionnaire.validators.QuestionValidator;
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
        List<UssdMenu> previousMenus;
        Question previousQuestion = session.getQuestion();
        UssdMenu previousMenu = menuService.getByQuestion(previousQuestion);
        List<UssdMenu> nextMenus = menuService.getByParentId(previousMenu);
        UssdMenu nextMenu = nextMenus.get(0);
        String ussdHeader = nextMenu.getTitleKin();
        StringBuilder ussdMessage = new StringBuilder();
        switch (previousQuestion) {
            case MAIN_ENTER_PIN:
                previousMenus = menuService.getChildrenByQuestion(previousQuestion);
                if (userService.isValidPin(request.getMsisdn(), request.getInput())) {
                    logger.info("MAIN_ENTER_PIN {}", previousMenus);
                    ussdMessage = ListFormatter.formatListMenus(previousMenus);
                } else {
                    ussdMessage.append(previousMenu.getTitleKin());
                }
                break;
            case MAIN_ASSOCIATIONS:
                previousMenus = menuService.getByParentId(previousMenu);
                logger.info("{} : {}", previousMenu, previousMenus);
                if (QuestionValidator.validateMenus(request.getInput(), previousMenus)) {
                    if (previousMenus.size() > 1) {
                        nextMenu = previousMenus.get(Integer.parseInt(request.getInput()) - 1);
                    } else {
                        nextMenu = previousMenus.get(0);
                    }
                    ussdMessage = ListFormatter.formatListMenus(menuService.getByParentId(nextMenu));
                } else {
                    previousMenu = menuService.getByQuestion(previousQuestion);
                    ussdMessage = ListFormatter.formatListMenus(menuService.getByParentId(previousMenu));
                }
                break;
            case MAIN_LAND_MANAGEMENT:
                previousMenus = menuService.getByParentId(previousMenu);
                logger.info("{} : {}", previousMenu, previousMenus);
                ussdMessage.append(nextMenu.getTitleKin());
                break;
            case MAIN_ACTIVITY_RECORDING:
                previousMenus = menuService.getByParentId(previousMenu);
                logger.info("{} : {}", previousMenu, previousMenus);
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


        session.setQuestionnaire(nextMenu.getQuestionnaire());
        session.setPreviousQuestion(previousMenu.getQuestion());
        session.setQuestion(nextMenu.getQuestion());
        session.setTransactionDatetime(new Date());
        session.setLeaf(nextMenu.getLeaf());
        session.setLastInput(session.getLastInput() + JOINER + request.getInput());
        sessionService.update(session);
        logger.info("MainQuestionnaire : {}", ussdMessage);
        return ussdMessage;
    }
}
