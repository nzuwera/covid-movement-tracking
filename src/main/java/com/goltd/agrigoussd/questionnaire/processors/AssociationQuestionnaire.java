package com.goltd.agrigoussd.questionnaire.processors;

import com.goltd.agrigoussd.domain.Session;
import com.goltd.agrigoussd.domain.UssdMenu;
import com.goltd.agrigoussd.helpers.UssdRequest;
import com.goltd.agrigoussd.helpers.enums.Question;
import com.goltd.agrigoussd.helpers.formatter.ListFormatter;
import com.goltd.agrigoussd.service.interfaces.IMenuService;
import com.goltd.agrigoussd.service.interfaces.ISessionService;
import com.goltd.agrigoussd.service.interfaces.IUserService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class AssociationQuestionnaire implements IAbstractQuestionnaireProcessor {

    private static final Logger logger = LoggerFactory.getLogger(AssociationQuestionnaire.class);

    private IMenuService menuService;
    private ISessionService sessionService;
    private IUserService userService;

    @Autowired
    public AssociationQuestionnaire(IMenuService menuService, ISessionService sessionService, IUserService userService) {
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
            case ASSOCIATIONS_JOIN:
                previousMenus = menuService.getChildrenByQuestion(previousQuestion);
                ussdMessage.append(ListFormatter.formatListMenus(previousMenus));
                break;
            case ASSOCIATIONS_ENTER_ASSOCIATION_CODE:
                ussdMessage.append(nextMenu.getTitleKin());
                break;
            case ASSOCIATIONS_SHOW_JOINED_ASSOCIATION:
                ussdMessage.append(nextMenu.getTitleKin());
                break;
            case ASSOCIATIONS_VIEW:
                ussdMessage.append(nextMenu.getTitleKin());
                break;
            case ASSOCIATIONS_SHOW_MY_ASSOCIATIONS:
                ussdMessage.append(nextMenu.getTitleKin());
                break;
            case ASSOCIATIONS_LEAVE:
                ussdMessage.append(nextMenu.getTitleKin());
                break;
            case ASSOCIATIONS_LEAVE_ASSOCIATION:
                ussdMessage.append(nextMenu.getTitleKin());
                break;
            case ASSOCIATIONS_CONFIRM_LEAVE_ASSOCIATION:
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
