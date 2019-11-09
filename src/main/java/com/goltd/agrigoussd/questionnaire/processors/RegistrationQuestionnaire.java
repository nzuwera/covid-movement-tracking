package com.goltd.agrigoussd.questionnaire.processors;

import com.goltd.agrigoussd.domain.Session;
import com.goltd.agrigoussd.domain.UssdMenu;
import com.goltd.agrigoussd.helpers.UssdRequest;
import com.goltd.agrigoussd.helpers.enums.Gender;
import com.goltd.agrigoussd.helpers.formatter.EnumFormatter;
import com.goltd.agrigoussd.service.interfaces.IMenuService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class RegistrationQuestionnaire implements IAbstractQuestionnaireProcessor {

    private IMenuService menuService;

    @Autowired
    public RegistrationQuestionnaire(IMenuService menuService) {
        this.menuService = menuService;

    }

    @Override
    public StringBuilder buildMenu(Session session, UssdRequest request) {
        UssdMenu menu = menuService.getByQuestion(session.getQuestion());
        String ussdHeader = menu.getTitleKin();
        StringBuilder ussdMessage = new StringBuilder();
        switch (menu.getQuestion()) {

            case REGISTRATION_SELECT_GENDER:
                ussdMessage.append(EnumFormatter.format(ussdHeader, Gender.class));
                break;
            case REGISTRATION_SELECT_LOCATION_PROVINCE:
                break;
            case REGISTRATION_SELECT_LOCATION_DISTRICT:
                break;
            case REGISTRATION_SELECT_LOCATION_SECTOR:
                break;
            case REGISTRATION_SELECT_LOCATION_CELL:
                break;
            case REGISTRATION_SELECT_LOCATION_VILLAGE:
                break;
            case REGISTRATION_ENTER_PIN:
                break;
            case REGISTRATION_VERIFY_PIN:
                break;
            default:
                ussdMessage.append(ussdHeader);
                break;
        }
        return ussdMessage;
    }
}
