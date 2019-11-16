package com.goltd.agrigoussd.service.interfaces;

import com.goltd.agrigoussd.domain.UssdMenu;
import com.goltd.agrigoussd.helpers.enums.Question;
import com.goltd.agrigoussd.helpers.enums.Questionnaire;

import java.util.List;

public interface IMenuService {
    UssdMenu getByQuestion(Question question);

    List<UssdMenu> getByQuestionnaire(Questionnaire questionnaire);

    List<UssdMenu> getByParentId(UssdMenu menu);

    List<UssdMenu> getChildrenByQuestion(Question question);
}
