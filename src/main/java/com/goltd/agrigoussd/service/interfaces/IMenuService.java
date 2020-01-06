package com.goltd.agrigoussd.service.interfaces;

import com.goltd.agrigoussd.domain.UssdMenu;
import com.goltd.agrigoussd.helpers.enums.Question;

import java.util.List;

public interface IMenuService {
    UssdMenu getByQuestion(Question question);

    List<UssdMenu> getNextMenus(UssdMenu menu);

    List<UssdMenu> getNextMenus(Question question);

}
