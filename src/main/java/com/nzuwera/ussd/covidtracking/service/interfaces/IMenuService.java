package com.nzuwera.ussd.covidtracking.service.interfaces;

import com.nzuwera.ussd.covidtracking.domain.UssdMenu;
import com.nzuwera.ussd.covidtracking.helpers.enums.Question;

import java.util.List;

public interface IMenuService {
    UssdMenu getByQuestion(Question question);

    List<UssdMenu> getNextMenus(UssdMenu menu);

    List<UssdMenu> getNextMenus(Question question);

}
