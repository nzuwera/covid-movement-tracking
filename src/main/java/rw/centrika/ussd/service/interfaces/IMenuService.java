package rw.centrika.ussd.service.interfaces;

import rw.centrika.ussd.domain.UssdMenu;
import rw.centrika.ussd.helpers.enums.Question;

import java.util.List;

public interface IMenuService {
    UssdMenu getByQuestion(Question question);

    List<UssdMenu> getNextMenus(UssdMenu menu);

    List<UssdMenu> getNextMenus(Question question);

}
