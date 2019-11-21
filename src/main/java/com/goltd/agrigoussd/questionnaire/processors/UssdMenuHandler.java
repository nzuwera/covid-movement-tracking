package com.goltd.agrigoussd.questionnaire.processors;

import com.goltd.agrigoussd.domain.UssdMenu;
import com.goltd.agrigoussd.helpers.UTKit;
import com.goltd.agrigoussd.service.interfaces.IMenuService;
import org.springframework.stereotype.Service;

import java.util.EnumSet;
import java.util.List;

@Service
public class UssdMenuHandler {

    private UssdMenuHandler() {
        // Empty Constructor
    }

    public String buildListMenus(List<UssdMenu> menuList) {
        StringBuilder menuString = new StringBuilder();
        for (int i = 0; i < menuList.size(); i++) {
            menuString.append(i + 1);
            menuString.append(UTKit.BLANK);
            menuString.append(menuList.get(i).getTitleKin());
            menuString.append(UTKit.EOL);
        }
        return menuString.toString();
    }

    public String buildFormInputMenu(UssdMenu menu) {
        return menu.getTitleKin();
    }

    public <E extends Enum<E>> String buildEnumMenu(Class<E> enumObject) {
        StringBuilder message = new StringBuilder();
        int i = 1;
        for (E enumValue : EnumSet.allOf(enumObject)) {
            message.append(i);
            message.append(UTKit.DOT + UTKit.BLANK);
            message.append(enumValue.toString());
            message.append(UTKit.EOL);
            i++;
        }
        return message.toString();
    }

    public String buildMessageMenu(UssdMenu menu) {
        return menu.getTitleKin();
    }

    public String buildDynamicListMenus(IMenuService menuService, UssdMenu menu) {
        return menu.getTitleKin();
    }
}
