package com.goltd.agrigoussd.service.interfaces;

import com.goltd.agrigoussd.domain.Session;
import com.goltd.agrigoussd.domain.UssdMenu;
import com.goltd.agrigoussd.helpers.UssdRequest;
import com.goltd.agrigoussd.helpers.UssdResponse;
import com.goltd.agrigoussd.helpers.enums.Question;
import com.goltd.agrigoussd.helpers.enums.Visibility;

import javax.servlet.http.HttpServletResponse;
import java.util.List;

public interface INavigationManager {

    Session forward(UssdRequest request);

    Session backward(UssdRequest request);

    Session toMainMenu(UssdRequest request, Visibility visibility);

    UssdResponse buildMenu(UssdRequest ussdRequest, Question question);

    StringBuilder formatMenu(UssdRequest ussdRequest, List<UssdMenu> menus);

    String formatMenu(String header, UssdRequest ussdRequest, List<UssdMenu> ussdMenus);

    String sendUssdResponse(UssdResponse ussdResponse, HttpServletResponse httpServletResponse);

}
