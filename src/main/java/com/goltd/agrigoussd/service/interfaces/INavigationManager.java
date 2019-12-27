package com.goltd.agrigoussd.service.interfaces;

import com.goltd.agrigoussd.domain.Session;
import com.goltd.agrigoussd.domain.UssdMenu;
import com.goltd.agrigoussd.helpers.UssdRequest;
import com.goltd.agrigoussd.helpers.UssdResponse;
import com.goltd.agrigoussd.helpers.enums.Visibility;

import javax.servlet.http.HttpServletResponse;
import java.util.List;

public interface INavigationManager {

    Session forward(Session session, UssdRequest request);

    String formatMenu(String input, List<UssdMenu> menus);

    Session backward(UssdRequest request);

    Session toMainMenu(UssdRequest request, Visibility visibility);

    UssdResponse buildMenu(UssdRequest ussdRequest, Session session);

    StringBuilder formatMenu(UssdRequest ussdRequest, List<UssdMenu> menus);

    String sendUssdResponse(UssdResponse ussdResponse, HttpServletResponse httpServletResponse);

    String traverseForward(Session session, UssdRequest request);

}
