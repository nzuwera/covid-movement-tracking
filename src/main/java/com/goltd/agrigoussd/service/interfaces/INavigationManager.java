package com.goltd.agrigoussd.service.interfaces;

import com.goltd.agrigoussd.domain.Session;
import com.goltd.agrigoussd.domain.UssdMenu;
import com.goltd.agrigoussd.helpers.ResponseObject;
import com.goltd.agrigoussd.helpers.UssdRequest;
import com.goltd.agrigoussd.helpers.UssdResponse;
import com.goltd.agrigoussd.helpers.enums.Visibility;

import javax.servlet.http.HttpServletResponse;
import java.util.List;

public interface INavigationManager {

    Session backward(UssdRequest request);

    Session toMainMenu(UssdRequest request, Visibility visibility);

    UssdResponse buildMenu(UssdRequest ussdRequest, Session session);

    String sendUssdResponse(UssdResponse ussdResponse, HttpServletResponse httpServletResponse);

    ResponseObject prepareDisplayMessage(UssdRequest request, List<UssdMenu> menus);

}
