package rw.centrika.ussd.service.interfaces;

import rw.centrika.ussd.domain.Session;
import rw.centrika.ussd.domain.UssdMenu;
import rw.centrika.ussd.helpers.ResponseObject;
import rw.centrika.ussd.helpers.UssdRequest;
import rw.centrika.ussd.helpers.UssdResponse;
import rw.centrika.ussd.helpers.enums.Visibility;

import javax.servlet.http.HttpServletResponse;
import java.util.List;

public interface INavigationManager {

    Session backward(UssdRequest request);

    Session toMainMenu(UssdRequest request, Visibility visibility);

    UssdResponse buildMenu(UssdRequest ussdRequest, Session session);

    String sendUssdResponse(UssdResponse ussdResponse, HttpServletResponse httpServletResponse);

    ResponseObject prepareDisplayMessage(UssdRequest request, List<UssdMenu> menus);

}
