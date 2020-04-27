package com.nzuwera.ussd.covidtracking.service.interfaces;

import com.nzuwera.ussd.covidtracking.domain.Session;
import com.nzuwera.ussd.covidtracking.domain.UssdMenu;
import com.nzuwera.ussd.covidtracking.helpers.ResponseObject;
import com.nzuwera.ussd.covidtracking.helpers.UssdRequest;
import com.nzuwera.ussd.covidtracking.helpers.UssdResponse;
import com.nzuwera.ussd.covidtracking.helpers.enums.Visibility;

import javax.servlet.http.HttpServletResponse;

public interface INavigationManager {

    Session backward(UssdRequest request);

    Session toMainMenu(UssdRequest request, Visibility visibility);

    UssdResponse buildMenu(UssdRequest ussdRequest, Session session);

    String sendUssdResponse(UssdResponse ussdResponse, HttpServletResponse httpServletResponse);

    ResponseObject prepareDisplayMessage(UssdRequest request, UssdMenu menu);

}
