package com.goltd.agrigoussd.controller;


import com.goltd.agrigoussd.helpers.UTKit;
import com.goltd.agrigoussd.helpers.UssdRequest;
import com.goltd.agrigoussd.helpers.enums.Freeflow;
import com.goltd.agrigoussd.service.interfaces.IUserService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.servlet.http.HttpServletResponse;

@RestController
@RequestMapping(value = "/ussd")
public class UssdEndpoint {

    private static final Logger logger = LoggerFactory.getLogger(UssdEndpoint.class);

    @Autowired
    private IUserService userService;

    @GetMapping(value = "/agrigo")
    public String getUssdResponse(UssdRequest request, HttpServletResponse httpServletResponse) {
        httpServletResponse.setHeader(UTKit.FREE_FLOW_HEADER, Freeflow.FC.name());
        String ussdResponse;
        if (Boolean.TRUE.equals(userService.exists(request.getInput()))) {
            ussdResponse = "Welcome to Agrigo Ussd\nEnter PIN";
        } else {
            ussdResponse = "Welcome to Agrigo Ussd\nEnter Fullname";
        }
        logger.info(ussdResponse);
        return ussdResponse;
    }
}
