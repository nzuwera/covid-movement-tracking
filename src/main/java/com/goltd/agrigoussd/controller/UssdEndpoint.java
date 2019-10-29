package com.goltd.agrigoussd.controller;


import com.goltd.agrigoussd.helpers.Freeflow;
import com.goltd.agrigoussd.helpers.UTKit;
import com.goltd.agrigoussd.helpers.UssdRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.servlet.http.HttpServletResponse;

@RestController
@RequestMapping(value = "/ussd")
public class UssdEndpoint {

    private static final Logger logger = LoggerFactory.getLogger(UssdEndpoint.class);

    @GetMapping(value = "/agrigo")
    public String getUssdResponse(UssdRequest request, HttpServletResponse httpServletResponse) {
        httpServletResponse.setHeader(UTKit.FREE_FLOW_HEADER, Freeflow.FB.name());
        logger.info(request.toString());
        return "Welcome to Agrigo Ussd";
    }
}
