package com.goltd.agrigoussd.service.impl;

import com.goltd.agrigoussd.domain.Session;
import com.goltd.agrigoussd.helpers.UssdRequest;
import com.goltd.agrigoussd.service.interfaces.ISessionService;
import com.goltd.agrigoussd.service.interfaces.IUssd;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class Ussd implements IUssd {
    private ISessionService sessionService;

    @Autowired
    public Ussd(ISessionService sessionService) {
        this.sessionService = sessionService;
    }

    @Override
    public Session navigateForward(UssdRequest request) {
        return sessionService.getByMsisdn(request.getMsisdn());
    }
}
