package com.goltd.agrigoussd.service.interfaces;

import com.goltd.agrigoussd.domain.Session;
import com.goltd.agrigoussd.helpers.UssdRequest;

public interface IUssd {

//    void initialize(UssdRequest request);
    Session navigateForward(UssdRequest request);

}
