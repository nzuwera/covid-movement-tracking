package com.goltd.agrigoussd.service.interfaces;

import com.goltd.agrigoussd.domain.UserAccount;

public interface IUserService {

    UserAccount create(String msisdn);
    void update(UserAccount userAccount);
    void delete(UserAccount userAccount);
    UserAccount getUserByMsisdn(String msisdn);
    Boolean exists(String msisdn);
}
