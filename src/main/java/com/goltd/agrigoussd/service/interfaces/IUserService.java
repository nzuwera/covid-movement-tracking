package com.goltd.agrigoussd.service.interfaces;

import com.goltd.agrigoussd.domain.UserAccount;

public interface IUserService {

    UserAccount create(UserAccount userAccount);
    void update(UserAccount userAccount);
    void delete(UserAccount userAccount);
    UserAccount getUserByMsisdn(String msisdn);
    Boolean exists(String msisdn);
    Boolean isValidPin(String msisdn, String pin);
}
