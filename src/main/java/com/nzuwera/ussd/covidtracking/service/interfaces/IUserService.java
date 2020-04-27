package com.nzuwera.ussd.covidtracking.service.interfaces;

import com.nzuwera.ussd.covidtracking.domain.Language;
import com.nzuwera.ussd.covidtracking.domain.UserAccount;

public interface IUserService {

    void create(UserAccount userAccount);
    void update(UserAccount userAccount);
    void delete(UserAccount userAccount);
    UserAccount getUserByMsisdn(String msisdn);
    Boolean exists(String msisdn);
    Boolean isValidPin(String msisdn, String pin);
    void updateLanguage(String msisdn, Language language);
}
