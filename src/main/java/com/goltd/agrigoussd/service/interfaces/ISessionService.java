package com.goltd.agrigoussd.service.interfaces;

import com.goltd.agrigoussd.domain.Session;

public interface ISessionService {

    Boolean exists(String msisdn);

    Session getByMsisdn(String msisdn);

    void create(Session session);

    Session update(Session session);

    void delete(Session session);
}
