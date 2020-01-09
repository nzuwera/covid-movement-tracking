package rw.centrika.ussd.service.interfaces;

import rw.centrika.ussd.domain.Session;

public interface ISessionService {

    Boolean exists(String msisdn);

    Session getByMsisdn(String msisdn);

    Session create(Session session);

    Session update(Session session);

    void delete(Session session);

    Boolean isExpired(Session session);
}
