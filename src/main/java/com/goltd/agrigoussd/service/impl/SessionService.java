package com.goltd.agrigoussd.service.impl;

import com.goltd.agrigoussd.domain.Session;
import com.goltd.agrigoussd.helpers.UTKit;
import com.goltd.agrigoussd.repository.SessionRepository;
import com.goltd.agrigoussd.service.interfaces.ISessionService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class SessionService implements ISessionService {

    private SessionRepository sessionRepository;

    @Autowired
    public SessionService(SessionRepository sessionRepository) {
        this.sessionRepository = sessionRepository;
    }

    @Override
    public Boolean exists(String msisdn) {
        return sessionRepository.existsByMsisdn(msisdn);
    }

    @Override
    public Session getByMsisdn(String msisdn) {
        return sessionRepository.findByMsisdn(msisdn);
    }

    @Override
    public Session create(Session session) {
        return sessionRepository.save(session);
    }

    @Override
    public Session update(Session session) {
        return sessionRepository.save(session);
    }

    @Override
    public void delete(Session session) {
        sessionRepository.deleteByMsisdn(session.getMsisdn());
    }

    @Override
    public Boolean isExpired(Session session) {
        int elapsedTime = UTKit.elapsedMinutes(session.getTransactionDatetime());
        return elapsedTime > 5 ? Boolean.TRUE : Boolean.FALSE;
    }
}
