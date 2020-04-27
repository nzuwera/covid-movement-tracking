package com.nzuwera.ussd.covidtracking.repository;

import com.nzuwera.ussd.covidtracking.domain.Session;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import javax.transaction.Transactional;
import java.util.UUID;

@Repository
public interface SessionRepository extends JpaRepository<Session, UUID> {
    Boolean existsByMsisdn(String msisdn);

    Session findByMsisdn(String msisdn);

    @Transactional
    void deleteByMsisdn(String msisdn);
}
