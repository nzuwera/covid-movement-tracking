package com.goltd.agrigoussd.repository;

import com.goltd.agrigoussd.domain.Session;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface SessionRepository extends JpaRepository<Session, UUID> {
    Boolean existsByMsisdn(String msisdn);

    Session findByMsisdn(String msisdn);

    @Query(value = "SELECT extract(minute from(now() - transaction_datetime)) as elapseTime FROM \"session\" where msisdn = ?1",nativeQuery = true)
    Integer getElapsedTime(String msisdn);
}
