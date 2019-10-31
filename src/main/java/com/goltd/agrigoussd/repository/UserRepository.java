package com.goltd.agrigoussd.repository;

import com.goltd.agrigoussd.domain.UserAccount;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface UserRepository extends JpaRepository<UserAccount, UUID> {
    UserAccount findByMsisdn(String msisdn);

    Boolean existsByMsisdn(String msisdn);
}
