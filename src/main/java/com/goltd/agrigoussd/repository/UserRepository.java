package com.goltd.agrigoussd.repository;

import com.goltd.agrigoussd.domain.UserAccount;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface UserRepository extends JpaRepository<UserAccount, UUID> {
    UserAccount findByMsisdn(String msisdn);

    Boolean existsByMsisdn(String msisdn);

    @Modifying
    @Query(value = "update UserAccount u set u.pin = :pin where u.msisdn = :msisdn")
    void updatePin(@Param("pin") String pin,@Param("msisdn") String msisdn);
}
