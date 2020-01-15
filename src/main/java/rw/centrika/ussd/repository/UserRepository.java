package rw.centrika.ussd.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import rw.centrika.ussd.domain.Language;
import rw.centrika.ussd.domain.UserAccount;

import java.util.UUID;

@Repository
public interface UserRepository extends JpaRepository<UserAccount, UUID> {
    UserAccount findByMsisdn(String msisdn);

    Boolean existsByMsisdn(String msisdn);

    @Modifying
    @Query(value = "update UserAccount u set u.language = :language where u.msisdn = :msisdn")
    void updateLanguage(@Param("language") Language language, @Param("msisdn") String msisdn);
}
