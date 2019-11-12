package com.goltd.agrigoussd;

import com.goltd.agrigoussd.domain.UserAccount;
import com.goltd.agrigoussd.helpers.enums.Gender;
import com.goltd.agrigoussd.service.interfaces.IUserService;
import org.junit.jupiter.api.*;
import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.Date;
import java.util.UUID;

import static com.goltd.agrigoussd.helpers.enums.AccountState.PENDING_SUBSCRIPTION;
import static com.goltd.agrigoussd.helpers.enums.AccountState.SUSPENDED;

@RunWith(SpringRunner.class)
@SpringBootTest
class AgrigoussdApplicationTests {

    private static final Logger logger = LoggerFactory.getLogger(AgrigoussdApplicationTests.class);

    @Autowired
    private IUserService userService;

    UserAccount userAccount;

    @BeforeEach
    void init() {

        String msisdn = "250788313551";
        String fullName = "Nzuwera Gilbert";
        String villageCode = "0102010605";
        int age = 35;
        String pin = "12345";
        userAccount = new UserAccount();
        userAccount.setExpireDate(new Date());
        userAccount.setMsisdn(msisdn);
        userAccount.setGender(Gender.MALE);
        userAccount.setAge(age);
        userAccount.setFullname(fullName);
        userAccount.setVillageCode(villageCode);
        userAccount.setPin(pin);
        userAccount.setAccountState(PENDING_SUBSCRIPTION);
        userAccount.setId(UUID.randomUUID());
        userService.create(userAccount);
        logger.info(userAccount.getMsisdn());
    }

    @AfterEach
    void tearDown() {
        String msisdn = "250788313551";
        UserAccount userAccount = userService.getUserByMsisdn(msisdn);
        userService.delete(userAccount);
    }

    @Test
    public void ContextLoad(){

    }

    @DisplayName("Test UserService.create()")
    @Test
    void testCreate() {
        String msisdn = "250788313551";
        userService.create(userAccount);
        UserAccount createdAccount = userService.getUserByMsisdn(msisdn);
        Assertions.assertEquals(msisdn, createdAccount.getMsisdn());
    }

    @DisplayName("Test UserService.update()")
    @Test
    void testUpdate() {
        String msisdn = "250788313551";
        // UserAccount userAccount = userService.getUserByMsisdn(msisdn);
        userAccount.setAccountState(SUSPENDED);
        userService.update(userAccount);
        UserAccount updateUserAccount = userService.getUserByMsisdn(msisdn);
        Assertions.assertEquals(SUSPENDED, updateUserAccount.getAccountState());
    }

    @DisplayName("Test UserService.getUserByMsisdn()")
    @Test
    void testGetUserByMsisdn() {
        String msisdn = "250788313551";
        UserAccount userAccount = userService.getUserByMsisdn(msisdn);
        Assertions.assertEquals(msisdn, userAccount.getMsisdn());
    }

    @DisplayName("Test UserService.exists()")
    @Test
    void testExists() {
        String msisdn = "250788313551";
        Assertions.assertTrue(userService.exists(msisdn));
    }
}
