package com.goltd.agrigoussd.service.impl;

import com.goltd.agrigoussd.domain.UserAccount;
import com.goltd.agrigoussd.helpers.enums.Gender;
import com.goltd.agrigoussd.service.interfaces.IUserService;
import org.junit.jupiter.api.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.Date;
import java.util.UUID;

import static com.goltd.agrigoussd.helpers.enums.AccountState.PENDING_SUBSCRIPTION;
import static com.goltd.agrigoussd.helpers.enums.AccountState.SUSPENDED;

@SpringBootTest
class UserServiceTest {
    private static final Logger logger = LoggerFactory.getLogger(UserServiceTest.class.getName());

    private String msisdn;
    private String fullName;
    private String villageCode;
    private int age;
    private String pin;

    @Autowired
    private IUserService userService;

    @BeforeEach
    void init() {
        msisdn = "250788313531";
        fullName = "Nzuwera Gilbert";
        villageCode = "0102010605";
        age = 35;
        pin = "12345";
        UserAccount userAccount = new UserAccount();
        userAccount.setExpireDate(new Date());
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
        UserAccount userAccount = userService.getUserByMsisdn(msisdn);
        userService.delete(userAccount);
    }

    @DisplayName("Test UserService.create()")
    @Test
    void testCreate() {
        UserAccount createdAccount = userService.getUserByMsisdn(msisdn);
        Assertions.assertEquals(createdAccount.getMsisdn(), msisdn);
    }

    @DisplayName("Test UserService.update()")
    @Test
    void testUpdate() {
        UserAccount userAccount = userService.getUserByMsisdn(msisdn);
        userAccount.setAccountState(SUSPENDED);
        userService.update(userAccount);
        UserAccount updateUserAccount = userService.getUserByMsisdn(msisdn);
        Assertions.assertEquals(SUSPENDED, updateUserAccount.getAccountState());
    }

    @DisplayName("Test UserService.getUserByMsisdn()")
    @Test
    void testGetUserByMsisdn() {
        UserAccount userAccount = userService.getUserByMsisdn(msisdn);
        Assertions.assertEquals(msisdn, userAccount.getFullname());
    }

    @DisplayName("Test UserService.exists()")
    @Test
    void testExists() {
        Assertions.assertTrue(userService.exists(msisdn));
    }
}