package com.goltd.agrigoussd.service.impl;

import com.goltd.agrigoussd.domain.UserAccount;
import com.goltd.agrigoussd.helpers.enums.AccountState;
import com.goltd.agrigoussd.service.interfaces.IUserService;
import org.junit.jupiter.api.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest
class UserServiceTest {
    private static final Logger logger = LoggerFactory.getLogger(UserServiceTest.class.getName());

    String msisdn = "250788313531";

    @Autowired
    private IUserService userService;

    @BeforeEach
    void init() {
        UserAccount userAccount = userService.create(msisdn);
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
        userAccount.setAccountState(AccountState.ACTIVE);
        userService.update(userAccount);
        UserAccount updateUserAccount = userService.getUserByMsisdn(msisdn);
        Assertions.assertEquals(AccountState.ACTIVE,updateUserAccount.getAccountState());
    }

    @DisplayName("Test UserService.getUserByMsisdn()")
    @Test
    void testGetUserByMsisdn() {
        UserAccount userAccount = userService.getUserByMsisdn(msisdn);
        Assertions.assertEquals(msisdn,userAccount.getFullname());
    }

    @DisplayName("Test UserService.exists()")
    @Test
    void testExists() {
        Assertions.assertTrue(userService.exists(msisdn));
    }
}