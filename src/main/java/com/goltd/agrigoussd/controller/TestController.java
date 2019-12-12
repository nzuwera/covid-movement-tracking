package com.goltd.agrigoussd.controller;

import com.goltd.agrigoussd.domain.UserAccount;
import com.goltd.agrigoussd.domain.UserAccountDto;
import com.goltd.agrigoussd.domain.UssdMenu;
import com.goltd.agrigoussd.helpers.UTKit;
import com.goltd.agrigoussd.helpers.enums.AccountState;
import com.goltd.agrigoussd.helpers.enums.Gender;
import com.goltd.agrigoussd.helpers.enums.Question;
import com.goltd.agrigoussd.service.interfaces.IMenuService;
import com.goltd.agrigoussd.service.interfaces.ISessionService;
import com.goltd.agrigoussd.service.interfaces.IUserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping(value = "/test")
public class TestController {

    private IMenuService menuService;
    private IUserService userService;
    private ISessionService sessionService;

    @Autowired
    public TestController(IMenuService menuService, IUserService userService, ISessionService sessionService) {
        this.menuService = menuService;
        this.userService = userService;
        this.sessionService = sessionService;
    }

    @GetMapping(value = "/getChildrenByQuestion/{question}")
    public List<UssdMenu> getChildrenByQuestion(@PathVariable String question) {
        try {
            return menuService.getNextMenus(Question.valueOf(question));
        } catch (EnumConstantNotPresentException e) {
            return new ArrayList<>();
        }
    }

    @GetMapping(value = "/getByParent/{question}")
    public List<UssdMenu> getByParentId(@PathVariable String question) {
        try {
            UssdMenu menu = menuService.getByQuestion(Question.valueOf(question));
            return menuService.getNextMenus(menu);
        } catch (Exception e) {
            throw e;
        }
    }


    @DeleteMapping(value = "/delete/{msisdn}")
    public String clearSession(@PathVariable String msisdn) {
        try {
            sessionService.delete(sessionService.getByMsisdn(msisdn));
            return "user " + msisdn + " deleted";
        } catch (Exception e) {
            return e.getMessage();
        }
    }

    @PostMapping(value = "/user")
    public UserAccount createUserAccount(@Valid @RequestBody UserAccountDto userAccountDto) {
        UserAccount userAccount = new UserAccount();
        userAccount.setId(UUID.randomUUID());
        userAccount.setAge(userAccountDto.getAge());
        userAccount.setExpireDate(UTKit.setExpiryDate(new Date(), 30));
        userAccount.setFullname(userAccountDto.getFullName());
        userAccount.setAccountState(AccountState.valueOf(userAccountDto.getAccountState().name()));
        userAccount.setGender(Gender.valueOf(userAccountDto.getGender().name()));
        userAccount.setVillageCode(userAccountDto.getVillageCode());
        userAccount.setPin(userAccountDto.getPin());
        userAccount.setMsisdn(userAccountDto.getMsisdn());
        userService.create(userAccount);
        return userAccount;
    }
}
