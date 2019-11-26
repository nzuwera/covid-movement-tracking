package com.goltd.agrigoussd.controller;

import com.goltd.agrigoussd.domain.UserAccount;
import com.goltd.agrigoussd.domain.UserAccountDto;
import com.goltd.agrigoussd.domain.UssdMenu;
import com.goltd.agrigoussd.helpers.UTKit;
import com.goltd.agrigoussd.helpers.enums.AccountState;
import com.goltd.agrigoussd.helpers.enums.Gender;
import com.goltd.agrigoussd.helpers.enums.Question;
import com.goltd.agrigoussd.service.interfaces.IMenuService;
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

    @Autowired
    public TestController(IMenuService menuService, IUserService userService) {
        this.menuService = menuService;
        this.userService = userService;
    }

    @GetMapping(value = "/getChildrenByQuestion/{question}")
    public List<UssdMenu> getChildrenByQuestion(@PathVariable String question) {
        try {
            return menuService.getChildrenByQuestion(Question.valueOf(question));
        } catch (EnumConstantNotPresentException e) {
            return new ArrayList<>();
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
