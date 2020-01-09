package rw.centrika.ussd.controller;

import rw.centrika.ussd.domain.Location;
import rw.centrika.ussd.domain.UserAccount;
import rw.centrika.ussd.domain.UserAccountDto;
import rw.centrika.ussd.domain.UssdMenu;
import rw.centrika.ussd.helpers.UTKit;
import rw.centrika.ussd.helpers.enums.AccountState;
import rw.centrika.ussd.helpers.enums.Gender;
import rw.centrika.ussd.helpers.enums.Question;
import rw.centrika.ussd.service.interfaces.ILocationService;
import rw.centrika.ussd.service.interfaces.IMenuService;
import rw.centrika.ussd.service.interfaces.ISessionService;
import rw.centrika.ussd.service.interfaces.IUserService;
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
    private ILocationService locationService;

    @Autowired
    public TestController(IMenuService menuService, IUserService userService, ISessionService sessionService, ILocationService locationService) {
        this.menuService = menuService;
        this.userService = userService;
        this.sessionService = sessionService;
        this.locationService = locationService;
    }

    @GetMapping(value = "/getChildrenByQuestion/{question}")
    public List<UssdMenu> getChildrenByQuestion(@PathVariable String question) {
        try {
            return menuService.getNextMenus(Question.valueOf(question));
        } catch (EnumConstantNotPresentException e) {
            return new ArrayList<>();
        }
    }

    @GetMapping(value = "/getLocations/{locationCode}")
    public List<Location> getLocationByCode(@PathVariable String locationCode) {
        try {
            return locationService.getlocationsByParentCode(locationCode);
        } catch (EnumConstantNotPresentException e) {
            return new ArrayList<>();
        }
    }

    @GetMapping(value = "/getByParent/{question}")
    public List<UssdMenu> getByParentId(@PathVariable String question) {
        List<UssdMenu> children;
        try {
            UssdMenu menu = menuService.getByQuestion(Question.valueOf(question));
            children = menuService.getNextMenus(menu);
        } catch (Exception e) {
            children = new ArrayList<>();
        }
        return children;
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
