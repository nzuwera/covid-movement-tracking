package com.goltd.agrigoussd.controller;

import com.goltd.agrigoussd.domain.UssdMenu;
import com.goltd.agrigoussd.helpers.enums.Question;
import com.goltd.agrigoussd.service.interfaces.IMenuService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.ArrayList;
import java.util.List;

@RestController
@RequestMapping(value = "/test")
public class TestController {

    private IMenuService menuService;

    @Autowired
    public TestController(IMenuService menuService) {
        this.menuService = menuService;
    }

    @GetMapping(value = "/getChildrenByQuestion/{question}")
    public List<UssdMenu> getChildrenByQuestion(@PathVariable String question) {
        try {
            return menuService.getChildrenByQuestion(Question.valueOf(question));
        } catch (EnumConstantNotPresentException e) {
            return new ArrayList<>();
        }
    }
}
