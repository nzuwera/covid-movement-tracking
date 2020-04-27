package com.nzuwera.ussd.covidtracking.service.impl;

import com.nzuwera.ussd.covidtracking.domain.UssdMenu;
import com.nzuwera.ussd.covidtracking.helpers.enums.Question;
import com.nzuwera.ussd.covidtracking.repository.MenuRepository;
import com.nzuwera.ussd.covidtracking.service.interfaces.IMenuService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class MenuService implements IMenuService {

    private MenuRepository menuRepository;

    @Autowired
    public MenuService(MenuRepository menuRepository) {
        this.menuRepository = menuRepository;
    }

    @Override
    public UssdMenu getByQuestion(Question question) {
        return menuRepository.findByQuestion(question);
    }

    @Override
    public List<UssdMenu> getNextMenus(UssdMenu menu) {
        return menuRepository.findByParentMenuOrderByPriorityAsc(menu);
    }

    @Override
    public List<UssdMenu> getNextMenus(Question question) {
        return menuRepository.findUssdMenusByParentMenuQuestionOrderByPriorityAsc(question);
    }
}
