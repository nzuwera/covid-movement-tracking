package com.goltd.agrigoussd.service.impl;

import com.goltd.agrigoussd.domain.UssdMenu;
import com.goltd.agrigoussd.helpers.enums.Question;
import com.goltd.agrigoussd.helpers.enums.Questionnaire;
import com.goltd.agrigoussd.repository.MenuRepository;
import com.goltd.agrigoussd.service.interfaces.IMenuService;
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
    public List<UssdMenu> getByQuestionnaire(Questionnaire questionnaire) {
        return menuRepository.findByQuestionnaire(questionnaire);
    }

    @Override
    public List<UssdMenu> getByParentId(UssdMenu menu) {
        return menuRepository.findByParentId(menu);
    }

    @Override
    public List<UssdMenu> getChildrenByQuestion(Question question) {
        return menuRepository.findUssdMenusByParentIdQuestion(question);
    }
}
