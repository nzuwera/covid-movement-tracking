package rw.centrika.ussd.service.impl;

import rw.centrika.ussd.domain.UssdMenu;
import rw.centrika.ussd.helpers.enums.Question;
import rw.centrika.ussd.repository.MenuRepository;
import rw.centrika.ussd.service.interfaces.IMenuService;
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
