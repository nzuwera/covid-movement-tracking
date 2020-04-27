package com.nzuwera.ussd.covidtracking.repository;

import com.nzuwera.ussd.covidtracking.domain.UssdMenu;
import com.nzuwera.ussd.covidtracking.helpers.enums.Question;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface MenuRepository extends JpaRepository<UssdMenu, UUID> {
    UssdMenu findByQuestion(Question question);

    List<UssdMenu> findByParentMenuOrderByPriorityAsc(UssdMenu menu);

    List<UssdMenu> findUssdMenusByParentMenuQuestionOrderByPriorityAsc(Question question);
}
