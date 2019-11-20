package com.goltd.agrigoussd.repository;

import com.goltd.agrigoussd.domain.UssdMenu;
import com.goltd.agrigoussd.helpers.enums.Question;
import com.goltd.agrigoussd.helpers.enums.Questionnaire;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface MenuRepository extends JpaRepository<UssdMenu, UUID> {
    UssdMenu findByQuestion(Question question);

    List<UssdMenu> findByParentId(UssdMenu menu);

    List<UssdMenu> findByQuestionnaire(Questionnaire questionnaire);

    List<UssdMenu> findUssdMenusByParentIdQuestionOrderByPriorityAsc(Question question);
}
