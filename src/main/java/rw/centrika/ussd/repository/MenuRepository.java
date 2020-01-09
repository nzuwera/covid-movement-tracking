package rw.centrika.ussd.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import rw.centrika.ussd.domain.UssdMenu;
import rw.centrika.ussd.helpers.enums.Question;

import java.util.List;
import java.util.UUID;

@Repository
public interface MenuRepository extends JpaRepository<UssdMenu, UUID> {
    UssdMenu findByQuestion(Question question);

    List<UssdMenu> findByParentMenuOrderByPriorityAsc(UssdMenu menu);

    List<UssdMenu> findUssdMenusByParentMenuQuestionOrderByPriorityAsc(Question question);
}
