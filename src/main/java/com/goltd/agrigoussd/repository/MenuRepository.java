package com.goltd.agrigoussd.repository;

import com.goltd.agrigoussd.domain.UssdMenu;
import com.goltd.agrigoussd.helpers.enums.Question;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface MenuRepository extends JpaRepository<UssdMenu, UUID> {
    UssdMenu findByQuestion(Question question);

    List<UssdMenu> findByParentId(UssdMenu menu);

    List<UssdMenu> findUssdMenusByParentIdQuestionOrderByPriorityAsc(Question question);

    List<UssdMenu> findUssdMenusByQuestion(Question question);

    @Query(value = "select * from ussd_menu m where m.parent_id = ?1", nativeQuery = true)
    List<UssdMenu> getChildrenByParentId(UUID uuid);
}
