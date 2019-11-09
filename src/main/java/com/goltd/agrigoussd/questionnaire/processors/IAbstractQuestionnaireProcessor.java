package com.goltd.agrigoussd.questionnaire.processors;

import com.goltd.agrigoussd.domain.Session;
import com.goltd.agrigoussd.helpers.UssdRequest;

public interface IAbstractQuestionnaireProcessor {

    StringBuilder buildMenu(Session session, UssdRequest request);
}
