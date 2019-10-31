package com.goltd.agrigoussd.audit;

import com.goltd.agrigoussd.helpers.WebUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.AuditorAware;

import java.util.Optional;

public class AuditorAwareImpl implements AuditorAware<String> {


    @Autowired
    private WebUtils webUtils;

    @Override
    public Optional<String> getCurrentAuditor() {
        return Optional.of(webUtils.getClientIp());
    }

}

