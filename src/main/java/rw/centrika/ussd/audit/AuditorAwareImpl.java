package rw.centrika.ussd.audit;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.AuditorAware;
import rw.centrika.ussd.helpers.WebUtils;

import java.util.Optional;

public class AuditorAwareImpl implements AuditorAware<String> {


    @Autowired
    private WebUtils webUtils;

    @Override
    public Optional<String> getCurrentAuditor() {
        return Optional.of(webUtils.getClientIp());
    }

}

