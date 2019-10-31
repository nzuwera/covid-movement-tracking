package com.goltd.agrigoussd;

import org.junit.jupiter.api.Test;
import org.junit.platform.runner.JUnitPlatform;
import org.junit.platform.suite.api.SelectClasses;
import org.junit.platform.suite.api.SelectPackages;
import org.junit.runner.RunWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.util.Assert;

@RunWith(JUnitPlatform.class)
@SelectPackages("com.goltd.agrigoussd.service.impl")
class AgrigoussdApplicationTests {

}
