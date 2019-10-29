package com.goltd.agrigoussd;

import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.util.Assert;

@SpringBootTest
class AgrigoussdApplicationTests {

    @Test
    void contextLoads() {
        Assert.isTrue(Boolean.TRUE, "Context Loaded");
    }

}
