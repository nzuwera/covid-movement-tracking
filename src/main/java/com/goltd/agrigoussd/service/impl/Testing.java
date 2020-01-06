package com.goltd.agrigoussd.service.impl;

import com.goltd.agrigoussd.validators.QuestionValidator;

public class Testing {
    public static void main(String[] args){
        System.out.println(QuestionValidator.validateUPIFormat("1/03/05/01/242323"));
    }
}
