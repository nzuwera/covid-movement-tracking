package com.nzuwera.ussd.covidtracking.validators;

import org.springframework.stereotype.Service;

@Service
public class QuestionValidator {
    private QuestionValidator() {
        //
    }

    private static final String MTN_PHONE = "^078\\d{7}$";
    private static final String AIRTEL_PHONE = "^073\\d{7}$";
    private static final String TIGO_PHONE = "^072\\d{7}$";
    private static final String LETTERS = "^[A-Za-z ]{3,}$";

    public static Boolean validateStringWord(String fullName) {
        return fullName.matches(LETTERS);
    }

    public static boolean validateMtnPhone(String phoneNumber) {
        return phoneNumber.matches(MTN_PHONE) || phoneNumber.matches(AIRTEL_PHONE) || phoneNumber.matches(TIGO_PHONE);
    }
}
