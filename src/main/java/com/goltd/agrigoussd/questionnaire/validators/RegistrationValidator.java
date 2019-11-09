package com.goltd.agrigoussd.questionnaire.validators;

import org.springframework.stereotype.Service;

@Service
public class RegistrationValidator {

    public static String validateFullname(String fullname) {
        return null;
    }

    public static String validateAge(String fullname) {
        return null;
    }

    public static String validateProvince(int provinceId) {
        return null;
    }

    public static String validateDistrict(int provinceId, int districtId) {
        return null;
    }

    public static String validateSector(int districtId, int sectorId) {
        return null;
    }

    public static String validateCell(int sectorId, int cellId) {
        return null;
    }

    public static String validateVillage(int cellId, int villageId) {
        return null;
    }

    public static Boolean validatePIN(String pin, String verifyPin){
        return null;
    }
}
