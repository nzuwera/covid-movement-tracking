package com.goltd.agrigoussd.validators;

import com.goltd.agrigoussd.domain.Location;
import com.goltd.agrigoussd.domain.UssdMenu;
import com.goltd.agrigoussd.helpers.enums.Gender;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class QuestionValidator {
    private QuestionValidator() {
        //
    }

    private static final String NUMBERS = "^[0-9]+$";
    private static final String MTN_PHONE = "^078\\d{7}";
    private static final String PIN = "^\\d{5}$";
    private static final String LETTERS = "^[A-Za-z ]{3,}$";
    private static final String UPI_FORMAT = "^\\d{1}\\/\\d{2}\\/\\d{2}\\/\\d{2}\\/\\d{4,}$";

    public static Boolean validateStringWord(String fullName) {
        return fullName.matches(LETTERS);
    }

    public static Boolean validateAge(String age) {
        return age.matches(NUMBERS) && Integer.parseInt(age) >= 18;
    }

    public static Boolean validateLocations(String input, List<Location> locations) {
        if (validateNumericalString(input)) {
            int selectedInput = Integer.parseInt(input);
            int locationSize = locations.size();
            return selectedInput <= locationSize;
        }
        return false;
    }

    public static Boolean validateGender(String string) {
        int enumLength = Gender.values().length;
        return Integer.parseInt(string) <= enumLength;

    }

    public static <E extends Enum<E>> Boolean validateEnum(String string, Class<E> enumObject) {
        int enumLength = enumObject.getEnumConstants().length;
        return Integer.parseInt(string) <= enumLength;
    }

    public static Boolean validatePinFormat(String pin) {
        return pin.matches(PIN);
    }

    public static Boolean validateNumericalString(String number) {
        return number.matches(NUMBERS);
    }

    public static boolean validateMenus(String input, List<UssdMenu> previousMenus) {
        if (validateNumericalString(input)) {
            int selectedInput = Integer.parseInt(input);
            int previousMenusSize = previousMenus.size();
            return selectedInput <= previousMenusSize;
        }
        return false;
    }

    public static boolean validateUPIFormat(String upiNumber){
        return upiNumber.matches(UPI_FORMAT);
    }
    public static boolean validateMtnPhone(String phoneNumber){
        return phoneNumber.matches(MTN_PHONE);
    }
}
