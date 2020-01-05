package com.goltd.agrigoussd.validators;

import com.goltd.agrigoussd.domain.Location;
import com.goltd.agrigoussd.domain.UssdMenu;
import com.goltd.agrigoussd.helpers.enums.Gender;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Service
public class QuestionValidator {
    private QuestionValidator() {
        //
    }

    private static final String NUMBERS = "[^0-9]";
    private static final String LETTERS = "[^A-Za-z ]";

    public static Boolean validateFullName(String fullName) {
        if (fullName == null || fullName.trim().isEmpty()) {
            return false;
        }
        Pattern p = Pattern.compile(LETTERS);
        Matcher m = p.matcher(fullName);
        return !m.find();
    }

    public static Boolean validateAge(String age) {
        if (age == null || age.trim().isEmpty() || Integer.parseInt(age) <= 18) {
            return false;
        }
        Pattern p = Pattern.compile(NUMBERS);
        Matcher m = p.matcher(age);
        return !m.find();
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
        return validateNumericalString(pin) && pin.length() == 5;
    }

    public static Boolean validateNumericalString(String number) {
        if (number == null) {
            return false;
        }
        try {
            Integer.parseInt(number);
        } catch (NumberFormatException nfe) {
            return false;
        }
        return true;
    }

    public static boolean validateMenus(String input, List<UssdMenu> previousMenus) {
        if (validateNumericalString(input)) {
            int selectedInput = Integer.parseInt(input);
            int previousMenusSize = previousMenus.size();
            return selectedInput <= previousMenusSize;
        }
        return false;
    }
}
