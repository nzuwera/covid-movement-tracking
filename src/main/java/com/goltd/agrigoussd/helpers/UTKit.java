package com.goltd.agrigoussd.helpers;

import com.goltd.agrigoussd.domain.UssdMenu;
import com.goltd.agrigoussd.helpers.enums.BuyerType;
import com.goltd.agrigoussd.helpers.enums.Gender;
import com.goltd.agrigoussd.helpers.enums.Question;
import com.goltd.agrigoussd.helpers.enums.QuestionType;
import com.goltd.agrigoussd.helpers.formatter.EnumFormatter;
import org.mindrot.jbcrypt.BCrypt;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * USSD Tool Kit helper class
 */
public class UTKit {

    private static final Logger LOGGER = LoggerFactory.getLogger(UTKit.class);
    public static final String EOL = "\n";
    public static final String JOINER = ":";
    public static final String EMPTY = "";
    public static final String BLANK = " ";
    public static final String FREE_FLOW_HEADER = "Freeflow";
    public static final String DOT = ".";

    private UTKit() {
        // Empty private constructor
    }

    public static int elapsedMinutes(Date date) {
        Date currentTime = new Date();
        return (int) ((currentTime.getTime() - date.getTime()) / (60 * 1000));
    }

    public static String getNewBackwardInput(String input) {
        String[] splitStrings = input.split(JOINER);
        StringBuilder res = new StringBuilder();
        String prefix = EMPTY;
        for (int i = 0; i < splitStrings.length - 1; i++) {
            res.append(prefix);
            res.append(splitStrings[i]);
            prefix = JOINER;
        }
        return res.toString();
    }

    public static String getLastInput(String input) {
        String[] lastInputs = input.split(JOINER);
        return lastInputs[lastInputs.length - 1];
    }

    public static String getLastInput(String input, int limit) {
        String[] lastInputs = input.split(JOINER);
        return lastInputs[lastInputs.length - limit];
    }

    public static String listMenus(List<UssdMenu> menus) {
        StringBuilder menuString = new StringBuilder();
        if (menus.size() == 1 && menus.get(0).getQuestionType() != QuestionType.LIST) {
            menuString.append(menus.get(0).getTitleKin());
        } else {
            for (int i = 0; i < menus.size(); i++) {
                menuString.append(i + 1);
                menuString.append(BLANK);
                menuString.append(menus.get(i).getTitleKin());
                menuString.append(EOL);
            }
        }
        return menuString.toString();
    }

    public static String listEnums(String header, Question question) {
        StringBuilder ussdMessage = new StringBuilder();
        switch (question) {
            case REGISTRATION_SELECT_GENDER:
                ussdMessage.append(header);
                ussdMessage.append(UTKit.EOL);
                ussdMessage.append(EnumFormatter.format(Gender.class));
                break;
            case ACTIVITY_SHOW_CATEGORY:
                ussdMessage.append(header);
                break;
            case ACTIVITY_SHOW_LIST:
                ussdMessage.append(header);
                ussdMessage.append(UTKit.EOL);
                ussdMessage.append("Activity list is missing");
                break;
            case MARKETPLACE_LIST_PRODUCT:
                ussdMessage.append(header);
                ussdMessage.append(UTKit.EOL);
                ussdMessage.append("Marketplace product list is missing");
                break;
            case AIRTIME_BUYER_MSISDN:
                ussdMessage.append(header);
                ussdMessage.append(UTKit.EOL);
                ussdMessage.append(EnumFormatter.format(BuyerType.class));
                break;
                default:
                    break;
        }
        return ussdMessage.toString();
    }

    public static Date setExpiryDate(Date date, int days) {
        GregorianCalendar cal = new GregorianCalendar();
        cal.setTime(date);
        cal.add(Calendar.DATE, days);
        return cal.getTime();
    }

    public static String getLocationCode(String lastInput, String locationType) {
        String[] lastInputs = lastInput.split(JOINER);
        int limit = 0;
        StringBuilder locationCode = new StringBuilder();
        if (locationType.equals("province")) {
            limit = 1;
        } else if (locationType.equals("district")) {
            limit = 2;
        } else if (locationType.equals("sector")) {
            limit = 3;
        } else if (locationType.equals("cell")) {
            limit = 4;
        } else if (locationType.equals("village")) {
            limit = 5;
        }
        for (int i = lastInputs.length - limit; i < lastInputs.length; i++) {
            LOGGER.info("locationType {} lowerLimit  {}", locationType, i);
            locationCode.append((String.valueOf(lastInputs[i]).length() > 1 ? String.valueOf(lastInputs[i]) : "0" + lastInputs[i]));
        }

        return locationCode.toString();
    }

    public static boolean validateFullName(String fullName) {
        Pattern p = Pattern.compile("^[ A-Za-z]+$");
        Matcher m = p.matcher(fullName);
        return m.matches();
    }

    public static boolean validateAge(String age, int minAge) {
        Pattern p = Pattern.compile("^[0-9]+$");
        Matcher m = p.matcher(age);
        return (m.matches() && (Integer.parseInt(age) >= minAge));
    }

    public static boolean isExpired(Date date) {
        Date today = new Date();
        return (today.getTime() - date.getTime()) >= 5 * 60 * 1000;
    }

    public static Boolean validateAssociationCode(String associationCode) {
        return !associationCode.isEmpty();
    }

    public static String securePassword(String value) {
        return BCrypt.hashpw(value, "$2a$10$5UhI215Wx3NDc.zc9Qb2ge");
    }
}
