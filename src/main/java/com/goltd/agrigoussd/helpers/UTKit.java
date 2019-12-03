package com.goltd.agrigoussd.helpers;

import com.goltd.agrigoussd.domain.UserAccount;
import com.goltd.agrigoussd.domain.UssdMenu;
import com.goltd.agrigoussd.helpers.enums.AccountState;
import com.goltd.agrigoussd.helpers.enums.Gender;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Date;
import java.util.List;
import java.util.UUID;
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
        for (int i = 0; i < menus.size(); i++) {
            menuString.append(i + 1);
            menuString.append(BLANK);
            menuString.append(menus.get(i).getTitleKin());
            menuString.append(EOL);
        }
        return menuString.toString();
    }

    public static Date setExpiryDate(Date date, int days) {
        LocalDateTime expiryDate = date.toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime();
        expiryDate = expiryDate.plusDays(days);
        return Date.from(expiryDate.atZone(ZoneId.systemDefault()).toInstant());
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

    public static UserAccount getUserDetailsFromLastInput(String msisdn, String lastInput) {
        UserAccount userAccount = new UserAccount();
        String[] userDetails = lastInput.split(JOINER);
        Gender gender = (userDetails[2].equals("1") ? Gender.MALE : Gender.FEMALE);
        String province = "0" + userDetails[3];
        String district = (userDetails[4].length() > 1 ? userDetails[4] : "0" + userDetails[4]);
        String sector = (userDetails[5].length() > 1 ? userDetails[5] : "0" + userDetails[5]);
        String cell = (userDetails[6].length() > 1 ? userDetails[6] : "0" + userDetails[6]);
        String village = (userDetails[7].length() > 1 ? userDetails[7] : "0" + userDetails[7]);
        String villageCode = province + district + sector + cell + village;

        userAccount.setId(UUID.randomUUID());
        userAccount.setFullname(userDetails[1]);
        userAccount.setAge(Integer.parseInt(userDetails[2]));
        userAccount.setGender(gender);
        userAccount.setAccountState(AccountState.PENDING_SUBSCRIPTION);
        userAccount.setVillageCode(villageCode);
        userAccount.setMsisdn(msisdn);
        userAccount.setExpireDate(setExpiryDate(new Date(), 7));
        userAccount.setPin(userDetails[8]);
        return userAccount;
    }

    public static boolean validateFullName(String fullName) {
        Pattern p = Pattern.compile("^[ A-Za-z]+$");
        Matcher m = p.matcher(fullName);
        return m.matches();
    }

    public static boolean validateAge(String age) {
        Pattern p = Pattern.compile("^[0-9]+$");
        Matcher m = p.matcher(age);
        return (m.matches() && (Integer.parseInt(age) >= 18));
    }

}
