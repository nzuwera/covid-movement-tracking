package com.goltd.agrigoussd.helpers;

import com.goltd.agrigoussd.domain.UssdMenu;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Date;
import java.util.List;

/**
 * USSD Tool Kit helper class
 */
public class UTKit {

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

}
