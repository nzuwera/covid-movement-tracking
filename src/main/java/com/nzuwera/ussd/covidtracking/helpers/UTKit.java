package com.nzuwera.ussd.covidtracking.helpers;

import java.util.Date;

/**
 * USSD Tool Kit helper class
 */
public class UTKit {

    public static final String EOL = "\n";
    public static final String JOINER = ":";
    public static final String EMPTY = "";
    public static final String BLANK = " ";
    public static final String UNDERSCORE = "_";
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


    public static boolean isExpired(Date date) {
        Date today = new Date();
        return (today.getTime() - date.getTime()) >= 5 * 60 * 1000;
    }


}
