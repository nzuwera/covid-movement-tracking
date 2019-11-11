package com.goltd.agrigoussd.helpers;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

/**
 * USSD Tool Kit helper class
 */
public class UTKit {
    public static final String EOL = "\n";
    public static final String JOINER = ":";
    public static final String EMPTY = "";
    public static final String BLANK = " ";
    public static final String FREE_FLOW_HEADER = "Freeflow";
    public static final String PIPE = "|";

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

    public static Map<String, String> getLocationCodes(String villageCode) {
        Map<String, String> locationCodes = new HashMap<>();
        String provinceCode = villageCode.substring(0, 2);
        String districtCode = villageCode.substring(0, 4);
        String sectorCode = villageCode.substring(0, 6);
        String cellCode = villageCode.substring(0, 8);
        locationCodes.put("provinceCode", provinceCode);
        locationCodes.put("districtCode", districtCode);
        locationCodes.put("sectorCode", sectorCode);
        locationCodes.put("cellCode", cellCode);
        locationCodes.put("villageCode", villageCode);
        return locationCodes;
    }

}
