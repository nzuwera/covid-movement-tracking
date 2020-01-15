package rw.centrika.ussd.helpers;

import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import rw.centrika.ussd.domain.BusTime;
import rw.centrika.ussd.domain.Language;
import rw.centrika.ussd.domain.UssdMenu;
import rw.centrika.ussd.helpers.enums.QuestionType;

import java.text.SimpleDateFormat;
import java.util.*;

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
    private static final String CENTRIKA_CARD = "^CENT\\d{12}$"; // CENT190701000753
    private static final String NUMBERS = "^[0-9]+$";

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

    public static String listMenus(Language language, List<UssdMenu> menus) {
        StringBuilder menuString = new StringBuilder();
        if (menus.size() == 1 && menus.get(0).getQuestionType() != QuestionType.LIST) {
            menuString.append(language.equals(Language.KIN) ? menus.get(0).getTitleKin() : menus.get(0).getTitleEng());
        } else {
            for (int i = 0; i < menus.size(); i++) {
                menuString.append(i + 1);
                menuString.append(BLANK);
                menuString.append(language.equals(Language.KIN) ? menus.get(i).getTitleKin() : menus.get(i).getTitleEng());
                menuString.append(EOL);
            }
        }
        return menuString.toString();
    }

    public static String setTitle(Language language, UssdMenu menu) {
        return language.equals(Language.KIN) ? menu.getTitleKin() : menu.getTitleEng();
    }

    public static boolean isExpired(Date date) {
        Date today = new Date();
        return (today.getTime() - date.getTime()) >= 5 * 60 * 1000;
    }

    public static JSONObject createJSONObject(String jsonString) {
        JSONObject jsonObject = new JSONObject();
        JSONParser jsonParser = new JSONParser();
        if ((jsonString != null) && !(jsonString.isEmpty())) {
            try {
                jsonObject = (JSONObject) jsonParser.parse(jsonString);
            } catch (org.json.simple.parser.ParseException e) {
                LOGGER.error(e.getMessage());
            }
        }
        return jsonObject;
    }

    public static String replaceLastInput(String lastInput, String replacingInput) {
        String[] sessionInputs = lastInput.split(JOINER);
        String[] sessionInputsReduced = Arrays.copyOf(sessionInputs, sessionInputs.length - 1);
        return String.join(JOINER, sessionInputsReduced) + JOINER + replacingInput;
    }

    public static String getBusTime() {
        List<BusTime> busTimes = getGetDepartureTime();
        StringBuilder timeString = new StringBuilder();
        for (int i = 0; i < busTimes.size(); i++) {
            timeString.append(i + 1);
            timeString.append(DOT);
            timeString.append(BLANK);
            timeString.append(busTimes.get(i).getStartDate());
            timeString.append(BLANK);
            timeString.append(busTimes.get(i).getStartTime());
            timeString.append(EOL);
        }
        return timeString.toString();
    }

    public static List<BusTime> getGetDepartureTime() {
        List<BusTime> listDepartureTime = new ArrayList<>();
        String dateFormat = "yyyy-MM-dd hh'H'mm";
        SimpleDateFormat df = new SimpleDateFormat(dateFormat);
        Calendar start = Calendar.getInstance();
        start.add(Calendar.HOUR, 1);
        start.set(Calendar.SECOND, 0);
        start.set(Calendar.MILLISECOND, 0);
        for (int i = 0; i < 4; i++) {
            int modulo = start.get(Calendar.MINUTE) % 30;
            if (modulo > 0) {
                start.add(Calendar.MINUTE, -modulo);
            } else {
                start.add(Calendar.MINUTE, 30);
            }
            String[] busTimes = df.format(start.getTime()).split(BLANK);
            listDepartureTime.add(new BusTime(busTimes[0], busTimes[1]));
        }
        return listDepartureTime;
    }

    public static boolean validateSafariBusCardForm(String upiNumber) {
        return upiNumber.toUpperCase().matches(CENTRIKA_CARD);
    }

    public static Boolean validateNumericalString(String number) {
        return number.matches(NUMBERS);
    }
}
