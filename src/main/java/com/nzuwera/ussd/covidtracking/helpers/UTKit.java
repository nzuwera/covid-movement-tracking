package com.nzuwera.ussd.covidtracking.helpers;

import com.nzuwera.ussd.covidtracking.domain.BusTime;
import com.nzuwera.ussd.covidtracking.domain.Language;
import com.nzuwera.ussd.covidtracking.domain.UssdMenu;
import com.nzuwera.ussd.covidtracking.helpers.enums.QuestionType;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
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
    public static final String UNDERSCORE = "_";
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


    public static List<BusTime> getDepartureTime(String departureDate, String timeOfTheDay) {

        String startDate = EMPTY;
        if (departureDate.equals("1")) {
            startDate = LocalDate.now().format(DateTimeFormatter.ISO_DATE);
        } else if (departureDate.equals("2")) {
            startDate = LocalDate.now().plusDays(1).format(DateTimeFormatter.ISO_DATE);
        }

        Map<String, String[]> hoursInDay = new HashMap<>();
        String[] night = {"00H00", "01H00", "02H00", "03H00"};
        String[] earlyMorning = {"04H00", "05H00", "06H00", "07H00"};
        String[] morning = {"08H00", "09H00", "10H00", "11H00"};
        String[] afternoon = {"12H00", "13H00", "14H00", "15H00"};
        String[] evening = {"16H00", "17H00", "18H00", "19H00"};
        String[] earlyEvening = {"20H00", "21H00", "22H00", "23H00"};
        hoursInDay.put("1", earlyMorning);
        hoursInDay.put("2", morning);
        hoursInDay.put("3", afternoon);
        hoursInDay.put("4", evening);
        hoursInDay.put("5", earlyEvening);
        hoursInDay.put("6", night);
        List<BusTime> busTimeList = new ArrayList<>();
        String[] selectedTimes = hoursInDay.get(timeOfTheDay);
        for (String startTime : selectedTimes) {
            busTimeList.add(new BusTime(startDate, startTime));
        }
        return busTimeList;
    }

    public static String showDepartureTime(String departureDate, String timeOfTheDay){
        List<BusTime> departureTimes = UTKit.getDepartureTime(departureDate,timeOfTheDay);
        StringBuilder departureDateTime = new StringBuilder();
        for (int i = 0; i < departureTimes.size(); i++) {
            departureDateTime.append(i + 1);
            departureDateTime.append(UTKit.DOT);
            departureDateTime.append(UTKit.BLANK);
            departureDateTime.append(departureTimes.get(i).getStartDate());
            departureDateTime.append(UTKit.BLANK);
            departureDateTime.append(departureTimes.get(i).getStartTime());
            departureDateTime.append(UTKit.EOL);
        }
        return departureDateTime.toString();
    }

    public static boolean validateSafariBusCardForm(String upiNumber) {
        return upiNumber.toUpperCase().matches(CENTRIKA_CARD);
    }

    public static Boolean validateNumericalString(String number) {
        return number.matches(NUMBERS);
    }

    public static String getTimeOfTheDay(Language language) {
        String timeOfTheDay;
        if (language == Language.ENG) {
            timeOfTheDay = "1. Early Morning (4AM-8AM)" + EOL +
                    "2. Morning(8AM-12PM)" + EOL +
                    "3. Afternoon(12PM-4PM)" + EOL +
                    "4. Evening(4PM-8PM)" + EOL +
                    "5. Early Evening(8PM-12AM)" + EOL +
                    "6. Night(12AM-4AM)";
        } else {
            timeOfTheDay = "1. Kare mu gitondo (4AM-8AM)" + EOL +
                    "2. Mu gitondo (8AM-12PM)" + EOL +
                    "3. Nyuma ya saa sita(12PM-4PM)" + EOL +
                    "4. Ku mugoroba(4PM-8PM)" + EOL +
                    "5. Ni joro(8PM-12AM)" + EOL +
                    "6. Mu gicuku(12AM-4AM)";
        }
        return timeOfTheDay;
    }


    public static String getDepartureDate(Language language) {
        String timeOfTheDay;
        if (language == Language.ENG) {
            timeOfTheDay = "1. Today" + EOL +
                    "2. Tomorrow";
        } else {
            timeOfTheDay = "1. Uyu munsi" + EOL +
                    "2. Ejo";
        }
        return timeOfTheDay;
    }

    public static Boolean validateTimeOfTheDay(String input) {
        return Integer.parseInt(input) <= 6 && Integer.parseInt(input) > 0;
    }

    public static Boolean validateDepartureDate(String input) {
        return Integer.parseInt(input) == 2 || Integer.parseInt(input) == 1;
    }
}
