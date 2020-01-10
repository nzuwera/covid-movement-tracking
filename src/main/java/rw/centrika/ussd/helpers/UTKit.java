package rw.centrika.ussd.helpers;

import org.mindrot.jbcrypt.BCrypt;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import rw.centrika.ussd.domain.Language;
import rw.centrika.ussd.domain.UssdMenu;
import rw.centrika.ussd.helpers.enums.QuestionType;

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

    public static Boolean validateAssociationCode(String associationCode) {
        return !associationCode.isEmpty();
    }
}
