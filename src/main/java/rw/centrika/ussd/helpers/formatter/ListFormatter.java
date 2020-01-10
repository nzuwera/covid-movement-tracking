package rw.centrika.ussd.helpers.formatter;

import rw.centrika.ussd.domain.Language;
import rw.centrika.ussd.domain.UssdMenu;
import rw.centrika.ussd.helpers.UTKit;
import rw.centrika.ussd.helpers.enums.QuestionType;

import java.util.List;

public class ListFormatter {

    private ListFormatter() {
        //
    }


    public static StringBuilder formatListMenus(String header, List<UssdMenu> listObject) {
        StringBuilder listMessage = new StringBuilder();
        if (!header.trim().equals("") && header.trim().length() > 0) {
            listMessage.append(header);
            listMessage.append(UTKit.EOL);
        }
        for (int i = 0; i < listObject.size(); i++) {
            listMessage.append(i + 1);
            listMessage.append(". ");
            listMessage.append(listObject.get(i).getTitleKin());
            listMessage.append(UTKit.EOL);
        }
        return listMessage;
    }

    public static StringBuilder formatListMenus(Language language, List<UssdMenu> listObject) {
        StringBuilder listMessage = new StringBuilder();
        if (listObject.get(0).getQuestionType() == QuestionType.LIST) {
            for (int i = 0; i < listObject.size(); i++) {
                listMessage.append(i + 1);
                listMessage.append(". ");
                listMessage.append(language.equals(Language.KIN) ? listObject.get(i).getTitleKin() : listObject.get(i).getTitleEng());
                listMessage.append(UTKit.EOL);
            }
        } else {
            listMessage.append(language.equals(Language.KIN) ? listObject.get(0).getTitleKin() : listObject.get(0).getTitleEng());
        }
        return listMessage;
    }
}
