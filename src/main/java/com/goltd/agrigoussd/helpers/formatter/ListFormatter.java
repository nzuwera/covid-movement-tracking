package com.goltd.agrigoussd.helpers.formatter;

import com.goltd.agrigoussd.domain.Location;
import com.goltd.agrigoussd.domain.UssdMenu;
import com.goltd.agrigoussd.helpers.UTKit;

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

    public static StringBuilder formatLocations(List<Location> listObject) {
        StringBuilder listMessage = new StringBuilder();
        for (int i = 0; i < listObject.size(); i++) {
            listMessage.append(i + 1);
            listMessage.append(". ");
            listMessage.append(listObject.get(i).getName());
            listMessage.append(UTKit.EOL);
        }
        return listMessage;
    }
}
