package com.goltd.agrigoussd.helpers.formatter;

import com.goltd.agrigoussd.helpers.UTKit;

import java.util.EnumSet;

public class EnumFormatter<E extends Enum<E>> {

    private EnumFormatter() {
        //
    }

    public static <E extends Enum<E>> String format(String header, Class<E> enumObject) {
        StringBuilder message = new StringBuilder();
        message.append(header);
        message.append(UTKit.EOL);
        int i = 1;
        for (E enumValue : EnumSet.allOf(enumObject)) {
            message.append(i);
            message.append(". ");
            message.append(enumValue.toString());
            message.append(UTKit.EOL);
            i++;
        }
        return message.toString();
    }
}
