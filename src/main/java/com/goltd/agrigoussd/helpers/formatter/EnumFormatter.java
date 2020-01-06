package com.goltd.agrigoussd.helpers.formatter;

import com.goltd.agrigoussd.helpers.UTKit;

import java.util.EnumSet;

public class EnumFormatter<E extends Enum<E>> {

    private EnumFormatter() {
        //
    }

    public static <E extends Enum<E>> String format(Class<E> enumObject) {
        StringBuilder message = new StringBuilder();
        int i = 1;
        for (E enumValue : EnumSet.allOf(enumObject)) {
            message.append(i);
            message.append(UTKit.DOT + UTKit.BLANK);
            message.append(enumValue.toString());
            message.append(UTKit.EOL);
            i++;
        }
        return message.toString();
    }
}
