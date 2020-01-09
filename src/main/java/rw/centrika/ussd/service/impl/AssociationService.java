package rw.centrika.ussd.service.impl;

import org.springframework.stereotype.Service;
import rw.centrika.ussd.helpers.UTKit;

@Service
public class AssociationService {
    private String[] associations = {"Association One", "Association Two", "Association Three"};

    public String[] getAssociations() {
        return associations;
    }

    public String showAssociation() {
        StringBuilder associationList = new StringBuilder();
        for (int i = 0; i < associations.length; i++) {
            associationList.append(i + 1);
            associationList.append(UTKit.BLANK);
            associationList.append(associations[i]);
            associationList.append(UTKit.EOL);
        }
        return associationList.toString();
    }

    public String joinAssociation(String code) {
        return "Association " + code;
    }

    public String deleteAssociation(String choice) {
        String message = "You left ";
        if (Integer.parseInt(choice) <= associations.length) {
            message += associations[Integer.parseInt(choice) - 1];
        } else {
            message = "";
        }
        return message;
    }
}
