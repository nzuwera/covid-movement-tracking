package com.goltd.agrigoussd.service.impl;

import com.goltd.agrigoussd.helpers.UTKit;
import com.goltd.agrigoussd.validators.QuestionValidator;
import org.springframework.stereotype.Service;

@Service
public class LandService {
    private String[] lands = {"Land One", "Land Two", "Land Three"};

    private String[] crops = {"Crop 1", "Crop 2", "Crop 3"};

    private String[] cropTypes = {"Crop 1", "Crop 2", "Crop 3"};

    public String[] getLands() {
        return lands;
    }

    public String[] getCrops() {
        return crops;
    }

    public String[] getCropTypes() {
        return cropTypes;
    }

    public String formatStringList(String[] strings) {
        StringBuilder landList = new StringBuilder();
        for (int i = 0; i < strings.length; i++) {
            landList.append(i + 1);
            landList.append(". ");
            landList.append(strings[i]);
            landList.append(UTKit.EOL);
        }
        return landList.toString();
    }

    public String registerLand(String code) {
        return "Land " + code;
    }

    public String removeLand(String choice) {
        String message = "You removed ";
        if (Integer.parseInt(choice) <= lands.length) {
            message += lands[Integer.parseInt(choice) - 1];
        } else {
            message = "";
        }
        return message;
    }

    public Boolean isValideLand(String choice, String[] lands) {
        if (QuestionValidator.validateNumericalString(choice)) {
            return lands.length >= Integer.parseInt(choice) && Integer.parseInt(choice) > 0;
        }
        return false;
    }
}
