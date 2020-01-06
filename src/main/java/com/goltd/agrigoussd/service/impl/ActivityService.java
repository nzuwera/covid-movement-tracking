package com.goltd.agrigoussd.service.impl;

import com.goltd.agrigoussd.helpers.enums.ActivityCategory;
import com.goltd.agrigoussd.helpers.enums.CropActivities;
import com.goltd.agrigoussd.helpers.enums.HarvestActivities;
import com.goltd.agrigoussd.helpers.enums.LandActivities;
import com.goltd.agrigoussd.helpers.formatter.EnumFormatter;
import com.goltd.agrigoussd.validators.QuestionValidator;
import org.springframework.stereotype.Service;

@Service
public class ActivityService {

    public String showCategories() {
        return EnumFormatter.format(ActivityCategory.class);
    }

    public String showActivity(String input) {

        String activities = "";
        if (input.equals("1")) {
            activities = EnumFormatter.format(LandActivities.class);
        } else if (input.equals("2")) {
            activities = EnumFormatter.format(CropActivities.class);

        } else if (input.equals("3")) {
            activities = EnumFormatter.format(HarvestActivities.class);
        }
        return activities;
    }

    public Boolean isValideCategory(String input) {
        return QuestionValidator.validateEnum(input, ActivityCategory.class);
    }

    public Boolean isValidaActivity(String categoryChoice, String input) {
        if (isValideCategory(categoryChoice)) {
            switch (categoryChoice) {
                case "1":
                    return QuestionValidator.validateEnum(input, LandActivities.class);
                case "2":
                    return QuestionValidator.validateEnum(input, CropActivities.class);
                case "3":
                    return QuestionValidator.validateEnum(input, HarvestActivities.class);
                default:
                    return false;
            }
        }
        return false;
    }
}