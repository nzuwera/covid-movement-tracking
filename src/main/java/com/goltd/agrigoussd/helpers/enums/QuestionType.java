package com.goltd.agrigoussd.helpers.enums;


public enum QuestionType {
    ROOT,
    OPTIONS,
    LIST, // list of dependent children menus
    FORM_INPUT, // Single question
    REMOTE_API, // Get menu list from remote api call
    DYNAMIC_LIST, // Get menu list other tables
    ENUM, // hard coded menu from enums
    MESSAGE // Informative text eg: confirmation or summary info
}
