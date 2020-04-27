package com.nzuwera.ussd.covidtracking.helpers;

public enum Message {
    NOT_FOUND("%s not found"),
    ERROR("Error occured : %s"),
    ALREADY_EXISTS("%s already exists"),
    CONTENT_TYPE("Content-Type"),
    AUTHORIZATION("Authorization"),
    CONFIRMATION_MESSAGE("Aho uvuye %s\nAho ugiye %s\nUmugenzi %s\n\n1. Emeza");

    public final String string;

    Message(String string) {
        this.string = string;
    }
}
