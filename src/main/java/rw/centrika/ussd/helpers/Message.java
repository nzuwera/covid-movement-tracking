package rw.centrika.ussd.helpers;

public enum Message {
    NOT_FOUND("%s not found"),
    ERROR("Error occured : %s"),
    ALREADY_EXISTS("%s already exists"),
    CONTENT_TYPE("Content-Type"),
    AUTHORIZATION("Authorization");

    public final String string;

    Message(String string) {
        this.string = string;
    }
}
