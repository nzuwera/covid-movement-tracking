package rw.centrika.ussd.helpers.enums;

public enum  BuyerType {
    SELF("Njyewe"),
    OTHER("Abandi");

    public final String text;

    BuyerType(String text) {
        this.text = text;
    }
}
