package rw.centrika.ussd.domain;

import org.json.simple.JSONObject;

public class BusStop {

    private String name;

    public BusStop() {
    }


    public BusStop(String name) {
        this.name = name;
    }


    public BusStop(JSONObject name) {
        this.name = name.get("Name").toString();
    }


    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return "BusStop{" +
                "name='" + name + '\'' +
                '}';
    }
}
